-module(client_info_emqx_plugin).

-define(PLUGIN_NAME, "client_info_emqx_plugin").
-define(PLUGIN_VSN, "1.1.1").
-define(CONFIG_REWRITE_DELAY_MS, 10).

%% Magic header bytes: 0x01, 0x35, 0x83, 0x08
-define(MAGIC_HEADER, <<16#01, 16#35, 16#83, 16#08>>).

-include_lib("emqx_plugin_helper/include/emqx.hrl").
-include_lib("emqx_plugin_helper/include/emqx_hooks.hrl").
-include_lib("emqx_plugin_helper/include/logger.hrl").

-export([
    hook/0,
    unhook/0,
    start_link/0
]).

-export([
    on_config_changed/2,
    on_health_check/1,
    get_config/0
]).

-export([
    on_client_connect/3,
    on_message_publish/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-dialyzer({no_unknown, [hook/0, unhook/0, on_message_publish/1]}).

hook() ->
    emqx_hooks:add('client.connect', {?MODULE, on_client_connect, []}, ?HP_HIGHEST),
    emqx_hooks:add('message.publish', {?MODULE, on_message_publish, []}, ?HP_HIGHEST).

unhook() ->
    emqx_hooks:del('client.connect', {?MODULE, on_client_connect}),
    emqx_hooks:del('message.publish', {?MODULE, on_message_publish}).

%%--------------------------------------------------------------------
%% Hook callbacks
%%--------------------------------------------------------------------

on_client_connect(ConnInfo, Props, []) ->
    Config = get_config(),
    TopicItems = maps:get(<<"topics">>, Config, []),
    EnabledTopics = [maps:get(<<"topic">>, Item) || Item <- TopicItems, is_enabled(Item)],
    ?SLOG(debug, #{
        msg => "client_info_emqx_plugin_on_client_connect",
        conninfo => ConnInfo,
        enabled_topics => EnabledTopics
    }),
    {ok, Props}.

on_message_publish(#message{topic = Topic, payload = Payload, headers = Headers, from = From} = Msg) ->
    Config = get_config(),
    TopicItems = maps:get(<<"topics">>, Config, []),
    ManagedTopics = [maps:get(<<"topic">>, Item) || Item <- TopicItems, is_enabled(Item)],
    case is_managed_topic(Topic, ManagedTopics) of
        true ->
            PeerHost = format_peerhost(maps:get(peerhost, Headers, undefined)),
            ClientId = format_clientid(From),
            NewPayload = prepend_header(PeerHost, ClientId, Payload),
            ?SLOG(debug, #{
                msg => "client_info_emqx_plugin_on_message_publish",
                topic => Topic,
                peerhost => PeerHost,
                clientid => ClientId
            }),
            {ok, Msg#message{payload = NewPayload}};
        false ->
            ok
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% null / missing => enabled by default, only explicit false => disabled
is_enabled(Item) ->
    enabled_to_bool(maps:get(<<"enabled">>, Item, true)).

%% Check if Topic matches any pattern in the managed topics list (supports MQTT wildcards)
is_managed_topic(_Topic, []) ->
    false;
is_managed_topic(Topic, [Pattern | Rest]) ->
    case emqx_topic:match(Topic, Pattern) of
        true -> true;
        false -> is_managed_topic(Topic, Rest)
    end.

%% Encode peerhost: IPv4 as 4 bytes, IPv6 as 16 bytes, otherwise raw binary
format_peerhost({A, B, C, D}) ->
    <<A, B, C, D>>;
format_peerhost({A, B, C, D, E, F, G, H}) ->
    <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>;
format_peerhost(Host) when is_binary(Host) ->
    Host;
format_peerhost(_) ->
    <<>>.

%% Encode clientid as binary
format_clientid(ClientId) when is_binary(ClientId) ->
    ClientId;
format_clientid(ClientId) when is_atom(ClientId) ->
    atom_to_binary(ClientId, utf8);
format_clientid(_) ->
    <<>>.

%% Build the prepended payload:
%%   Magic(4) + Length(4) + PeerHost(N) + ClientId(M) + OriginalPayload
%% Length = byte_size(PeerHost) + byte_size(ClientId)  (excludes magic and length field itself)
prepend_header(PeerHost, ClientId, Payload) ->
    TotalLen = byte_size(PeerHost) + byte_size(ClientId),
    <<?MAGIC_HEADER/binary, TotalLen:32/big-unsigned-integer, PeerHost/binary, ClientId/binary, Payload/binary>>.

%%--------------------------------------------------------------------
%% Plugin callbacks
%%--------------------------------------------------------------------

on_health_check(_Options) ->
    ok.

on_config_changed(_OldConfig, NewConfig) ->
    NormalizedConfig = normalize_config(NewConfig),
    maybe_schedule_config_rewrite(NewConfig, NormalizedConfig),
    ok = gen_server:cast(?MODULE, {on_changed, NormalizedConfig}).

%%--------------------------------------------------------------------
%% Working with config
%%--------------------------------------------------------------------

get_config() ->
    persistent_term:get(?MODULE, #{<<"topics">> => []}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:process_flag(trap_exit, true),
    PluginNameVsn = <<?PLUGIN_NAME, "-", ?PLUGIN_VSN>>,
    Config = normalize_config(emqx_plugin_helper:get_config(PluginNameVsn)),
    ?SLOG(debug, #{
        msg => "client_info_emqx_plugin_init",
        config => Config
    }),
    persistent_term:put(?MODULE, Config),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({on_changed, Config}, State) ->
    persistent_term:put(?MODULE, Config),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    persistent_term:erase(?MODULE),
    ok.

normalize_config(Config) when is_map(Config) ->
    TopicItems = maps:get(<<"topics">>, Config, []),
    NormalizedTopics = [normalize_topic_item(Item) || Item <- TopicItems],
    Config#{<<"topics">> => NormalizedTopics};
normalize_config(_Config) ->
    #{<<"topics">> => []}.

normalize_topic_item(Item) when is_map(Item) ->
    Enabled = enabled_to_bool(maps:get(<<"enabled">>, Item, undefined)),
    %% Keep Avro union form for Dashboard rendering compatibility
    Item#{<<"enabled">> => #{<<"boolean">> => Enabled}};
normalize_topic_item(Item) ->
    ?SLOG(warning, #{
        msg => "client_info_emqx_plugin_normalize_topic_item_malformed",
        item => Item
    }),
    #{<<"enabled">> => #{<<"boolean">> => true}}.

enabled_to_bool(Value0) ->
    Value = unwrap_enabled_value(Value0),
    case Value of
        false -> false;
        <<"false">> -> false;
        "false" -> false;
        true -> true;
        null -> true;
        undefined -> true;
        Other ->
            ?SLOG(warning, #{
                msg => "client_info_emqx_plugin_enabled_unexpected_value",
                value => Other
            }),
            true
    end.

%% Dashboard may serialize Avro union fields as wrapped maps, e.g.
%% #{<<"boolean">> => true} / #{<<"null">> => null}.
%% Unwrap first so downstream logic can stay on plain values.
unwrap_enabled_value(#{<<"boolean">> := Bool}) ->
    Bool;
unwrap_enabled_value(#{boolean := Bool}) ->
    Bool;
unwrap_enabled_value(#{<<"null">> := null}) ->
    null;
unwrap_enabled_value(#{null := null}) ->
    null;
unwrap_enabled_value(Value) ->
    Value.

maybe_schedule_config_rewrite(Config, Config) ->
    ok;
maybe_schedule_config_rewrite(_Config, NormalizedConfig) ->
    PluginNameVsn = <<?PLUGIN_NAME, "-", ?PLUGIN_VSN>>,
    _ = spawn(fun() ->
        timer:sleep(?CONFIG_REWRITE_DELAY_MS),
        case catch emqx_plugins:update_config(PluginNameVsn, NormalizedConfig) of
            ok ->
                ?SLOG(info, #{
                    msg => "client_info_emqx_plugin_config_rewritten",
                    plugin => PluginNameVsn
                });
            {error, Reason} ->
                ?SLOG(warning, #{
                    msg => "client_info_emqx_plugin_config_rewrite_failed",
                    plugin => PluginNameVsn,
                    reason => Reason
                });
            {'EXIT', Reason} ->
                ?SLOG(warning, #{
                    msg => "client_info_emqx_plugin_config_rewrite_exit",
                    plugin => PluginNameVsn,
                    reason => Reason
                });
            Other ->
                ?SLOG(warning, #{
                    msg => "client_info_emqx_plugin_config_rewrite_unexpected",
                    plugin => PluginNameVsn,
                    result => Other
                })
        end
    end),
    ok.
