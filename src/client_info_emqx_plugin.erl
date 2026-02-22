-module(client_info_emqx_plugin).

-define(PLUGIN_NAME, "client_info_emqx_plugin").
-define(PLUGIN_VSN, "1.0.0").

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
    ManagedTopics = maps:get(<<"managed_topics">>, Config, []),
    ?SLOG(debug, #{
        msg => "client_info_emqx_plugin_on_client_connect",
        conninfo => ConnInfo,
        managed_topics => ManagedTopics
    }),
    {ok, Props}.

%% @doc
%% Called when a message is published.
%% If the topic matches any configured managed topic,
%% prepend the following header to the payload:
%%   [0x01, 0x35, 0x83, 0x08] ++ [4-byte big-endian total-appended-length] ++ peerhost ++ clientid
%%
%% The 4-byte length field encodes the total length of the appended prefix,
%% i.e., 4 (magic) + 4 (length field itself) + byte_size(peerhost) + byte_size(clientid).
on_message_publish(#message{topic = Topic, payload = Payload, headers = Headers, from = From} = Msg) ->
    Config = get_config(),
    %% managed_topics is a plain list of binary strings: [<<"/client/#">>, ...]
    ManagedTopics = maps:get(<<"managed_topics">>, Config, []),
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
    PeerHostSize = byte_size(PeerHost),
    ClientIdSize = byte_size(ClientId),
    TotalLen = PeerHostSize + ClientIdSize,
    <<?MAGIC_HEADER/binary, TotalLen:32/big-unsigned-integer, PeerHost/binary, ClientId/binary, Payload/binary>>.

%%--------------------------------------------------------------------
%% Plugin callbacks
%%--------------------------------------------------------------------

on_health_check(_Options) ->
    case get_config() of
        #{<<"managed_topics">> := _} -> ok;
        _ -> {error, <<"Invalid config, missing managed_topics">>}
    end.

on_config_changed(_OldConfig, NewConfig) ->
    ok = gen_server:cast(?MODULE, {on_changed, NewConfig}).

%%--------------------------------------------------------------------
%% Working with config
%%--------------------------------------------------------------------

get_config() ->
    persistent_term:get(?MODULE, #{}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:process_flag(trap_exit, true),
    PluginNameVsn = <<?PLUGIN_NAME, "-", ?PLUGIN_VSN>>,
    Config = emqx_plugin_helper:get_config(PluginNameVsn),
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
