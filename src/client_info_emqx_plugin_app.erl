-module(client_info_emqx_plugin_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([
    start/2,
    stop/1
]).

-export([
    on_config_changed/2,
    on_health_check/1
]).

%% NOTE
%% Functions from EMQX are unavailable at compile time.
-dialyzer({no_unknown, [start/2, stop/1]}).

start(_StartType, _StartArgs) ->
    {ok, Sup} = client_info_emqx_plugin_sup:start_link(),
    client_info_emqx_plugin:hook(),
    emqx_ctl:register_command(client_info_emqx_plugin, {client_info_emqx_plugin_cli, cmd}),
    {ok, Sup}.

stop(_State) ->
    emqx_ctl:unregister_command(client_info_emqx_plugin),
    client_info_emqx_plugin:unhook().

on_config_changed(OldConfig, NewConfig) ->
    client_info_emqx_plugin:on_config_changed(OldConfig, NewConfig).

on_health_check(Options) ->
    client_info_emqx_plugin:on_health_check(Options).
