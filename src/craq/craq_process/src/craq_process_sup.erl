-module(craq_process_sup).

-behavior(supervisor).

-export([start_link/1]).

-export([init/1]).

-include("craq.hrl").

% {craq_node_sup, craq_node, [craq_node_process]}
% {craq_server_sup, craq_server, [craq_server_process, craq_node_process]}
-spec start_link(Input :: term()) -> {ok, pid()} | term().
start_link({SupName, AppName, ServerList}) ->
    supervisor:start_link({local, SupName}, ?MODULE, [AppName, ServerList]).

-spec init(Arg :: list()) -> {ok, {tuple(), list()}}.
init([AppName, ServerList]) ->
    ConfigMap        = config_map:get_env(AppName),
    RestartIntensity = config_map:get_sup_restart_intensity(ConfigMap),
    RestartPeriod    = config_map:get_sup_restart_period(ConfigMap),
    ChildShutdown    = config_map:get_sup_child_shutdown(ConfigMap),

    Childern        = lists:map(fun(ServerName)->
					{ServerName, {ServerName, start_link, [ConfigMap]},
					 permanent, ChildShutdown, worker, [ServerName]}
				end, ServerList),

    RestartStrategy = {one_for_one, RestartIntensity, RestartPeriod},

    {ok, {RestartStrategy, Childern}}.
