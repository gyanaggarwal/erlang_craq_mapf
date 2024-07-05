-module(craq_server_app).

-behavior(application).

-export([start/2, stop/1]).

-spec start(StartType :: atom(), StartArgs :: term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    craq_process_sup:start_link({craq_server_sup, craq_server, [craq_server_process, craq_node_process]}).

-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.
