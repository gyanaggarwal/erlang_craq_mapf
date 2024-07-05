-module(craq_node_app).

-behavior(application).

-export([start/2, stop/1]).

-spec start(StartType :: atom(), StartArgs :: term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    craq_process_sup:start_link({craq_node_sup, craq_node, [craq_node_process]}).

-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.
