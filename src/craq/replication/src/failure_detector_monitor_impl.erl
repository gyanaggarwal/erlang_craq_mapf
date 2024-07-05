-module(failure_detector_monitor_impl).

-behavior(failure_detector).

-export([set/3,
         set/1,
         detect/1]).

-include("craq.hrl").

-define(X_PROCESS,        process).

-spec set(Server :: atom(),
	  Node :: atom(),  
	  NodeList :: list()) -> term().
set(Server,
    Node,  
    NodeList) ->
    NewNodeList = lists:delete(Node, NodeList),
    lists:foreach(fun(N) -> set({Server, N}) end, NewNodeList).

-spec set(ServerRef :: term()) -> term().
set(ServerRef) ->
    monitor(?X_PROCESS, ServerRef).

-spec detect(Input :: term()) -> term().
detect({'DOWN', MRef, ?X_PROCESS, {_Server, Node}, _Reason}) ->
    demonitor(MRef, [flush]),
    {?CRAQ_PROCESS_NODEDOWN, Node};
detect(_) ->
    ok.
