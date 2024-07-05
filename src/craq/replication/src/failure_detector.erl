-module(failure_detector).

-export([set/4,
	 set/2,
	 detect/2]).

-include("craq.hrl").

-callback set(Server :: atom(), Node :: atom(), ReplRing :: list()) -> ok.

-callback set(ServerRef :: term()) -> ok.

-callback detect(Msg :: term()) -> ok | {?CRAQ_PROCESS_NODEDOWN, atom()}.

-spec set(Server :: atom(),
	  Node :: atom(),  
	  ReplRing :: list(), 
	  ConfigMap :: maps:map()) -> ok.
set(Server, 
    Node,  
    ReplRing, 
    ConfigMap) ->
    FailureDetectorMod = config_map:get_failure_detector(ConfigMap),
    FailureDetectorMod:set(Server, Node, ReplRing).

-spec set(ServerRef :: term(), 
	  ConfigMap :: maps:map()) -> ok.
set(ServerRef, 
    ConfigMap) ->
    FailureDetectorMod = config_map:get_failure_detector(ConfigMap),
    FailureDetectorMod:set(ServerRef).

-spec detect(Msg :: term(), 
	     ConfigMap :: maps:map()) -> ok | {?CRAQ_PROCESS_NODEDOWN, atom()}.
detect(Msg, 
       ConfigMap) ->
    FailureDetectorMod = config_map:get_failure_detector(ConfigMap),
    FailureDetectorMod:detect(Msg).