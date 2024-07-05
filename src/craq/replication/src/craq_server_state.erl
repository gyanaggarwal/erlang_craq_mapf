-module(craq_server_state).

-export([init/1,
	 update_repl_ring/2,
	 handle_msg/2]).

-include("craq.hrl").

-spec init(ConfigMap :: maps:map()) -> #craq_server_state{}.
init(ConfigMap) ->
    State = #craq_server_state{config_map=ConfigMap},
    load_balancer:init(State).

-spec update_repl_ring({MsgTag :: ?CRAQ_ADD_NODE | ?CRAQ_DROP_NODE | ?CRAQ_SET_REPL_RING, Msg :: term()},
		       State :: #craq_server_state{}) -> #craq_server_state{}.
update_repl_ring({?CRAQ_SET_REPL_RING, RingMsg},
		 #craq_server_state{config_map=ConfigMap}=State) ->
    CraqNodeProcess = config_map:get_craq_node_process(ConfigMap),
    {ReplRing, PrecedenceOrderRing} = craq_precedence_order_ring:set_ring(RingMsg),
    failure_detector:set(CraqNodeProcess, ?CRAQ_UNDEFINED, ReplRing, ConfigMap),
    craq_client:abcast(ReplRing, CraqNodeProcess, {?CRAQ_REPL_RING, ReplRing}),
    State#craq_server_state{repl_ring=ReplRing, precedence_order_ring=PrecedenceOrderRing};
update_repl_ring({?CRAQ_DROP_NODE, Msg},
		 #craq_server_state{repl_ring=ReplRing,
				    precedence_order_ring=PrecedenceOrderRing,
				    config_map=ConfigMap}=State) ->
    case failure_detector:detect(Msg, ConfigMap) of
	{?CRAQ_PROCESS_NODEDOWN, DownNode} ->
	    ReplRing1 = replication_ring:drop_node(DownNode, ReplRing),
	    PrecedenceOrderRing1 = craq_precedence_order_ring:drop_node(DownNode, PrecedenceOrderRing),
	    State#craq_server_state{repl_ring=ReplRing1, precedence_order_ring=PrecedenceOrderRing1};
	_ ->
	    State
    end;
update_repl_ring({?CRAQ_ADD_NODE, {?CRAQ_UNDEFINED, _, _}},
		 #craq_server_state{}=State) ->
    State;
update_repl_ring({?CRAQ_ADD_NODE, {NodeRingId, PrecedenceOrder, Node}},
		 #craq_server_state{repl_ring=ReplRing,
				    precedence_order_ring=PrecedenceOrderRing,
				    config_map=ConfigMap}=State) ->
    ServerRingId = config_map:get_craq_ring_id(ConfigMap),
    CraqNodeProcess = config_map:get_craq_node_process(ConfigMap),
    case NodeRingId =:= ServerRingId of
	true  ->
	    ReplRing1 = replication_ring:add_node(Node, ReplRing),
	    PrecedenceOrderRing1 = craq_precedence_order_ring:add_node(Node, PrecedenceOrder, PrecedenceOrderRing),
	    failure_detector:set({CraqNodeProcess, Node}, ConfigMap),
	    craq_client:abcast(ReplRing1, CraqNodeProcess, {?CRAQ_ADD_NODE, {Node, ReplRing1}}),
	    State#craq_server_state{repl_ring=ReplRing1, precedence_order_ring=PrecedenceOrderRing1};
	false ->
	    State
    end.
	    
-spec handle_msg({MsgTag :: atom(), {PrecedenceOrder :: term(), Msg :: term()}},
		 State :: #craq_server_state{}) -> {term(), #craq_server_state{}}.
handle_msg({?CRAQ_FORWARD_MSG, {PrecedenceOrder, {MsgTag, Msg0}}=Msg},
	   #craq_server_state{server_msg=ServerMsg,
			      config_map=ConfigMap}=State) ->
    CraqNodeProcess = config_map:get_craq_node_process(ConfigMap),
    {CraqNode, State1} = load_balancer:choose(PrecedenceOrder, State),
    ServerMsgRef = unique_id_generator:unique_id(ConfigMap),
    {Flag, ServerMsg1} = craq_server_msg:add({ServerMsgRef, Msg}, ServerMsg),
    craq_client:cast({CraqNodeProcess, CraqNode}, {MsgTag, {ServerMsgRef, Msg0}}),
    {Flag, State1#craq_server_state{server_msg=ServerMsg1}};
handle_msg({?CRAQ_NODE_OUTPUT_MSG, UpdateMsgList},
	   #craq_server_state{server_msg=ServerMsg}=State) ->
    {List1, ServerMsg1} = lists:foldl(fun(#craq_update_msg{server_msg_ref=ServerMsgRefX}=UpdateMsgX, {ListX, ServerMsgX}) -> 
					      {FlagX, ServerMsgX1} = craq_server_msg:remove(ServerMsgRefX, ServerMsgX),
					      {[{FlagX, UpdateMsgX} | ListX], ServerMsgX1}
				      end, {[], ServerMsg}, UpdateMsgList),
    {List1, State#craq_server_state{server_msg=ServerMsg1}};
handle_msg({?CRAQ_MSG_REF_LIST_MSG, MRefList},
	   #craq_server_state{server_msg=ServerMsg}=State) ->
    {MList1, ServerMsg1} = craq_server_msg:find_missing(MRefList, ServerMsg),
    State1 = State#craq_server_state{server_msg=ServerMsg1},
    lists:foldl(fun({_, MsgX}, {_FlagX, StateX}) ->
			handle_msg({?CRAQ_FORWARD_MSG, MsgX}, StateX)
		end, {true, State1}, MList1);
handle_msg(_Msg,
	   #craq_server_state{}=State) ->
    {false, State}.



			 
			
 
			      