-module(craq_replication).

-export([init/1,
	 update_repl_ring/2,
	 handle_snapshot/2,
	 handle_update/2]).

-include("craq.hrl").

-type next_step_value() :: {term(), term(), #craq_node_state{}}.

-spec init(ConfigMap :: maps:map()) -> #craq_node_state{}.
init(ConfigMap) ->
    Node = node(),
    CraqPrecedenceOrder = config_map:get_craq_precedence_order(ConfigMap),
    State = #craq_node_state{node_id=Node,
			     node_process=config_map:get_craq_node_process(ConfigMap),
			     precedence_order=CraqPrecedenceOrder,
			     config_map=ConfigMap},
    CraqServer = config_map:get_craq_server(ConfigMap),
    CraqServerProcess = config_map:get_craq_server_process(ConfigMap),
    CraqRingId = config_map:get_craq_ring_id(ConfigMap),
    Msg = {?CRAQ_ADD_NODE, {CraqRingId, CraqPrecedenceOrder, Node}},
    craq_client:cast({CraqServerProcess, CraqServer}, Msg),
    State.

-spec update_repl_ring(Msg :: term(),
		       State :: #craq_node_state{}) -> #craq_node_state{}.
update_repl_ring({?CRAQ_REPL_RING, NewReplRing},
		 #craq_node_state{repl_ring=ReplRing}=State) ->
    case missing_node(NewReplRing, ReplRing) of
	false ->
	    State;
	{true, Node} ->
	    update_repl_ring({?CRAQ_ADD_NODE, {Node, NewReplRing}}, State)
    end;
update_repl_ring({?CRAQ_ADD_NODE, {Node, NodeList}},
		 #craq_node_state{node_id=NodeId,
				  node_process=NodeProcess,
				  config_map=ConfigMap}=State) ->
    ValidMsg = craq_node_state:update_repl_ring_msg(Node, State),
    case ValidMsg of
	?CRAQ_INVALID_MSG ->
	    State;
	_ ->
	    NodeOrder = config_map:get_node_order(ConfigMap),
	    {NodeList1, Pred1, Succ1} = replication_ring:get_ordered_list_pred_succ(NodeId, NodeList, NodeOrder),
	    State1 = State#craq_node_state{repl_ring=NodeList1,
					   predecessor=Pred1,
					   successor=Succ1},
	    case {ValidMsg, Succ1} of
		{?CRAQ_VALID_NEW_NODE_MSG, ?CRAQ_UNDEFINED} ->
		    State1#craq_node_state{node_state=?CRAQ_NODE_READY};
		{?CRAQ_VALID_NEW_NODE_MSG, _} ->
		    failure_detector:set(NodeProcess, NodeId, NodeList1, ConfigMap),
		    handle_snapshot({?CRAQ_REQ_SNAPSHOT, Succ1}, State1);
		{?CRAQ_VALID_EXISTING_NODE_MSG, _} ->
		    failure_detector:set({NodeProcess, Node}, ConfigMap),
		    State1
	    end
    end;
update_repl_ring({?CRAQ_DROP_NODE, Msg},
		 #craq_node_state{node_id=NodeId,
				  repl_ring=ReplRing,
				  successor=Succ,
				  predecessor=Pred,
				  config_map=ConfigMap}=State) ->
    case failure_detector:detect(Msg, ConfigMap) of
        {?CRAQ_PROCESS_NODEDOWN, DownNode} ->
	    NodeOrder = config_map:get_node_order(ConfigMap),
            ReplRing1 = replication_ring:drop(DownNode, ReplRing, NodeOrder),
	    Succ1 = replication_ring:successor(NodeId, ReplRing1, NodeOrder),
	    Pred1 = replication_ring:predecessor(NodeId, ReplRing1, NodeOrder),
	    State1 = State#craq_node_state{repl_ring=ReplRing1, predecessor=Pred1, successor=Succ1},
	    case {Succ1, Succ =:= DownNode, Pred =:= DownNode} of
		{?CRAQ_UNDEFINED, _, _} ->
		    State2 = process_down_msg(State1),
		    State2#craq_node_state{pre_update_msg_list=[], 
					   update_msg_list=[],
					   completed_msg_list=[]};
		{_, true, false} ->
		    send_pre_update_down_msg(State1),
		    State1;
		{_, false, true} ->
		    send_msg_ref_list(State1),
		    send_update_down_msg(State1),
		    State1;
		_ ->
		    State1
	    end;
	_ ->            
	    State
    end.
	
-spec handle_snapshot(Msg :: term(),
		      State :: #craq_node_state{}) -> #craq_node_state{}.
handle_snapshot({?CRAQ_REQ_SNAPSHOT, Succ}, 
		#craq_node_state{node_id=NodeId,
				 node_process=NodeProcess,
				 config_map=ConfigMap}=State) ->
    UniqueId = unique_id_generator:unique_id(ConfigMap),
    craq_client:cast({NodeProcess, Succ}, {?CRAQ_PROCESS_SNAPSHOT, {NodeId, UniqueId}}),
    State#craq_node_state{node_state=?CRAQ_NODE_TRANSIENT, 
			  snapshot_ref=UniqueId};
handle_snapshot({?CRAQ_PROCESS_SNAPSHOT, {Pred, UniqueId}},
	        #craq_node_state{node_process=NodeProcess,
				 logical_clock=LogicalClock,
				 data_view=DataView,
				 successor=Succ}=State) ->
    {Msg, State1} = case {craq_node_state:data_state(State), Succ =:= Pred} of
			{?CRAQ_NODE_NOT_READY, false} ->
			    {{?CRAQ_RES_SNAPSHOT, {?CRAQ_NODE_NOT_READY, {UniqueId, Succ}}}, 
			     State};
			{_, _} ->
			    {{?CRAQ_RES_SNAPSHOT, {?CRAQ_NODE_READY, {UniqueId, {LogicalClock, DataView}}}}, 
			     State#craq_node_state{node_state=?CRAQ_NODE_READY}} 
	  end,
    craq_client:cast({NodeProcess, Pred}, Msg),
    State1;
handle_snapshot({?CRAQ_RES_SNAPSHOT, {?CRAQ_NODE_READY, {UniqueId, {LogicalClock, DataView}}}},
		#craq_node_state{snapshot_ref=UniqueId}=State) ->
    State#craq_node_state{node_state=?CRAQ_NODE_READY,
			  logical_clock=LogicalClock,
			  data_view=DataView,
			  snapshot_ref=?CRAQ_UNDEFINED};
handle_snapshot({?CRAQ_RES_SNAPSHOT, {?CRAQ_NODE_NOT_READY, {UniqueId, Succ}}},
		#craq_node_state{snapshot_ref=UniqueId}=State) ->
    handle_snapshot({?CRAQ_REQ_SNAPSHOT, Succ}, State);
handle_snapshot(_, #craq_node_state{}=State) ->
    State.

-spec missing_node(NewReplRing :: list(), ReplRing :: list()) -> false | {true, term()}.
missing_node(NewReplRing, ReplRing) ->
    lists:foldl(fun(Node, false) ->
			case lists:member(Node, ReplRing) of
			    true  ->
				false;
			    false ->
				{true, Node}
			end;
		   (_, {true, Node}) ->
			{true, Node}
		end, false, NewReplRing).
    
-spec handle_update(Msg :: term(),
		    State :: #craq_node_state{}) -> next_step_value().
handle_update({?CRAQ_UPDATE, {ServerMsgRef, {ClientId, ClientMsgRef, {PrecedenceOrder, InputMsg, OutputMsg, UpdateMsgList}}}},
	      #craq_node_state{node_id=NodeId,
			       logical_clock=LogicalClock,
			       successor=Succ,
			       completed_msg_list=CompletedMsgList}=State) ->
    LogicalClock1 = LogicalClock+1,
    UpdateMsg = craq_update_msg:make(LogicalClock1,
				     NodeId,
				     ServerMsgRef,
				     ClientMsgRef,
				     ClientId,
				     PrecedenceOrder,
				     InputMsg,
				     OutputMsg,
				     CompletedMsgList,
				     UpdateMsgList),
    NextStep = case craq_node_state:client_state(State) of
		   ?CRAQ_NODE_NOT_READY ->
		       craq_next_step:get_next_step(?CRAQ_NODE_UNAVAILABLE);
		   _ ->
		       case craq_update_msg:causal_consistency(UpdateMsgList, State) of
			   false ->
			       craq_next_step:get_next_step(?CRAQ_BEING_UPDATED_CAUSALLY);
			   true  ->
			       case Succ of
				   ?CRAQ_UNDEFINED ->
				       craq_next_step:get_next_step(?CRAQ_UPDATE1);
				   _ ->
				       craq_next_step:get_next_step(?CRAQ_PRED_PRE_UPDATE1)
			       end
		       end
	       end,
    next_step(NextStep, UpdateMsg, State);
handle_update({?CRAQ_PRED_PRE_UPDATE, #craq_update_msg{}=UpdateMsg},
	      #craq_node_state{pre_update_msg_list=PreUpdateMsgList,
			       update_msg_list=UpdateMsgList}=State) ->
    process_msg(?CRAQ_PRED_PRE_UPDATE,
		UpdateMsg,
		PreUpdateMsgList,
		UpdateMsgList,
		State);
handle_update({?CRAQ_SUCC_UPDATE, #craq_update_msg{}=UpdateMsg},
	      #craq_node_state{update_msg_list=UpdateMsgList,
                               completed_msg_list=CompletedMsgList}=State) ->
    process_msg(?CRAQ_SUCC_UPDATE,
		UpdateMsg,
		UpdateMsgList,
		CompletedMsgList,
		State);
handle_update(_Msg, 
	      #craq_node_state{}=State) ->
    {?CRAQ_INVALID_MSG, [], State}.
		   
-spec next_step(NextStep :: #craq_next_step{},
		UpdateMsg :: #craq_update_msg{},
		State :: #craq_node_state{}) -> next_step_value().
next_step(#craq_next_step{next_tag=NextTag,
			  make_output=MakeOutput,
			  being_updated_msg_list=BeingUpdatedMsgList}=NextStep,
	  #craq_update_msg{}=UpdateMsg,
	  #craq_node_state{}=State) ->
    send_succ_msg(NextStep, UpdateMsg, State),
    send_pred_msg(NextStep, UpdateMsg, State),
    State1 = craq_node_state:update_state(NextStep, UpdateMsg, State), 
    State2 = craq_data_view:update_view(NextStep, UpdateMsg, State1),
    {NextTag1, MakeOutput1, UpdateMsgList1} = case BeingUpdatedMsgList of
						  [] ->
						      {NextTag, MakeOutput, [UpdateMsg]};
						  _ ->
						      {?CRAQ_BEING_UPDATED, true, BeingUpdatedMsgList}
					      end,
    Output1 = make_output(MakeOutput1, UpdateMsgList1),
    send_output(NextStep, Output1, State2),
    {NextTag1, Output1, State2}.

-spec make_output(MakeOutput :: true | false,
		  UpdateMsgList :: list()) -> list().
make_output(true,
	    UpdateMsgList) ->
    lists:map(fun(#craq_update_msg{}=UpdateMsg) ->
		      craq_update_msg:make_output(UpdateMsg)
	      end, UpdateMsgList);
make_output(false,
	    _UpdateMsgList) ->
    [].

-spec send_succ_msg(NextStep :: #craq_next_step{},
		    UpdateMsg :: #craq_update_msg{},
		    State :: #craq_node_state{}) -> term().
send_succ_msg(#craq_next_step{send_to_succ=true,
			      next_tag=NextTag},
	      #craq_update_msg{}=UpdateMsg,
	      #craq_node_state{successor=Dest}=State) ->
    send_msg(NextTag, Dest, UpdateMsg, State);
send_succ_msg(#craq_next_step{},
	      #craq_update_msg{},
	      #craq_node_state{}) ->
    ok.

-spec send_pred_msg(NextStep :: #craq_next_step{},
                    UpdateMsg :: #craq_update_msg{},
                    State :: #craq_node_state{}) -> term().
send_pred_msg(#craq_next_step{send_to_pred=true,
                              next_tag=NextTag},
	      #craq_update_msg{}=UpdateMsg,
	      #craq_node_state{predecessor=Dest}=State) ->
    send_msg(NextTag, Dest, UpdateMsg, State);
send_pred_msg(#craq_next_step{},
              #craq_update_msg{},
              #craq_node_state{}) ->
    ok.

-spec send_msg(NextTag :: term(),
	       Dest :: term(),
	       UpdateMsg :: #craq_update_msg{},
	       State :: #craq_node_state{}) -> term().    
send_msg(NextTag, 
	 Dest,
	 #craq_update_msg{}=UpdateMsg,
	 #craq_node_state{config_map=ConfigMap}) ->
    CraqNodeProcess = config_map:get_craq_node_process(ConfigMap),
    craq_client:cast({CraqNodeProcess, Dest}, {NextTag, UpdateMsg}).

-spec send_server_msg(Msg :: term(),
		      State :: #craq_node_state{}) -> term().
send_server_msg(Msg,
		#craq_node_state{config_map=ConfigMap}) ->
    CraqServer = config_map:get_craq_server(ConfigMap),
    CraqServerProcess = config_map:get_craq_server_process(ConfigMap),
    craq_client:cast({CraqServerProcess, CraqServer}, Msg).

-spec send_output(NextStep :: #craq_next_step{},
                  Output :: term(),
                  State :: #craq_node_state{}) -> term().
send_output(#craq_next_step{send_output=true},
            Output,
            #craq_node_state{}=State) ->
    send_server_msg({?CRAQ_NODE_OUTPUT_MSG, Output}, State);
send_output(#craq_next_step{},
            _Output,
            #craq_node_state{}) ->
    ok.

-spec send_msg_ref_list(State :: #craq_node_state{}) -> term().
send_msg_ref_list(#craq_node_state{}=State) ->
    List = server_msg_ref_list(State),
    send_server_msg({?CRAQ_MSG_REF_LIST_MSG, List}, State).

-spec process_msg(MsgTag :: ?CRAQ_PRED_PRE_UPDATE | ?CRAQ_SUCC_UPDATE,
		  UpdateMsg :: #craq_update_msg{},
		  UpdateMsgList1 :: list(),
		  UpdateMsgList2 :: list(),
		  State :: #craq_node_state{}) -> next_step_value().
process_msg(MsgTag,
	    #craq_update_msg{}=UpdateMsg,
	    UpdateMsgList1,
	    UpdateMsgList2,
	    #craq_node_state{}=State) ->
    {{ValidMsgTag, BeingUpdatedMsgList}, #craq_node_state{}=State1} = craq_update_msg:valid_msg(MsgTag,
												UpdateMsg,
												UpdateMsgList1,
												UpdateMsgList2,
												State),
    next_step(craq_next_step:get_next_step(craq_next_step:get_next_tag(ValidMsgTag, MsgTag), BeingUpdatedMsgList), UpdateMsg, State1).
			 
-spec send_pre_update_down_msg(State :: #craq_node_state{}) -> term().
send_pre_update_down_msg(#craq_node_state{pre_update_msg_list=PreUpdateMsgList}=State) ->
    send_down_msg(?CRAQ_PRED_PRE_UPDATE, PreUpdateMsgList, State).

-spec send_update_down_msg(State ::#craq_node_state{}) -> term().
send_update_down_msg(#craq_node_state{update_msg_list=UpdateMsgList}=State) ->
    send_down_msg(?CRAQ_SUCC_UPDATE, UpdateMsgList, State).

-spec send_down_msg(MsgTag :: ?CRAQ_PRED_PRE_UPDATE | ?CRAQ_SUCC_UPDATE, 
		    UpdateMsgList :: list(),
		    State :: #craq_node_state{}) -> term().
send_down_msg(MsgTag, UpdateMsgList, #craq_node_state{}=State) ->
    lists:foreach(fun(UpdateMsg) ->
			  MsgStatus = craq_update_msg:msg_status(UpdateMsg, State),
			  next_step(craq_next_step:get_next_step(craq_next_step:get_next_tag(MsgStatus, MsgTag)), UpdateMsg, State)
		  end, UpdateMsgList).

-spec process_down_msg(State :: #craq_node_state{}) -> term().
process_down_msg(#craq_node_state{pre_update_msg_list=PreUpdateMsgList,
				  update_msg_list=UpdateMsgList}=State) ->
    send_msg_ref_list(State),
    State1 = process_down_msg(?CRAQ_PRE_UPDATE_DOWN, PreUpdateMsgList, State),
    process_down_msg(?CRAQ_UPDATE_DOWN, UpdateMsgList, State1).

-spec process_down_msg(MsgTag :: ?CRAQ_PRE_UPDATE_DOWN | ?CRAQ_UPDATE_DOWN,
		       UpdateMsgList :: list(),
		       State :: #craq_node_state{}) -> term().
process_down_msg(MsgTag, 
		 UpdateMsgList, 
		 #craq_node_state{}=State) ->
    lists:foldl(fun(UpdateMsg, StateX) ->
			  {_, _, StateX1} = next_step(craq_next_step:get_next_step(MsgTag), UpdateMsg, StateX),
			StateX1
		  end, State, UpdateMsgList).

-spec server_msg_ref_list(State :: #craq_node_state{}) -> list().
server_msg_ref_list(#craq_node_state{pre_update_msg_list=PreUpdateMsgList,
				     update_msg_list=UpdateMsgList}) ->
    List1 = craq_update_msg_list:server_msg_ref_list(PreUpdateMsgList),
    List2 = craq_update_msg_list:server_msg_ref_list(UpdateMsgList),
    List1 ++ List2.




			  
			  

			   

			 
	
			    