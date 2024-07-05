-module(craq_update_msg).

-export([causal_consistency/2,
	 write_conflict/3,
	 make/10,
	 make_output/1,
	 msg_id/1,
	 msg_equal/2,
	 msg_status/2,
	 valid_msg/5]).

-include("craq.hrl").

-spec causal_consistency(UpdateMsgList :: list(), 
			 State :: #craq_node_state{}) -> true | false.
causal_consistency(UpdateMsgList,
		   #craq_node_state{pre_update_msg_list=PreUpdateMsgList}) ->
    not check_conflict2(UpdateMsgList, PreUpdateMsgList).

-spec write_conflict(MsgTag :: ?CRAQ_PRED_PRE_UPDATE | ?CRAQ_SUCC_UPDATE,
		     UpdateMsg :: #craq_update_msg{},
		     State :: #craq_node_state{}) -> {true | false, {list(), list()}}.
write_conflict(?CRAQ_PRED_PRE_UPDATE,
	       #craq_update_msg{logical_clock=LogicalClock1,
			        msg_node_id=MsgNodeId1,
			        update_msg_list=UML1}=UM1,
	       #craq_node_state{pre_update_msg_list=PreUpdateMsgList}=State) ->
    {Flag, {WL, LL}} = lists:foldl(fun(#craq_update_msg{logical_clock=LogicalClock2,
							msg_node_id=MsgNodeId2}=UM2,
				       {Flag2, {L21, L22}}) ->
					   case LogicalClock1 =:= LogicalClock2 andalso MsgNodeId1 =/= MsgNodeId2 of
					       true  ->
						   case check_conflict1(UML1, UM2) of
						       true ->
							   {{_, WNodeId}, WUM} = write_conflict_resolver:resolve({?CRAQ_PRED_PRE_UPDATE, 
														  {?CRAQ_CRITERIA_VALUE, {conflict_resolve_criteria(UM1), UM1}}},
														 {?CRAQ_PRED_PRE_UPDATE, 
														  {?CRAQ_CRITERIA_VALUE, {conflict_resolve_criteria(UM2), UM2}}},
														 State),
							   LUM = case WNodeId =:= MsgNodeId1 of
								     true ->
									 UM2;
								     false ->
									 UM1
								 end,
							   {true, {craq_update_msg_list:add_msg(true, WUM, L21), craq_update_msg_list:add_msg(true, LUM, L22)}};
						       false ->
							   {Flag2, {L21, L22}}
						   end;
					       false ->
						   {Flag2, {L21, L22}}
					   end
				   end, {false, {[], []}}, PreUpdateMsgList),
    case Flag of
	true ->
	    {true, {craq_update_msg_list:remove_list(true, LL, WL), LL}};
	false ->
	    {false, {[], []}}
    end;
write_conflict(?CRAQ_SUCC_UPDATE,
	       #craq_update_msg{},
	       #craq_node_state{}) ->
    {false, {[], []}}.

-spec make(LogicalClock :: non_neg_integer(),
	   MsgNodeId :: term(),
	   ServerMsgRef :: term(),
	   ClientMsgRef :: term(),
	   ClientId :: term(),
	   PrecedenceOrder :: term(),
	   InputMsg :: term(),
	   OutputMsg :: term(),
	   CompletedMsgList :: list(),
	   UpdateMsgList :: list()) -> #craq_update_msg{}.
make(LogicalClock,
     MsgNodeId,
     ServerMsgRef,
     ClientMsgRef,
     ClientId,
     PrecedenceOrder,
     InputMsg,
     OutputMsg,
     CompletedMsgList,
     UpdateMsgList) ->			
    #craq_update_msg{logical_clock=LogicalClock,
		     msg_node_id=MsgNodeId,
		     server_msg_ref=ServerMsgRef,
		     client_msg_ref=ClientMsgRef,
		     client_id=ClientId,
		     precedence_order=PrecedenceOrder,
		     input_msg=InputMsg,
		     output_msg=OutputMsg,
		     completed_msg_list=CompletedMsgList,
		     update_msg_list=UpdateMsgList}.

-spec make_output(UpdateMsg :: #craq_update_msg{}) -> #craq_update_msg{}.
make_output(#craq_update_msg{}=UpdateMsg) ->
    UpdateMsg#craq_update_msg{completed_msg_list=?CRAQ_UNDEFINED,
			      update_msg_list=?CRAQ_UNDEFINED}.

-spec msg_id(UpdateMsg :: #craq_update_msg{}) -> term().
msg_id(#craq_update_msg{logical_clock=LogicalClock,
			msg_node_id=MsgNodeId}) ->
    {LogicalClock, MsgNodeId}.

-spec msg_equal(UpdateMsg1 :: #craq_update_msg{},
		UpdateMsg2 :: #craq_update_msg{}) -> true | false.
msg_equal(#craq_update_msg{}=UpdateMsg1,
	  #craq_update_msg{}=UpdateMsg2) ->
    msg_id(UpdateMsg1) =:= msg_id(UpdateMsg2).

-spec valid_msg(MsgTag :: ?CRAQ_PRED_PRE_UPDATE | ?CRAQ_SUCC_UPDATE,
		UpdateMsg :: #craq_update_msg{},
		UpdateMsgList1 :: list(),
		UpdateMsgList2 :: list(),
		State :: #craq_node_state{}) -> {{?CRAQ_HEAD_MSG | ?CRAQ_TAIL_MSG | ?CRAQ_RING_MSG | ?CRAQ_INVALID_MSG | ?CRAQ_BEING_UPDATED | ?CRAQ_BEING_UPDATED_HEAD, 
						  list()},
						 #craq_node_state{}}.
valid_msg(MsgTag,
	  #craq_update_msg{msg_node_id=MsgNodeId}=UpdateMsg,
	  UpdateMsgList1,
	  UpdateMsgList2,
	  #craq_node_state{node_id=NodeId,
			   repl_ring=ReplRing,
			   config_map=ConfigMap,
			   pre_update_msg_list=PreUpdateMsgList}=State) ->
    NodeOrder = config_map:get_node_order(ConfigMap),
    Flag1 = craq_update_msg_list:exists(UpdateMsg, UpdateMsgList1) orelse craq_update_msg_list:exists(UpdateMsg, UpdateMsgList2),
    case Flag1 of 
	true  ->
	    {{?CRAQ_INVALID_MSG, []}, State};
	false ->
	    case write_conflict(MsgTag, UpdateMsg, State) of
		{true, {[WUpdateMsg], LList}} ->
		    State1 = State#craq_node_state{pre_update_msg_list=craq_update_msg_list:remove_list(LList, PreUpdateMsgList)},
		    case msg_equal(WUpdateMsg, UpdateMsg) of
			true  ->
			    {{msg_status(UpdateMsg, State1), extract_effective_head_node_id(LList, State)}, State1};
			false ->
			    HeadNodeId = replication_ring:effective_head_node_id(MsgNodeId, ReplRing, NodeOrder),
			    BeingUpdatedTag = case HeadNodeId =:= NodeId of
						  true  ->
						      ?CRAQ_BEING_UPDATED_HEAD;
						  false ->
						      ?CRAQ_BEING_UPDATED
					      end,
			    {{BeingUpdatedTag, []}, State1}
		    end;
		{false, {_, _}} ->
		    {{msg_status(UpdateMsg, State), []}, State}
	    end
    end.
	    	       
-spec conflict_resolve_criteria(UpdateMsg :: #craq_update_msg{}) -> term().
conflict_resolve_criteria(#craq_update_msg{msg_node_id=MsgNodeId, precedence_order=PrecedenceOrder}) ->
    {PrecedenceOrder, MsgNodeId}.

-spec check_conflict1(UML1 :: list(),
		      UpdateMsg :: #craq_update_msg{}) -> true | false.
check_conflict1(UML1, 
		#craq_update_msg{update_msg_list=UML2}) ->
    lists:any(fun({PKX, _}) ->
		      case lists:keyfind(PKX, 1, UML2) of
			  false ->
			      false;
			  _ ->
			      true
		      end
	      end, UML1).
			     
-spec check_conflict2(UML1 :: list(), 
		      UMList2 :: list()) -> true | false.
check_conflict2(UML1,
		UMList2) ->
    lists:any(fun(#craq_update_msg{}=UMX) ->
		      check_conflict1(UML1, UMX)
	      end, UMList2).

-spec msg_status(UpdateMsg :: #craq_update_msg{},
		 State :: #craq_node_state{}) -> ?CRAQ_RING_MSG | ?CRAQ_HEAD_MSG | ?CRAQ_TAIL_MSG.
msg_status(#craq_update_msg{msg_node_id=MsgNodeId},
           #craq_node_state{node_id=NodeId, 
			    repl_ring=ReplRing, 
			    config_map=ConfigMap}) ->
    NodeOrder = config_map:get_node_order(ConfigMap),
    TailNodeId = replication_ring:effective_tail_node_id(MsgNodeId, ReplRing, NodeOrder),
    HeadNodeId = replication_ring:effective_head_node_id(MsgNodeId, ReplRing, NodeOrder),
    case {NodeId =:= HeadNodeId, NodeId =:= TailNodeId} of
        {true, false}  ->
	    ?CRAQ_HEAD_MSG;
	{false, true}  ->
            ?CRAQ_TAIL_MSG;
        {false, false} ->
            ?CRAQ_RING_MSG
    end.

-spec extract_effective_head_node_id(UpdateMsgList :: list(),
				     State :: #craq_node_state{}) -> list().
extract_effective_head_node_id(UpdateMsgList,
			       #craq_node_state{node_id=NodeId,
						repl_ring=ReplRing,
						config_map=ConfigMap}) ->
    NodeOrder = config_map:get_node_order(ConfigMap),
    lists:map(fun(#craq_update_msg{msg_node_id=MsgNodeId}) ->
		      HeadNodeId = replication_ring:effective_head_node_id(MsgNodeId, ReplRing, NodeOrder),
		      NodeId =:= HeadNodeId
	      end, UpdateMsgList).

		  
					    
				
