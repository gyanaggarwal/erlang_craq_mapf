-module(craq_node_state).

-export([set_state/2,
	 client_state/1,
	 msg_state/1,
	 data_state/1,
	 update_repl_ring_msg/2,
	 update_logical_clock/3,
	 update_state/3]).

-include("craq.hrl").

-spec set_state(NodeState :: ?CRAQ_NODE_READY | ?CRAQ_NODE_NOT_READY | ?CRAQ_NODE_TRANSIENT,
		State :: #craq_node_state{}) -> #craq_node_state{}.
set_state(NodeState, #craq_node_state{}=State) ->
    State#craq_node_state{node_state=NodeState}.
		       
-spec client_state(State :: #craq_node_state{}) -> ?CRAQ_NODE_READY | ?CRAQ_NODE_NOT_READY.
client_state(#craq_node_state{node_state=?CRAQ_NODE_READY}) ->
    ?CRAQ_NODE_READY;
client_state(#craq_node_state{}) ->    
    ?CRAQ_NODE_NOT_READY.

-spec msg_state(State :: #craq_node_state{}) -> ?CRAQ_NODE_READY | ?CRAQ_NODE_NOT_READY.
msg_state(#craq_node_state{node_state=?CRAQ_NODE_NOT_READY}) ->    
    ?CRAQ_NODE_NOT_READY;
msg_state(#craq_node_state{}) ->
    ?CRAQ_NODE_READY.

-spec data_state(State :: #craq_node_state{}) -> ?CRAQ_NODE_READY | ?CRAQ_NODE_NOT_READY.
data_state(#craq_node_state{node_state=?CRAQ_NODE_READY}) ->
    ?CRAQ_NODE_READY;
data_state(#craq_node_state{}) -> 
    ?CRAQ_NODE_NOT_READY.

-spec update_repl_ring_msg(Node :: atom(),
			   State :: #craq_node_state{}) -> ?CRAQ_INVALID_MSG |
							   ?CRAQ_VALID_NEW_NODE_MSG |
							   ?CRAQ_VALID_EXISTING_NODE_MSG.
update_repl_ring_msg(Node,
		     #craq_node_state{node_id=NodeId}=State) ->
    case {msg_state(State), Node =:= NodeId} of
        {?CRAQ_NODE_NOT_READY, true} ->
            ?CRAQ_VALID_NEW_NODE_MSG;
	{?CRAQ_NODE_READY, false} ->
            ?CRAQ_VALID_EXISTING_NODE_MSG;
        _ ->
            ?CRAQ_INVALID_MSG
    end.

-spec update_logical_clock(UpdateFlag :: true | false,
			   LogicalClock :: non_neg_integer(),
			   State :: #craq_node_state{}) -> #craq_node_state{}.
update_logical_clock(UpdateFlag,
		     LogicalClock1,
		     #craq_node_state{logical_clock=LogicalClock0}=State) ->
    State#craq_node_state{logical_clock=logical_clock(UpdateFlag, LogicalClock1, LogicalClock0)}.

-spec update_state(NextStep :: #craq_next_step{},
		   UpdateMsg :: #craq_update_msg{},
		   State :: #craq_node_state{}) -> #craq_node_state{}.
update_state(#craq_next_step{update_logical_clock=UpdateLogicalClock,
			     remove_complete=RemoveComplete,
			     remove_from_complete=RemoveFromComplete,
			     add_to_pre_update=AddToPreUpdate,
			     add_to_update=AddToUpdate,
			     move_to_update=MoveToUpdate,
			     move_to_complete=MoveToComplete},
	     #craq_update_msg{logical_clock=LogicalClock1,
			      completed_msg_list=MsgCompletedMsgList}=UpdateMsg,
	     #craq_node_state{logical_clock=LogicalClock0,
			      pre_update_msg_list=PreUpdateMsgList,
			      update_msg_list=UpdateMsgList,
			      completed_msg_list=CompletedMsgList}=State) ->

    PUML1 = craq_update_msg_list:remove_list(RemoveComplete, MsgCompletedMsgList, PreUpdateMsgList),
    UML1  = craq_update_msg_list:remove_list(RemoveComplete, MsgCompletedMsgList, UpdateMsgList),
    
    CML1  = craq_update_msg_list:remove_list(RemoveFromComplete, MsgCompletedMsgList, CompletedMsgList),

    PUML2 = craq_update_msg_list:remove_msg((MoveToUpdate orelse MoveToComplete), UpdateMsg, PUML1),
     
    PUML3 = craq_update_msg_list:add_msg(AddToPreUpdate, UpdateMsg, PUML2),
    
    UML2  = craq_update_msg_list:add_msg((AddToUpdate orelse MoveToUpdate), UpdateMsg, UML1),
    
    CML2  = craq_update_msg_list:add_msg(MoveToComplete, UpdateMsg, CML1),
    
    LC2   = logical_clock(UpdateLogicalClock, LogicalClock1, LogicalClock0),

    State#craq_node_state{logical_clock=LC2,
			  pre_update_msg_list=PUML3, 
			  update_msg_list=UML2,
			  completed_msg_list=CML2}.
			  
-spec logical_clock(UpdateFlag :: true | false,
		    LogicalClock1 :: non_neg_integer(),
		    LogicalClock0 :: non_neg_integer()) -> non_neg_integer().
logical_clock(true,
	      LogicalClock1,
	      LogicalClock0) ->
    max(LogicalClock1, LogicalClock0);
logical_clock(false,
	      _LogicalClock1,
	      LogicalClock0) ->
    LogicalClock0.





			      

			

		     
				  


