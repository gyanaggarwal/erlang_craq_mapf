-module(craq_valid_msg).

-export([update_repl_ring_msg/2]).

-include("craq.hrl").

-spec update_repl_ring_msg(Node :: atom(), 
			   State :: #craq_node_state{}) -> ?CRAQ_INVALID_MSG |
							   ?CRAQ_VALID_NEW_NODE_MSG |
							   ?CRAQ_VALID_EXISTING_NODE_MSG.
update_repl_ring_msg(Node, 
		     #craq_node_state{node_id=NodeId}=State) ->
    case {craq_node_state:msg_state(State), Node =:= NodeId} of
	{?CRAQ_NODE_NOT_READY, true} ->
	    ?CRAQ_VALID_NEW_NODE_MSG;
	{?CRAQ_NODE_READY, false} ->
	    ?CRAQ_VALID_EXISTING_NODE_MSG;
	_ ->
	    ?CRAQ_INVALID_MSG
    end.

  