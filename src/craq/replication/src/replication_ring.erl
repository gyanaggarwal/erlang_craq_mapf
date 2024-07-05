-module(replication_ring).

-export([drop/3,
	 drop_node/2,
         add/3,
	 add_node/2,
         predecessor/3,
         successor/3,
         get_ordered_list/2,
         get_ordered_list_pred_succ/3,
         effective_head_node_id/3,
	 effective_head_node_id/4,
         effective_tail_node_id/3]).

-include("craq.hrl").

-type node_order() :: ?CRAQ_NODE_ORDER_SORTED | {?CRAQ_NODE_ORDER_USER_DEFINED, list()}.

-spec drop_node(Node :: atom(),
		NodeList :: list()) -> list().
drop_node(Node, 
	  NodeList) ->
    lists:delete(Node, NodeList).

-spec add_node(Node :: atom(),
	       NodeList :: list()) -> list().
add_node(Node, 
	 NodeList) ->
    [Node | drop_node(Node, NodeList)].
		      
-spec get_ordered_list(NodeList :: list(), 
		       NodeOrder :: node_order()) -> list().
get_ordered_list(NodeList, 
		 ?CRAQ_NODE_ORDER_SORTED) ->
    lists:sort(NodeList);
get_ordered_list(NodeList, 
		 {?CRAQ_NODE_ORDER_USER_DEFINED, NodeOrderList}) ->
    lists:filter(fun(X) -> lists:member(X, NodeList) end, NodeOrderList).

-spec drop(Node :: atom(),
	   NodeList :: list(),
	   NodeOrder :: node_order()) -> list().
drop(Node, 
     NodeList, 
     NodeOrder) ->
    get_ordered_list(drop_node(Node, NodeList), NodeOrder).

-spec add(Node :: atom(),
	  NodeList :: list(),
	  NodeOrder :: node_order()) -> list().
add(Node, 
    NodeList, 
    NodeOrder) ->
    get_ordered_list(add_node(Node, NodeList), NodeOrder).

-spec predecessor(Node :: atom(),
		  NodeList :: list(),
		  NodeOrder :: node_order()) -> atom().
predecessor(Node, 
	    NodeList, 
	    NodeOrder) ->
    NewNodeList = add(Node, NodeList, NodeOrder),
    {L1, L2} = lists:splitwith(fun(N) -> N =/= Node end, NewNodeList),
    case {length(L1), length(L2)} of
	{0, 0} -> 
	    ?CRAQ_UNDEFINED;
	{0, 1} ->
	    ?CRAQ_UNDEFINED;
	{0, _} ->
	    lists:last(L2);
	{_, _} ->
	    lists:last(L1)
    end.

-spec successor(Node :: atom(),
		NodeList :: list(),
		NodeOrder :: node_order()) -> atom().
successor(Node, 
	  NodeList, 
	  NodeOrder) ->
    NewNodeList = add(Node, NodeList, NodeOrder),
    {L1, L2} = lists:splitwith(fun(N) -> N =/= Node end, NewNodeList),
    case {length(L1), length(L2)} of
	{0, 0} ->
	    ?CRAQ_UNDEFINED;
	{0, 1} ->
	    ?CRAQ_UNDEFINED;
	{_, 1} ->
	    hd(L1);
	{_, _} ->
	    [Node, Succ | _] = L2,
	    Succ
  end.

-spec get_ordered_list_pred_succ(Node :: atom(),
				 NodeList :: list(),
				 NodeOrder :: node_order()) -> {list(), atom(), atom()}.
get_ordered_list_pred_succ(Node, 
			   NodeList, 
			   NodeOrder) ->
    NodeList1 = get_ordered_list(NodeList,  NodeOrder),
    Pred = predecessor(Node, NodeList, NodeOrder),
    Succ = successor(Node, NodeList, NodeOrder),
    {NodeList1, Pred, Succ}.

-spec effective_head_node_id(MsgNodeId :: atom(),
			     NodeList :: list(),
			     NodeOrder :: node_order()) -> atom().
effective_head_node_id(MsgNodeId, 
		       NodeList, 
		       NodeOrder) ->
    case lists:member(MsgNodeId, NodeList) of
	true  ->
	    MsgNodeId;
	false ->
	    successor(MsgNodeId, NodeList, NodeOrder)
    end.

-spec effective_tail_node_id(MsgNodeId :: atom(),
			     NodeList :: list(),
			     NodeOrder :: node_order()) -> atom().
effective_tail_node_id(MsgNodeId, 
		       NodeList, 
		       NodeOrder) ->
    predecessor(MsgNodeId, NodeList, NodeOrder).

-spec effective_head_node_id(MsgTag :: ?CRAQ_PRED_PRE_UPDATE | ?CRAQ_SUCC_UPDATE,
			     MsgNodeId :: atom(),
			     NodeList :: list(),
			     NodeOrder :: node_order()) -> atom().
effective_head_node_id(MsgTag,
		       MsgNodeId,
		       NodeList,
		       NodeOrder) ->
    case {lists:member(MsgNodeId, NodeList), MsgTag} of
	{true, _} ->
	    MsgNodeId;
	{false, ?CRAQ_PRED_PRE_UPDATE} ->
	    predecessor(MsgNodeId, NodeList, NodeOrder);
	{false, ?CRAQ_SUCC_UPDATE} ->
	    successor(MsgNodeId, NodeList, NodeOrder)
    end.
				    

