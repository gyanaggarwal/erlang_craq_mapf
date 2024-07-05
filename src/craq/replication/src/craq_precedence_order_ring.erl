-module(craq_precedence_order_ring).

-export([get_ring/3,
	 set_ring/1,
	 add_node/3,
	 drop_node/2]).

-include("craq.hrl").

-spec get_ring(PrecedenceOrder :: term(),
	       PrecedenceOrderRing :: list(),
	       ReplRing :: list()) -> list().
get_ring(PrecedenceOrder,
	 PrecedenceOrderRing,
	 ReplRing) ->
    case lists:keyfind(PrecedenceOrder, 1, PrecedenceOrderRing) of
	false ->
	    ReplRing;
	{_, PORing} ->
	    PORing
    end.

-spec set_ring({MsgTag :: ?CRAQ_REPL_RING | ?CRAQ_PRECEDENCE_ORDER_RING, Ring :: list()}) -> {list(), list()}.
set_ring({?CRAQ_PRECEDENCE_ORDER_RING, PrecedenceOrderRing}) ->
    ReplRing = lists:foldl(fun({_, PORX}, RingX) ->
				   RingX ++ PORX
			   end, [], PrecedenceOrderRing),
    {ReplRing, PrecedenceOrderRing};
set_ring({?CRAQ_REPL_RING, ReplRing}) ->
    {ReplRing, [{?CRAQ_DEFAULT_PRECEDENCE_ORDER, ReplRing}]}.

-spec add_node(Node :: term(),
	       PrecedenceOrder :: term(),
	       PrecedenceOrderRing :: list()) -> list().
add_node(Node, 
	 PrecedenceOrder,
	 PrecedenceOrderRing) ->
    case lists:keyfind(PrecedenceOrder, 1, PrecedenceOrderRing) of
	false ->
	    [{PrecedenceOrder, [Node]} | PrecedenceOrderRing];
	{_, PORing} ->
	    PORing1 = [Node | lists:delete(Node, PORing)],
	    lists:keyreplace(PrecedenceOrder, 1, PrecedenceOrderRing, {PrecedenceOrder, PORing1})
    end.

-spec drop_node(Node :: term(),
		PrecedenceOrderRing :: list()) -> list().
drop_node(Node,
	  PrecedenceOrderRing) ->
    lists:foldl(fun({POX, PORX}, RingX) ->
			PORX1 = lists:delete(Node, PORX),
			case PORX1 of
			    [] ->
				RingX;
			    _ ->
				[{POX, PORX1} | RingX]
			end
		end, [], PrecedenceOrderRing).


		       
		      

				       