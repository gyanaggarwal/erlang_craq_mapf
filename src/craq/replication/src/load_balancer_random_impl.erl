-module(load_balancer_random_impl).

-behaviour(load_balancer).

-export([initialize/1,
         choose_node/2]).

-include("craq.hrl").

-spec initialize(State :: #craq_server_state{}) -> #craq_server_state{}.
initialize(#craq_server_state{}=State) ->
    random_util:init(),
    State.

-spec choose_node(PrecedenceOrder :: term(), 
		  State :: #craq_server_state{}) -> {term(), #craq_server_state{}}.
choose_node(PrecedenceOrder, 
	    #craq_server_state{repl_ring=ReplRing,
			       precedence_order_ring=PrecedenceOrderRing}=State) ->
    Ring = craq_precedence_order_ring:get_ring(PrecedenceOrder, PrecedenceOrderRing, ReplRing),
    {random_util:choose_from_list(Ring), State}.
