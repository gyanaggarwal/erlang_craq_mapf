-module(load_balancer).

-export([init/1,
	 choose/2]).

-include("craq.hrl").

-callback initialize(State :: #craq_server_state{}) -> #craq_server_state{}.

-callback choose_node(PrecedenceOrder :: term(), State :: #craq_server_state{}) -> {term(), #craq_server_state{}}.

-spec init(State :: #craq_server_state{}) -> #craq_server_state{}.
init(#craq_server_state{config_map=ConfigMap}=State) ->
    LoadBalancerMod = config_map:get_load_balancer(ConfigMap),
    LoadBalancerMod:initialize(State).

-spec choose(PrecedenceOrder :: term(), State :: #craq_server_state{}) -> {term(), #craq_server_state{}}.
choose(PrecedenceOrder, #craq_server_state{config_map=ConfigMap}=State) ->
    LoadBalancerMod = config_map:get_load_balancer(ConfigMap),
    LoadBalancerMod:choose_node(PrecedenceOrder, State).
