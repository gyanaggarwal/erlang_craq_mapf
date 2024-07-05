-module(config_map).

-export([get_env/1,
	 get_node_order/1,
	 get_load_balancer/1,
	 get_failure_detector/1,
	 get_write_conflict_resolver/1,
	 get_unique_id_generator/1,
	 get_craq_node_process/1,
	 get_craq_server_process/1,
	 get_craq_server/1,
	 get_craq_ring_id/1,
	 get_craq_process_time/1,
	 get_craq_precedence_order/1,
	 get_craq_precedence_plan/1,
	 get_craq_precedence_move/1,
         get_sup_restart_intensity/1,
         get_sup_restart_period/1,
         get_sup_child_shutdown/1]).

-include("craq.hrl").

-define(X_NODE_ORDER,                          {node_order,              ?CRAQ_NODE_ORDER_SORTED}).
-define(X_LOAD_BALANCER,                       {load_balancer,           load_balancer_random_impl}).
-define(X_FAILURE_DETECTOR,                    {failure_detector,        failure_detector_monitor_impl}).
-define(X_WRITE_CONFLICT_RESOLVER,             {write_conflict_resolver, write_conflict_resolver_node_value_impl}).
-define(X_UNIQUE_ID_GENERATOR,                 {unique_id_generator,     unique_id_generator_impl}).
-define(X_CRAQ_NODE_PROCESS,                   {craq_node_process,       ?CRAQ_UNDEFINED}).
-define(X_CRAQ_SERVER_PROCESS,                 {craq_server_process,     ?CRAQ_UNDEFINED}).
-define(X_CRAQ_SERVER,                         {craq_server,             ?CRAQ_UNDEFINED}).
-define(X_CRAQ_RING_ID,                        {craq_ring_id,            ?CRAQ_UNDEFINED}).
-define(X_CRAQ_PROCESS_TIME,                   {craq_process_time,       []}).
-define(X_CRAQ_PRECEDENCE_ORDER,               {craq_precedence_order,   0}).
-define(X_CRAQ_PRECEDENCE_PLAN,                {craq_precedence_plan,    0}).
-define(X_CRAQ_PRECEDENCE_MOVE,                {craq_precedence_move,    0}).
-define(X_SUP_RESTART_INTENSITY,               {sup_restart_intensity,   100}).
-define(X_SUP_RESTART_PERIOD,                  {sup_restart_period,      1}).
-define(X_SUP_CHILD_SHUTDOWN,                  {sup_child_shutdown,      2000}).

-define(X_CRAQ_PARAM,                          [?X_NODE_ORDER,
						?X_LOAD_BALANCER,
						?X_FAILURE_DETECTOR,
						?X_WRITE_CONFLICT_RESOLVER,
						?X_UNIQUE_ID_GENERATOR,
						?X_CRAQ_NODE_PROCESS,
						?X_CRAQ_SERVER_PROCESS,
						?X_CRAQ_SERVER,
						?X_CRAQ_RING_ID,
						?X_CRAQ_PROCESS_TIME,
						?X_CRAQ_PRECEDENCE_ORDER,
						?X_CRAQ_PRECEDENCE_PLAN,
						?X_CRAQ_PRECEDENCE_MOVE,
					        ?X_SUP_RESTART_INTENSITY,
					        ?X_SUP_RESTART_PERIOD,
					        ?X_SUP_CHILD_SHUTDOWN]).
-spec get_env(AppName :: term()) -> maps:map().
get_env(AppName) ->
    lists:foldl(fun({PX, DX}, MapX) ->
			maps:put(PX, application:get_env(AppName, PX, DX), MapX)
		end, maps:new(), ?X_CRAQ_PARAM).

-spec get_node_order(ConfigMap :: maps:map()) -> term().
get_node_order(ConfigMap) ->
    get(?X_NODE_ORDER, ConfigMap).

-spec get_load_balancer(ConfigMap :: maps:map()) -> term().
get_load_balancer(ConfigMap) ->    
    get(?X_LOAD_BALANCER, ConfigMap).

-spec get_failure_detector(ConfigMap :: maps:map()) -> term().
get_failure_detector(ConfigMap) ->    
    get(?X_FAILURE_DETECTOR, ConfigMap).

-spec get_write_conflict_resolver(ConfigMap :: maps:map()) -> term().
get_write_conflict_resolver(ConfigMap) ->    
    get(?X_WRITE_CONFLICT_RESOLVER, ConfigMap).

-spec get_unique_id_generator(ConfigMap :: maps:map()) -> term().
get_unique_id_generator(ConfigMap) ->     
    get(?X_UNIQUE_ID_GENERATOR, ConfigMap).

-spec get_craq_node_process(ConfigMap :: maps:map()) -> term().
get_craq_node_process(ConfigMap) ->
    get(?X_CRAQ_NODE_PROCESS, ConfigMap).

-spec get_craq_server_process(ConfigMap :: maps:map()) -> term().
get_craq_server_process(ConfigMap) ->     
    get(?X_CRAQ_SERVER_PROCESS, ConfigMap).

-spec get_craq_server(ConfigMap :: maps:map()) -> term().
get_craq_server(ConfigMap) ->
    get(?X_CRAQ_SERVER, ConfigMap).

-spec get_craq_ring_id(ConfigMap :: maps:map()) -> term().
get_craq_ring_id(ConfigMap) ->
    get(?X_CRAQ_RING_ID, ConfigMap).

-spec get_craq_process_time(ConfigMap :: maps:map()) -> term().
get_craq_process_time(ConfigMap) ->
    get(?X_CRAQ_PROCESS_TIME, ConfigMap).

-spec get_craq_precedence_order(ConfigMap :: maps:map()) -> term().
get_craq_precedence_order(ConfigMap) ->    
    get(?X_CRAQ_PRECEDENCE_ORDER, ConfigMap).

-spec get_craq_precedence_plan(ConfigMap :: maps:map()) -> term().
get_craq_precedence_plan(ConfigMap) ->     
    get(?X_CRAQ_PRECEDENCE_PLAN, ConfigMap).

-spec get_craq_precedence_move(ConfigMap :: maps:map()) -> term().
get_craq_precedence_move(ConfigMap) ->     
    get(?X_CRAQ_PRECEDENCE_MOVE, ConfigMap).

-spec get_sup_restart_intensity(ConfigMap :: maps:map()) -> term().
get_sup_restart_intensity(ConfigMap) ->
    get(?X_SUP_RESTART_INTENSITY, ConfigMap).

-spec get_sup_restart_period(ConfigMap :: maps:map()) -> term().
get_sup_restart_period(ConfigMap) ->    
    get(?X_SUP_RESTART_PERIOD, ConfigMap).

-spec get_sup_child_shutdown(ConfigMap :: maps:map()) -> term().
get_sup_child_shutdown(ConfigMap) ->    
    get(?X_SUP_CHILD_SHUTDOWN, ConfigMap).

-spec get({Param :: term(), Default :: term()}, 
	  ConfigMap :: maps:map()) -> term().
get({Param, Default}, 
    ConfigMap) ->
    maps:get(Param, ConfigMap, Default).