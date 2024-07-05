-module(write_conflict_resolver).

-export([resolve/3]).

-include("craq.hrl").

-callback resolve(Value1 :: term(), Value2 :: term()) -> term().

-spec resolve(Value1 :: term(), 
	      Value2 :: term(), 
	      State :: #craq_node_state{}) -> term().
resolve(Value1, 
	Value2, 
	#craq_node_state{config_map=ConfigMap}) ->
    WriteConflictResolverMod = config_map:get_write_conflict_resolver(ConfigMap),
    WriteConflictResolverMod:resolve(Value1, Value2).