-module(unique_id_generator).

-export([unique_id/1]).

-callback unique_id() -> term().

-spec unique_id(ConfigMap :: maps:map()) -> term().
unique_id(ConfigMap) ->
    UniqueIdGeneratorMod = config_map:get_unique_id_generator(ConfigMap), 
    UniqueIdGeneratorMod:unique_id().
