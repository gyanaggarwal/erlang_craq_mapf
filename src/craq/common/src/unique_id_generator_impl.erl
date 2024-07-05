-module(unique_id_generator_impl).

-behavior(unique_id_generator).

-export([unique_id/0]).

-spec unique_id() -> term().
unique_id() ->
    make_ref().

