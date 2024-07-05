-module(random_util).

-export([init/0,
	 choose_from_list/1]).

-spec init() -> term().
init() ->
    rand:seed(exs1024).

-spec choose_from_list(List :: list()) -> term().
choose_from_list(List) ->
    N = rand:uniform(length(List)),
    lists:nth(N, List).

    