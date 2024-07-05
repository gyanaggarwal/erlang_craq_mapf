-module(craq_client).

-export([call/2,
	 cast/2,
	 abcast/3]).

-spec call(ServerRef :: term(),
	   Msg :: term()) -> term().
call(ServerRef, 
     Msg) ->
    gen_server:call(get_server_ref(ServerRef), Msg).

-spec cast(ServerRef :: term(), 
	   Msg :: term()) -> term().
cast(ServerRef, 
     Msg) -> 
    gen_server:cast(get_server_ref(ServerRef), Msg).

-spec abcast(NodeList :: list(), 
	     Server :: atom(), 
	     Msg :: term()) -> term().
abcast(NodeList, 
       Server, 
       Msg) ->     
    gen_server:abcast(NodeList, Server, Msg).

-spec get_server_ref(ServerRef :: term()) -> term().
get_server_ref({Name, Node}) ->
    case Node =:= node() of
	true  ->
	    Name;
	false ->
	    {Name, Node}
    end;
get_server_ref(ServerRef) ->
    ServerRef.

