-module(craq_server_msg).

-export([add/2,
	 remove/2,
	 find_missing/2]).

-include("craq.hrl").

-spec add({MRef :: term(), Msg :: term()},
	  ServerMsg :: #craq_server_msg{}) -> {true | false, #craq_server_msg{}}.
add({MRef, Msg},
    #craq_server_msg{mref_map=MRefMap}=ServerMsg) ->
    {true, ServerMsg#craq_server_msg{mref_map=maps:put(MRef, Msg, MRefMap)}}.

-spec remove(MRef :: term(),
	     ServerMsg :: #craq_server_msg{}) -> {true | false, #craq_server_msg{}}.
remove(MRef,
       #craq_server_msg{mref_map=MRefMap}=ServerMsg) ->
    case maps:find(MRef, MRefMap) of
	error ->
	    {false, ServerMsg};
	_ ->
	    {true, ServerMsg#craq_server_msg{mref_map=maps:remove(MRef, MRefMap)}}
    end.

-spec find_missing(MRefList :: list(),
		   ServerMsg :: #craq_server_msg{}) -> {list(), #craq_server_msg{}}.
find_missing(MRefList,
	     #craq_server_msg{mref_map=MRefMap}) ->
    MList = maps:fold(fun(MRefX, MsgX, ListX) ->
			      case lists:member(MRefX, MRefList) of
				  true  ->
				      ListX;
				  false ->
				      [{MRefX, MsgX} | ListX]
			      end
		      end, [], MRefMap),
    MRefMap1 = lists:foldl(fun({MRefX, _MsgX}, MRefMapX) ->
				   maps:remove(MRefX, MRefMapX)
			   end, MRefMap, MList),
    {MList, #craq_server_msg{mref_map=MRefMap1}}.

			  

		    
		 