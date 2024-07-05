-module(craq_update_msg_list).

-export([add_msg/3,
	 remove_msg/3,
	 remove_list/3,
	 exists/2,
	 extract_node_id/2,
	 msg_id_list/1,
	 server_msg_ref_list/1]).

-include("craq.hrl").

-spec add_msg(UpdateFlag :: true | false,
              UpdateMsg :: #craq_update_msg{},
              MsgList :: list()) -> list().
add_msg(true,
        #craq_update_msg{}=UpdateMsg,
        MsgList) ->
    [UpdateMsg | remove_msg(true, UpdateMsg, MsgList)];
add_msg(false,
        #craq_update_msg{},
        MsgList) ->
    MsgList.

-spec remove_msg(UpdateFlag :: true | false,
                 UpdateMsg :: #craq_update_msg{},
                 MsgList :: list()) -> list().
remove_msg(true,
           #craq_update_msg{}=UpdateMsg,
           MsgList) ->
    MsgId = craq_update_msg:msg_id(UpdateMsg),
    lists:filter(fun(#craq_update_msg{}=UMX) ->
                         craq_update_msg:msg_id(UMX) =/= MsgId
                 end, MsgList);
remove_msg(false,
           #craq_update_msg{},
           MsgList) ->
    MsgList.

-spec remove_list(UpdateFlag :: true | false,
                  MsgCompletedMsgList :: list(),
                  MsgList :: list()) -> list().
remove_list(true,
            MsgCompletedMsgList,
            MsgList) ->
    lists:filter(fun(#craq_update_msg{}=UMX) -> 
                         not exists(UMX, MsgCompletedMsgList)
                 end, MsgList);
remove_list(false,
            _MsgCompletedMsgList,
            MsgList) ->
    MsgList.

-spec exists(UpdateMsg :: #craq_update_msg{},
             MsgList :: list()) -> true | false.
exists(#craq_update_msg{}=UpdateMsg,
       MsgList) ->
    MsgId = craq_update_msg:msg_id(UpdateMsg),
    lists:any(fun(UMX) ->
                      craq_update_msg:msg_id(UMX) =:= MsgId
              end, MsgList).

-spec extract_node_id(NodeId :: term(),
		      UpdateMsgList :: list()) -> list().
extract_node_id(NodeId,
		UpdateMsgList) ->
    lists:filter(fun(#craq_update_msg{msg_node_id=MsgNodeId}) ->
			 NodeId =:= MsgNodeId
		 end, UpdateMsgList).

-spec msg_id_list(UpdateMsgList :: list()) -> list().
msg_id_list(UpdateMsgList) ->     
    lists:map(fun(UpdateMsg) ->
		      craq_update_msg:msg_id(UpdateMsg)
	      end, UpdateMsgList).

-spec server_msg_ref_list(UpdateMsgList :: list()) -> list().
server_msg_ref_list(UpdateMsgList) ->
    lists:map(fun(#craq_update_msg{server_msg_ref=ServerMsgRef}) ->
		      ServerMsgRef
	      end, UpdateMsgList).


