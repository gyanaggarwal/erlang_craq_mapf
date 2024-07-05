-module(craq_next_step).

-export([get_next_step/1,
	 get_next_step/2,
	 get_next_tag/2]).

-include("craq.hrl").

-spec get_next_step(NextTag :: term(),
		    BeingUpdatedMsgList :: list()) -> #craq_next_step{}.
get_next_step(NextTag, 
	      BeingUpdatedMsgList) ->
    NextStep = get_next_step(NextTag),
    case BeingUpdatedMsgList of
	[] ->
	    NextStep;
	_ ->
	    NextStep#craq_next_step{being_updated_msg_list=BeingUpdatedMsgList,
				    send_output=true}
    end.

-spec get_next_step(NextTag :: term()) -> #craq_next_step{}.
get_next_step(?CRAQ_PRE_UPDATE_DOWN) ->
    #craq_next_step{next_tag=?CRAQ_UPDATED,
		    update_logical_clock=false,
		    make_output=true,
		    send_output=true,
		    update_data_view=true};
get_next_step(?CRAQ_UPDATE_DOWN) ->
    #craq_next_step{next_tag=?CRAQ_UPDATED,
		    update_logical_clock=false,
		    send_output=true,
		    make_output=true};
get_next_step(?CRAQ_NODE_UNAVAILABLE)->
    #craq_next_step{next_tag=?CRAQ_NODE_UNAVAILABLE,
		    make_output=true,
		    send_output=true,
		    update_logical_clock=false};
get_next_step(?CRAQ_BEING_UPDATED_CAUSALLY) ->
    #craq_next_step{next_tag=?CRAQ_BEING_UPDATED_CAUSALLY,
		    make_output=true,
		    send_output=true,
		    update_logical_clock=false};
get_next_step(?CRAQ_UPDATE1) ->
    #craq_next_step{next_tag=?CRAQ_UPDATED,
		    make_output=true,
		    send_output=true,
		    update_data_view=true};
get_next_step(?CRAQ_PRED_PRE_UPDATE1) ->
    #craq_next_step{next_tag=?CRAQ_PRED_PRE_UPDATE, 
		    send_to_succ=true, 
		    add_to_pre_update=true};
get_next_step(?CRAQ_BEING_UPDATED) ->
    #craq_next_step{next_tag=?CRAQ_BEING_UPDATED,
		    make_output=true,
		    send_output=true,
		    remove_complete=true};
get_next_step(?CRAQ_BEING_UPDATED_HEAD) ->    
    #craq_next_step{next_tag=?CRAQ_BEING_UPDATED_HEAD,
                    make_output=true,
                    send_output=true,
                    remove_complete=true};
get_next_step(?CRAQ_INVALID_MSG) ->
    #craq_next_step{next_tag=?CRAQ_INVALID_MSG,
		    update_logical_clock=false};
get_next_step(?CRAQ_PRED_PRE_UPDATE) ->
    #craq_next_step{next_tag=?CRAQ_PRED_PRE_UPDATE,
		    send_to_succ=true,
		    add_to_pre_update=true,
		    remove_complete=true};
get_next_step(?CRAQ_SUCC_UPDATE) ->
    #craq_next_step{next_tag=?CRAQ_SUCC_UPDATE,
		    update_data_view=true,
		    make_output=true,
		    send_to_pred=true,
		    move_to_update=true};
get_next_step(?CRAQ_HEAD_MSG) ->
    #craq_next_step{next_tag=?CRAQ_UPDATED,
		    update_data_view=true,
		    make_output=true,
		    send_output=true,
		    move_to_complete=true,
		    remove_from_complete=true};
get_next_step(?CRAQ_TAIL_MSG) ->
    #craq_next_step{next_tag=?CRAQ_SUCC_UPDATE,
		    update_data_view=true,
		    make_output=true,
		    send_to_pred=true,
		    add_to_update=true,
		    remove_complete=true}.

-spec get_next_tag(ValidMsgTag :: term(), 
		   MsgTag :: term()) -> term().
get_next_tag(?CRAQ_RING_MSG, MsgTag) ->
    MsgTag;
get_next_tag(ValidMsgTag, _MsgTag) ->
    ValidMsgTag.




			  
			  

			   

			 
	
			    