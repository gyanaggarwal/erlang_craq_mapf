-module(craq_data_view).

-export([update_view/3]).

-include("craq.hrl").

-spec update_view(NextStep :: #craq_next_step{},
		  UpdateMsg :: #craq_update_msg{},
		  State :: #craq_node_state{}) -> #craq_node_state{}.
update_view(#craq_next_step{update_data_view=true},
	    #craq_update_msg{logical_clock=LogicalClock,
			     update_msg_list=UpdateMsgList},
	    #craq_node_state{data_view=DataView}=State) ->
    DataView1 = lists:foldl(fun({{DataTypeX, DataIdX}, {UpdateTagX, UpdateColList}}, DataViewX) ->
				    DataTypeX:update(UpdateTagX, DataIdX, LogicalClock, UpdateColList, DataViewX)
			    end, DataView, UpdateMsgList),
    State#craq_node_state{data_view=DataView1};
update_view(#craq_next_step{},
	    #craq_update_msg{},
	    #craq_node_state{}=State) ->
    State.

		     



