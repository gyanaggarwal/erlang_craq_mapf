-module(craq_demo_type).

-export([update/5]).

-include("craq.hrl").

-spec update(UpdateTag :: term(),
	     DataId :: term(),
	     LogicalClock :: non_neg_integer(),
	     UpdateColList :: list(),
	     DataView :: term()) -> term().
update(_UpdateTag,
       _DataId,
       _LogicalClock,
       _UpdateColList,
       DataView) ->
    DataView.

		    