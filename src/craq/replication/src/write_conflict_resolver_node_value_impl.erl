-module(write_conflict_resolver_node_value_impl).

-behavior(write_conflict_resolver).

-export([resolve/2]).

-include("craq.hrl").

-spec resolve({Tag1 :: atom(), Value1 :: term()}, 
	      {Tag2 :: atom(), Value2 :: term()}) -> term().
resolve({?CRAQ_PRED_PRE_UPDATE, {?CRAQ_CRITERIA_VALUE, {Criteria1, Value1}}},
	{?CRAQ_PRED_PRE_UPDATE, {?CRAQ_CRITERIA_VALUE, {Criteria2, Value2}}}) ->
    case Criteria1 > Criteria2 of
	true  ->
	    {Criteria1, Value1};
	false ->
	    {Criteria2, Value2}
    end;
resolve({?CRAQ_PRED_PRE_UPDATE, Criteria1},  
	{?CRAQ_PRED_PRE_UPDATE, Criteria2}) ->
    max(Criteria1, Criteria2);
resolve({?CRAQ_PRED_PRE_UPDATE, _Critetia1}, 
	{?CRAQ_SUCC_UPDATE,     Criteria2}) ->
    Criteria2;
resolve({?CRAQ_SUCC_UPDATE,     Criteria1},  
	{?CRAQ_PRED_PRE_UPDATE, _Criteria2}) ->
    Criteria1.
