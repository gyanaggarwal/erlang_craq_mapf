-module(craq_node_process).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("craq.hrl").

start_link(ConfigMap) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ConfigMap], []).

init([ConfigMap]) ->    
    State = craq_replication:init(ConfigMap),
    print({init, ConfigMap}, State),
    {ok, State}.

handle_call(?CRAQ_REQ_NODE_STATE,
	    _From,
	    #craq_node_state{}=State) ->
    {reply, State, State};
handle_call(_Msg, 
	    _From, 
	    #craq_node_state{}=State) ->
    {reply, ok, State}.

handle_cast({?CRAQ_REPL_RING, _Msg}=Msg1,
	    #craq_node_state{}=State) ->
    State1 = craq_replication:update_repl_ring(Msg1, State),
    print(Msg1, State1),
    {noreply, State1};
handle_cast({?CRAQ_ADD_NODE, {_Node, _ReplRing}}=Msg1,
	    #craq_node_state{}=State) ->
    State1 = craq_replication:update_repl_ring(Msg1, State),
    print(Msg1, State1),
    {noreply, State1};
handle_cast({?CRAQ_PROCESS_SNAPSHOT, {_Node, _UniqueId}}=Msg1,
	    #craq_node_state{}=State) ->
    State1 = craq_replication:handle_snapshot(Msg1, State),
    print(Msg1, State1),
    {noreply, State1};
handle_cast({?CRAQ_RES_SNAPSHOT, _Msg}=Msg1,
	    #craq_node_state{}=State) ->
    State1 = craq_replication:handle_snapshot(Msg1, State),
    print(Msg1, State1),
    {noreply, State1};
handle_cast(Msg, #craq_node_state{}=State) ->
    PTime = process_time(Msg, State),
    timer:sleep(PTime),
    {RTag, _, State1} = craq_replication:handle_update(Msg, State),
    print_update(RTag, Msg, State1),
    {noreply, State1}.

handle_info(Msg, #craq_node_state{}=State) ->
    State1 = craq_replication:update_repl_ring({?CRAQ_DROP_NODE, Msg}, State),
    print({?CRAQ_DROP_NODE, Msg}, State1),
    {noreply, State1}.

code_change(_OldVsn, #craq_node_state{}=State, _Extra) ->    
    {ok, State}.

terminate(_Reason, #craq_node_state{}) ->
    ok.

print({MsgTag, _}, #craq_node_state{node_id=NodeId, 
				    node_state=NodeState, 
				    repl_ring=ReplRing, 
				    successor=Succ, 
				    predecessor=Pred}) ->
    io:fwrite("~n~w ~w, node=~w, state=~w, ring=~w, succ=~w, pred=~w~n",
	      [?MODULE, MsgTag, NodeId, NodeState, ReplRing, Succ, Pred]).

%print_update(RTag, {MsgTag, {_, {_, _, {PrecOrder, _, _, _}}}}, 
%	     #craq_node_state{node_id=NodeId}=State) ->
%    print(MsgTag, RTag, NodeId, PrecOrder, State);
%print_update(RTag, {_, #craq_update_msg{}}, 
%	     #craq_node_state{}) ->
%    ok.
%print_update(RTag, {MsgTag, #craq_update_msg{msg_node_id=MsgNodeId,
%					     precedence_order=PrecOrder}}, 
%	     #craq_node_state{}=State) ->
%    print(MsgTag, RTag, MsgNodeId, PrecOrder, State).

%print(MsgTag, RTag, MsgNodeId, PrecOrder, #craq_node_state{node_id=NodeId, 
%							   node_state=NodeState,
%							   logical_clock=LogicalClock,
%							   pre_update_msg_list=PreUpdateMsgList,
%							   update_msg_list=UpdateMsgList,
%							   completed_msg_list=CompletedMsgList}) ->
%    io:fwrite("~n~w {~w, ~w}, msg_node_id=~w, prec_order=~w, node=~w, state=~w, clock=~w, pre_update=~w, update=~w, completed=~w~n",
%              [?MODULE, MsgTag, RTag, MsgNodeId, PrecOrder, NodeId, NodeState, LogicalClock, 
%	       craq_update_msg_list:msg_id_list(PreUpdateMsgList), 
%	       craq_update_msg_list:msg_id_list(UpdateMsgList), 
%	       craq_update_msg_list:msg_id_list(CompletedMsgList)]).

print_update(RTag, {MsgTag, {_, {_, _, {PrecOrder, _, _, _}}}},
             #craq_node_state{node_id=NodeId,
                              logical_clock=LogicalClock}) ->
    print_update(MsgTag, RTag, PrecOrder, NodeId, NodeId, LogicalClock); 
print_update(RTag, {MsgTag, #craq_update_msg{precedence_order=PrecOrder,
					     msg_node_id=MsgNodeId,
					     logical_clock=LogicalClock}}, 
	     #craq_node_state{node_id=NodeId}) ->
    print_update(MsgTag, RTag, PrecOrder, NodeId, MsgNodeId, LogicalClock).

print_update(MsgTag, RTag, PrecOrder, NodeId, MsgNodeId, LogicalClock) ->
    io:fwrite("~w {~w, ~w}, node={~w, ~w}, prec_order=~w, clock=~w~n",
	      [?MODULE, MsgTag, RTag, NodeId, MsgNodeId, PrecOrder, LogicalClock]).

process_time({_, {_, {_, _, {PrecOrder, _, _, _}}}},
             #craq_node_state{config_map=ConfigMap}) ->
    PTimeList = config_map:get_craq_process_time(ConfigMap),
    {_, PTime} = lists:keyfind(PrecOrder, 1, PTimeList), 
    PTime;
process_time({_, #craq_update_msg{}},
             #craq_node_state{}) ->
    0.



 
