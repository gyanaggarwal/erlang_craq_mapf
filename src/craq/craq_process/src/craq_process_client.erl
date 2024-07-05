-module(craq_process_client).

-export([start/1,
	 stop/1,
	 node_state/1,
	 set_repl_ring/2,
	 send_msg/3,
	 get_node/0]).

-include("craq.hrl").

-define(PO_LIST,                [50, 50, 50, 50, 50, 20, 20, 20]).
-define(ID_LIST,                [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]).
-define(CRAQ_DEMO_TYPE,         craq_demo_type).
-define(CRAQ_SERVER_PROCESS,    craq_server_process).

start(AppName) ->
    application:start(AppName).

stop(AppName) ->    
    application:stop(AppName).

get_node() ->
    cs@localhost.

node_state(ServerRef) ->
    craq_client:call(ServerRef, ?CRAQ_REQ_NODE_STATE).

set_repl_ring(ServerRef, ReplRing) ->
    craq_client:cast(ServerRef, {?CRAQ_SET_REPL_RING, ReplRing}).

make_node_msg(ClientId, MsgRef, PrecedenceOrder) ->
    DataId = random_util:choose_from_list(?ID_LIST),
    {?CRAQ_UPDATE, {ClientId, MsgRef, {PrecedenceOrder, ?CRAQ_UNDEFINED, ?CRAQ_UNDEFINED, [{{?CRAQ_DEMO_TYPE, DataId}, {update, []}}]}}}.

make_server_msg(ClientId, MsgRef) ->
    PrecedenceOrder = random_util:choose_from_list(?PO_LIST),
    {?CRAQ_FORWARD_MSG, {PrecedenceOrder, make_node_msg(ClientId, MsgRef, PrecedenceOrder)}}.

send_msg(Node, NoMessage, SleepTime) ->
    random_util:init(),
    ClientId = self(),
    MsgRef = make_ref(),
    ServerRef = {?CRAQ_SERVER_PROCESS, Node},
    lists:foreach(fun(_) ->
			  Msg = make_server_msg(ClientId, MsgRef),
			  craq_client:cast(ServerRef, Msg),
			  timer:sleep(SleepTime)
		  end, lists:seq(1, NoMessage)).


			  



