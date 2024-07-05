-module(craq_server_process).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("craq.hrl").

start_link(ConfigMap) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ConfigMap], []).

init([ConfigMap]) ->  
    State = craq_server_state:init(ConfigMap),
    print({init, ConfigMap}, State),
    {ok, State}.

handle_call(_Msg, 
	    _From, 
	    #craq_server_state{}=State) ->
    {reply, ok, State}.

handle_cast({?CRAQ_SET_REPL_RING, _Msg}=Msg1,
	    #craq_server_state{}=State) ->
    State1 = craq_server_state:update_repl_ring(Msg1, State),
    print(Msg1, State1),
    {noreply, State1};
handle_cast({?CRAQ_ADD_NODE, _Msg}=Msg1,
	    #craq_server_state{}=State) ->
    State1 = craq_server_state:update_repl_ring(Msg1, State),
    print(Msg1, State1),
    {noreply, State1};
handle_cast(Msg,
	    #craq_server_state{}=State) ->
    {_, State1} = craq_server_state:handle_msg(Msg, State),
%    print_msg(Msg, State1),
    {noreply, State1}.

handle_info(Msg, #craq_server_state{}=State) ->
    State1 = craq_server_state:update_repl_ring({?CRAQ_DROP_NODE, Msg}, State),
    print({?CRAQ_DROP_NODE, Msg}, State1),
    {noreply, State1}.

code_change(_OldVsn, #craq_server_state{}=State, _Extra) ->    
    {ok, State}.

terminate(_Reason, #craq_server_state{}) ->
    ok.

print({MsgTag, _}, #craq_server_state{repl_ring=ReplRing, precedence_order_ring=PrecedenceOrderRing}) ->
    io:fwrite("~n~w ~w, repl_ring=~w, precedence_order_ring=~w~n", [?MODULE, MsgTag, ReplRing, PrecedenceOrderRing]).

%print_msg({MsgTag, _}, #craq_server_state{server_msg=#craq_server_msg{mref_map=MRefMap}}) ->
%    io:fwrite("~n~w ~w, mref_map=~w~n", [?MODULE, MsgTag, MRefMap]).

