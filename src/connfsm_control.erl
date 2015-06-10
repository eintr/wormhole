-module(connfsm_relay).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include("msg.hrl").
-include("protocol.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, control/2, control/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(State) ->
	{ok, control, State}.

control({up, FromAddr, Msg}, State) ->
	case Msg#msg.code of
		?CODE_CHAP ->
			ok;
		?CODE_CHAP_CONNECT ->
			ok;
		?CODE_CHAP_REJECT ->
			ok;
		?CODE_ECHO ->
			ok;
		_Code ->
			io:format("Control channel doesn't deal with msg ~p, dropped.\n", [_Code])
	end,
	{next_state, control, State};
control(_Event, State) ->
	io:format("conn/control: Unknown event: ~p\n", [_Event]),
	{next_state, control, State}.

control({admin, new_conn, {ConnID, SharedKey, Uname, Passwd, Addr, Port}=Arg}, _From, State) ->
	gen_server:call(State#conn_state.fec_pid, {encode, frame:encode(#frame{connection_id=?CONNID_CTRL, payload=msg:encode(chap, {})}), [push]}),
	{next_state, control_wait_chap_result, {State, Arg}};
control(_Event, _From, State) ->
	io:format("Unknown event: ~p from ~p\n", [_Event, _From]),
    {reply, unknown_event, control, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info({}, StateName, State) ->
    {next_state, StateName, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, State) ->
	ok = tuncer:close(State#conn_state.tun_pid),
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

