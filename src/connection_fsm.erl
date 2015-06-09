-module(connection_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include("frame.hrl").

-record(conn_state, {
		  conn_id,
		  fec_pid,
		  tun_pid,
		  send_socket
		 }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, start/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, relay/2, relay/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Socket, ConnID) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [Socket, ConnID], []).
start(Socket, ConnID) ->
    gen_fsm:start({local, ?SERVER}, ?MODULE, [Socket, ConnID], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([Socket, ?CONNID_CTRL]) ->
	{ok, FecPID} = fec_server:start_link(),
	ConnState = #conn_state {
	  conn_id = ?CONNID_CTRL,
	  fec_pid = FecPID,
	  tun_pid = 0,
	  send_socket = Socket
	 },
	ok = gen_server:call(udp_dispatcher, {register, {?CONNID_CTRL, FecPID}}),
	{ok, control, ConnState};
init([Socket, ConnID]) ->
	{ok, FecPID} = fec_server:start_link(ConnID),
	{ok, TunPID} = tuncer:create("", [tun, {active, true}]),
	ConnState = #conn_state {
	  conn_id = ConnID,
	  tun_pid = TunPID,
	  send_socket = Socket
	 },
	ok = gen_server:call(udp_dispatcher, {register, {ConnID, FecPID}}),
    {ok, relay, ConnState}.

control({net_packet, Packet}, State) ->
	case gen_server:call(State#conn_state.fec_pid, {decode, Packet}) of
		{ok, Packets} ->
			llist:foreach(control_protocol/1, Packets),
			{next_state, control, State};
		_ ->
			{next_state, control, State}
	end;
control(_Event, State) ->
	io:format("conn/control: Unknown event: ~p\n", [_Event]),
	{next_state, control, State}.

control({admin, new_conn, {ConnID, SharedKey, Uname, Passwd, Addr, Port}=Arg}, _From, State) ->
	gen_server:call(State#conn_state.fec_pid, {encode, frame:encode(#frame{connection_id=?CONNID_CTRL, payload=msg:encode(chap, {})}), [push]});
	{next_state, control_wait_chap_result, {State, Arg}};
control(_Event, _From, State) ->
	io:format("Unknown event: ~p from ~p\n", [_Event, _From]),
    {reply, unknown_event, control, State}.

control_wait_chap_result({net_packet, Pakcet}, {State, Arg}) ->

relay({net_packet, Pakcet}, State) ->
	case gen_server:call(State#conn_state.fec_pid, {decode, Pakcet}) of
		{ok, Packets} ->
			llist:foreach(fun (P)-> tuncer:send(State#conn_state.tun_pid, P) end,
						  llist:map(fun (P)-> cryptor:decrypt(P, key) end, Packets)),
	    	{next_state, relay, State};
		{need_more} ->
			io:format("Incomlete fec group.\n"),
	    	{next_state, relay, State};
		{too_late} ->
			io:format("Frame was late, dropped.\n"),
	    	{next_state, relay, State};
		_Unknown ->
			io:format("fec_server return unknown result: ~p\n", [_Unknown]),
	    	{next_state, relay, State}
	end;
relay({tuntap, _PID, TunPacket}, State) ->
    {next_state, relay, State};
relay(_Event, State) ->
	io:format("conn/relay: Unknown event: ~p\n", [_Event]),
    {next_state, relay, State}.

relay(_Event, _From, State) ->
	io:format("Unknown event: ~p from ~p\n", [_Event, _From]),
    {reply, ok, state_name, State}.

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
control_protocol(Packet, Config) ->
	ok.

