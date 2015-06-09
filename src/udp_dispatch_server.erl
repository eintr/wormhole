-module(udp_dispatch_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("frame.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(SockAddr) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [SockAddr], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{Addr, Port}]) ->
	io:format("~s is initiating.\n", [?MODULE]),
	case gen_udp:open(Port, [binary, {ip, Addr}]) of
		{ok, Socket} ->
			{ok, _Pid} = connection_fsm:start_link(Socket, ?CONNID_CTRL),
			{ok, _Pid} = connection_fsm:start_link(Socket, ?CONNID_INVAL),
			FilterList = [],
			StatList = [],
			{ok, {Socket, FilterList, StatList}};
		{error, Reason} -> {stop, Reason}
	end.

handle_call({register, ConnID}, From, {Socket, FilterList, StatList}) ->
   	{reply, ok, {Socket, lists:keystore(ConnID, 1, FilterList, {ConnID, fun (P)-> conn_dispatch(From, P) end}), StatList}};
handle_call({unregister, ConnID}, _From, {Socket, FilterList, StatList}) ->
	{reply, ok, {Socket, lists:keydelete(ConnID, 1, FilterList), StatList}};
handle_call(_Request, _From, State) ->
    {reply, {error, "Unknown call request."}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Socket, SAddr, SPort, Packet}, {Socket, FilterList, _StatList}=State) ->
	{ok, FecFrame} = fec_frame:decode(Packet),
	fec_server ! {up, {SAddr, SPort}, FecFrame}
    {noreply, State};
handle_info(_Info, State) ->
	io:format("Unknown info: ~p\n", [_Info]),
    {noreply, State}.

terminate(_Reason, {Socket, _FilterList, _StatList}) ->
	gen_udp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

conn_dispatch(Pid, Pakcet) ->
	Pid ! {net_packet, Pakcet}.

