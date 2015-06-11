-module(fec_node).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include("fec_frame.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, loop/2, loop/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(Addr) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [Addr], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([Addr]) ->
	put(peeraddr, Addr),
	put(gsize, 2),
	put(interleave, 1),
	put(timeout, 10000),
	put(current_gid, 1),
    {ok, loop, []}.

loop({down, ToAddr, MsgBin}, State) ->	%% TODO: Do the read FEC magic.
	loop({down_push, ToAddr, MsgBin}, State);
loop({down_push, ToAddr, MsgBin}, State) ->	%% TODO: Do the read FEC magic.
	FecInfo = #fec_info{fecg_id=get(current_gid), fec_seq=1, fec_gsize=get(gsize)},
	put(current_gid, next_gid(get(current_gid))),
	{ok, FecFrameBin} = fec_frame:encode(#fec_frame{fec_info=FecInfo, payload=MsgBin}),
	gen_server:cast(transcvr_pool, {down, ToAddr, FecFrameBin}),
	{next_state, loop, State};
loop({up, FromAddr, FecFrameBin}, State) ->	%% TODO: Do the read FEC magic.
	FecFrame = fec_frame:decode(FecFrameBin),
	gen_server:cast(conn_pool, {up, FromAddr, FecFrame#fec_frame.payload }),
	{next_state, loop, State};
loop(_Event, State) ->
    {next_state, loop, State}.

loop(_Event, _From, State) ->
    {reply, ok, state_name, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

next_gid(?GIDMAX) ->
	0;
next_gid(N) ->
	N+1.

