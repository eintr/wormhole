-module(fec_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("frame.hrl").
-include("fec.hrl").

-record(fec_frame, {
		  seq,
		  data
		 }).

-record(fecg_slot, {
		  fecg_id,
		  time,
		  width,
		  pending_frames=[]
		 }).

-record(fec_encode_config, {
		  width=2,
		  interleave=1,
		  timeout=10000,	%% in ms
		  current_gid,
		  pending_fecgs=[]
		 }).

-record(fec_decode_config, {
		  minimal_gid=0,
		  pending_fecgs=[]
		 }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, {#fec_encode_config{}, #fec_decode_config{}}}.

handle_call({config, _}, _From, State) ->
    {reply, todo, State};
handle_call({decode, F}, _From, {E, D}) ->
	FecGid = F#frame.fec_info#fec_info.fecg_id,
	FecSeq = F#frame.fec_info#fec_info.fec_seq,
	FecGsize = F#frame.fec_info#fec_info.fec_gsize,
	case llist:keyfind(FecGid, 1, D#fec_decode_config.pending_fecgs) of
		{FecGid, FecGsize, Buffer} ->
			case buffer_process({FecSeq, F#frame.payload}, Buffer) of
				{ok, Packets} ->
					{reply, {ok, Packets}, {E, D}};
				completed ->
					NewPendingFecgs = lists:keydelete(FecGid, 1, D#fec_decode_config.pending_fecgs),
					NewMin = min(D#fec_decode_config.minimal_gid, FecGid),
					NewD = D#fec_decode_config{pending_fecgs=NewPendingFecgs, minimal_gid=NewMin},
					{reply, ok, {E, NewD}};
				need_more ->
					{reply, need_more, {E, D}};
				duplicated ->
					{reply, duplicated, {E, D}};
				too_late ->
					{reply, too_late, {E, D}}
			end;
		{FecGid, _FixedSize, Buffer} ->
			io:format("Differrent fec_gsize(~p) within a fec group with fec_gsize(~p), drop it!\n", [FecGsize, _FixedSize]),
			{reply, {invalid_gsize}, {E, D}};
		false ->
			{reply, {ok, [F]}, {E, D}}
	end;
handle_call({encode, F, Flags}, _From, State) ->
    {reply, {ok, [F]}, State};
handle_call({conn_add, ConnID}, _From, State) ->
    {reply, ok, State};
handle_call({conn_add, ConnID, FecConfig}, _From, State) ->
    {reply, ok, State};
handle_call({conn_delete, ConnID}, _From, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

buffer_process(Buf) ->
	ok.

