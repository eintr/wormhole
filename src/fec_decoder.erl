-module(fec_decoder).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include("fec_frame.hrl").
-include("fec.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, loop/2, loop/3, handle_event/3,
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

init([]) ->
	put(timeout, 10000),
	put(minimal_gid, 0),
    {ok, loop, []}.

loop(timeout, State) ->
    {next_state, loop, State};
loop(_Event, State) ->
    {next_state, loop, State}.

loop(_Event, _From, State) ->
    {reply, ok, state_name, State}.

handle_event({FromAddr, FecFrameBin}, _From, State) ->
	FecFrame = fec_frame:decode(FecFrameBin),
	FecInfo = FecFrame#fec_frame.fec_info,
	DeltaGid = fec:delta_gid(FecInfo#fec_info.fecg_id, get(minimal_gid)),
	if
		DeltaGid <0 ->
			io:format("Frame(~p) came too late.\n", [FecInfo]),
			{reply, too_late, loop, State};
		(FecInfo#fec_info.fec_gsize == 0) or (FecInfo#fec_info.fec_gsize > 16) ->
			io:format("Frame ~p has invalid fec_gsize.\n", [FecInfo]),
			{reply, invalid, loop, State};
		FecInfo#fec_info.fec_gsize == 1 ->	% No FEC
			gen_server:cast(conn_pool, {up, FromAddr, FecFrame#fec_frame.payload }),
			{reply, {ok, [FecFrame#fec_frame.payload]}, loop, State};
		true ->
			case get({decode_context, FecInfo#fec_info.fecg_id}) of
				undefined ->
					Context = #fecg_context{	id = FecInfo#fec_info.fecg_id,
												width = FecInfo#fec_info.fec_gsize,
												timestamp = util:timestamp_ms(),
												pool=[]},
					put({decode_context, FecInfo#fec_info.fecg_id}, Context);
				_ -> ok
			end,
			case fec:decode(FecFrame) of
				{ok, Msgs} ->
					{reply, {ok, Msgs}, loop, State};
				need_more ->
					{reply, pass, loop, State};
				duplicated ->
					io:format("Frame(~p) is duplicated.\n", [FecInfo]),
					{reply, pass, loop, State};
				completed ->
					erase(FecInfo#fec_info.fecg_id),
					io:format("Frame group ~p is completed.\n", [FecInfo#fec_info.fecg_id]),
					{reply, pass, loop, State};
				_Other ->
					io:format("Got unknwon return ~p while decoding frame ~p.\n", [_Other, FecInfo]),
    				{reply, unknown, loop, State}
			end
	end;
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


-ifdef(TEST).
-include("fec_decoder_test.hrl").
-endif.

