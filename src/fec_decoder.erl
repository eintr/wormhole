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

loop({up, FromAddr, FecFrameBin}, State) ->

loop(_Event, State) ->
    {next_state, loop, State}.

loop(_Event, _From, State) ->
    {reply, ok, state_name, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event({FromAddr, FecFrameBin}, _From, loop, State) ->
	FecFrame = fec_frame:decode(FecFrameBin),
	FecInfo = FecFrame#fec_frame.fec_info,
	DeltaGid = fec:delta_gid(FecInfo#fec_info.fecg_id, get(minimal_gid)),
	if
		DeltaGid <0 ->
			io:format("Frame(~p) came too late.\n", [FecInfo]),
			{next_state, loop, State};
		(FecInfo#fec_info.fec_gsize == 0) or (FecInfo#fec_info.fec_gsize > 16) ->
			io:format("Frame ~p has invalid fec_gsize.\n", [FecInfo]),
			{next_state, loop, State};
		FecInfo#fec_info.fec_gsize == 1 ->	% No FEC
			gen_server:cast(conn_pool, {up, FromAddr, FecFrame#fec_frame.payload }),
			{next_state, loop, State};
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
					lists:foreach(fun (M)->
										  gen_server:cast(conn_pool, {up, FromAddr, M}) end, Msgs),
					{next_state, loop, State};
				need_more ->
					{next_state, loop, State};
				duplicated ->
					io:format("Frame(~p) is duplicated.\n", [FecInfo]),
					{next_state, loop, State};
				completed ->
					io:format("Frame group ~p is completed.\n", [FecInfo#fec_info.fecg_id]),
					{next_state, loop, State};
				_Other ->
					io:format("Got unknwon return ~p while decoding frame ~p.\n", [_Other, FecInfo]),
    					{reply, ok, loop, State}
			end
	end;
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

