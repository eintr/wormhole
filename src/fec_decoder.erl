-module(fec_decoder).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include("wire_frame.hrl").
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
	io:format("~p: inited ~p.\n", [?MODULE, self()]),
	put(shared_key, <<"=PRESET=">>),
    {ok, loop, []}.

loop(timeout, State) ->
    {next_state, loop, State};
loop(_Event, State) ->
    {next_state, loop, State}.

loop(_Event, _From, State) ->
    {reply, ok, state_name, State}.

handle_event({_FromAddr, WireFrame}, _From, State) ->
	FecInfo = WireFrame#wire_frame.fec_info,
	DeltaGid = fec:delta_gid(FecInfo#fec_info.fecg_id, get(minimal_gid)),
	if
		DeltaGid <0 ->
			io:format("Frame(~p) came too late.\n", [FecInfo]),
			{reply, too_late, loop, State};
		(FecInfo#fec_info.fec_gsize == 0) or (FecInfo#fec_info.fec_gsize > 16) ->
			io:format("Frame ~p has invalid fec_gsize.\n", [FecInfo]),
			{reply, invalid, loop, State};
		FecInfo#fec_info.fec_gsize == 1 ->	% No FEC
			PayLoadSize = FecInfo#fec_info.fec_payload_size,
			<<MsgBin:PayLoadSize/binary, _/binary>>
			= cryptor:de(WireFrame#wire_frame.payload_cipher, get(shared_key)),
			Msg = msg:decode(MsgBin),
			{reply, {ok, [Msg]}, loop, State};
		true ->
			case fec:decode(WireFrame) of
				{ok, PayloadCyphers} ->
					Msgs = lists:map(fun(B)->
											 PayLoadSize = FecInfo#fec_info.fec_payload_size,
											 <<MsgBin:PayLoadSize/binary, _/binary>>
											 =cryptor:de(B, get(shared_key)),
											 {ok, M} = msg:decode(MsgBin),
											 M
									 end, PayloadCyphers),
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

