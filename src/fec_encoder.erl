-module(fec_encoder).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include("wire_frame.hrl").
-include("fec.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, loop/2, loop/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(CFG) ->
    gen_fsm:start_link(?MODULE, [CFG], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([CFG]) ->
	{ConnID} = CFG,
	put(conn_id, ConnID),
	put(shared_key, <<"=PRESET=">>),
	put(encode_context, #fec_encode_context{}),
	io:format("~p: inited ~p.\n", [?MODULE, self()]),
    {ok, loop, {}}.

loop({encode, Msg}, _From, State) ->
	%io:format("~p: EnFEC msg ~p\n", [?MODULE, Msg]),
	{ok, MsgBin} = msg:encode(Msg),
	MsgBinCi = cryptor:en(MsgBin, get(shared_key)),
	case fec:encode(MsgBinCi, byte_size(MsgBin)) of
		{ok, WireFrames} ->
			Bins = lists:map(fun(F)-> {ok, B}=wire_frame:encode(F), B end, WireFrames),
			{reply, {ok, Bins}, loop, State};
		need_mode ->
			{reply, pass, loop, State}
	end;
loop({encode_push, Msg}, _From, State) ->	
	%io:format("~p: Encoding with push msg ~p\n", [?MODULE, Msg]),
	{ok, MsgBin} = msg:encode(Msg),
	MsgBinCi = cryptor:en(MsgBin, get(shared_key)),
	{ok, [WireFrame]} = fec:encode_push(MsgBinCi, byte_size(MsgBin)),
	{ok, WireFrameBin} = wire_frame:encode(WireFrame),
	{reply, {ok, [WireFrameBin]}, loop, State};
loop(_Event, _From, State) ->
	io:format("~p: Donlt know how to process sync event: ~p\n", [?MODULE, _Event]),
    {reply, ok, loop, State}.

loop(_Event, State) ->
    {next_state, loop, State}.

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
-include("fec_encoder_test.hrl").
-endif.

