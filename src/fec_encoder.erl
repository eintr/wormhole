-module(fec_encoder).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include("fec_frame.hrl").

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
	put(conn_id, ConnID),
	put(gsize, 2),
	put(interleave, 1),
	put(timeout, 10000),
	put(current_gid, 1),
	put(encode_context, []),
    {ok, loop, {}}.

loop({down, ToAddr, Msg}, State) ->
	case fec:encode(Msg) of
		{ok, FecFrames} ->
			lists:foreach(fun (F)->
								  {ok, FecFrameBin} = fec_frame:encode(F),
								  gen_server:cast(transcvr_pool, {down, ToAddr, FecFrameBin})
						  end, FecFrames),
			{next_state, loop, State};
		{need_mode} ->
			{next_state, loop, State}
	end;
loop({down_push, ToAddr, MsgBin}, State) ->	
	{ok, FecFrames} = fec:encode_push(MsgBin),
	lists:foreach(fun (F)->
						  {ok, FecFrameBin} = fec_frame:encode(F),
						  gen_server:cast(transcvr_pool, {down, ToAddr, FecFrameBin})
				  end, FecFrames),
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

-ifdef(TEST).
-include("fec_encoder_test.hrl").
-endif.

