-module(fec_pool).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("fec_frame.hrl").

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

start_link(Socket) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Socket], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Socket]) ->
	DownIndex = [],	%% of {{addr, port}, fec_encoder_pid}
    {ok, {[]}}.

handle_call(_Request, _From, State) ->
    {reply, null, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({up, FromAddr, FrameBin}, State) ->
    {noreply, State}.
handle_info({down, ToAddr, FramePayload}, {DownIndex}=State) ->
	case lists:keyfind(ToAddr, 1, DownIndex) of
		{ToAddr, Pid} ->
			Pid ! {down, ToAddr, FramePayload},
			{noreply, State};
		false ->
			{ok, Pid} = fec_encoder:start(ToAddr),
			{noreply, {DownIndex ++ [{ToAddr, Pid}]}}
	end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

