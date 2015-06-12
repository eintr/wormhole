-module(fec_pool).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("fec_frame.hrl").

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
	io:format("~p: inited.\n", [?MODULE]),
    {ok, {}}.

handle_call(_Request, _From, State) ->
    {reply, null, State}.

handle_cast({up, FromAddr, FrameBin}, State) ->	%% TODO: Do the real fec magic
	{ok, Frame} = fec_frame:decode(FrameBin),
	gen_server:cast(connection_pool, {up, FromAddr, Frame#fec_frame.payload}),
    {noreply, State};
handle_cast({down, ToAddrList, FramePayload}, State) ->
	io:format("~p: Handling cast ~p\n", [?MODULE, {down, ToAddrList, FramePayload}]),
	lists:foreach(fun ({Addr, _Port}=ToAddr)->
			case get(Addr) of
				{Pid} ->
					gen_fsm:send_event(Pid, {down, ToAddr, FramePayload});
				undefined ->
					{ok, Pid} = fec_node:start(Addr),
					put(Addr, {Pid}),
					gen_fsm:send_event(Pid, {down, ToAddr, FramePayload})
			end
		end, ToAddrList),
		{noreply, State};
handle_cast({down_push, ToAddrList, FramePayload}, State) ->
	io:format("~p: Handling cast ~p\n", [?MODULE, {down_push, ToAddrList, FramePayload}]),
	lists:foreach(fun ({Addr, _Port}=ToAddr)->
			case get(Addr) of
				{Pid} ->
					gen_fsm:send_event(Pid, {down_push, ToAddr, FramePayload});
				undefined ->
					{ok, Pid} = fec_node:start(Addr),
					put(Addr, {Pid}),
					gen_fsm:send_event(Pid, {down_push, ToAddr, FramePayload})
			end
		end, ToAddrList),
		{noreply, State};
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

