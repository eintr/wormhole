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
    {ok, {}}.

handle_call(_Request, _From, State) ->
    {reply, null, State}.

handle_cast({up, FromAddr, FrameBin}, State) ->
	gen_server:cast(connection_pool, {up, [{FromAddr, FrameBin}]}),
    {noreply, State};
handle_cast({down, ToAddrList, FramePayload}, State) ->
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

