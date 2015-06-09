-module(transcvr_pool).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

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

start_link(AddrList) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [AddrList], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([AddrList]) ->
	Sockets = lists:map(fun ({Addr, Port})->
							case gen_udp:open(Port, [binary, {ip, Addr}]) of
								{ok, Socket} ->
									{ok, {Addr, Port}, Socket};
								_ -> {error, {Addr, Port}, "Bind error."}
							end
					end, AddrList),
	lists:foreach(fun ({error, Addr, Reason})->
						  io:format("Failed to open socket on ~p: ~p\n", [Addr, Reason]);
					  (_) -> nothing_todo
				  end, Sockets),
	DownIndex = lists:filtermap(fun ({ok, A, P})	-> {true, {A, P}};
					 (_)	-> false
				 end, Sockets),
    {ok, {DownIndex}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({down, {DAddr, DPort}, FrameBin}, {DownIndex}) ->
	[H|T] = DownIndex,
	ok = gen_udp:send(H, DAddr, DPort, FrameBin),
	{noreply, {T++[H]}};
handle_info({udp, _Socket, SAddr, SPort, PacketBin}, State) ->
	fec_server ! {up, {SAddr, SPort}, PacketBin},
	{noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

