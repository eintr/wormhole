-module(transcvr_pool).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

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
	{ok, Filename} = application:get_env(configfile),
	{ok, Config} = file:script(Filename),
	LocalAddrs = case lists:keyfind(local_addrs, 1, Config) of
					 {local_addrs, AddrList} ->
						 lists:filtermap(fun (Str)->
												 case inet:parse_ipv4_address(Str) of 
													 {ok, Addr} -> {true, Addr};
													 {error, _} -> false
												 end
										 end, AddrList);
					 false ->
						 [{0,0,0,0}]
				 end,
	LocalPorts = case lists:keyfind(local_ports, 1, Config) of
					 {local_ports, ListInConf} -> ListInConf;
					 false -> [0]
				 end,
	L = [{Addr, Port} || Addr<-LocalAddrs, Port<-LocalPorts],
	%io:format("~p: Try to open socket on ~p.\n", [?MODULE, L]),
	Sockets = lists:map(fun ({Addr, Port})->
							case gen_udp:open(Port, [binary, {ip, Addr}, {active, true}]) of
								{ok, Socket} ->
									{ok, {Addr, Port}, Socket};
								_ -> {error, {Addr, Port}, "Bind error."}
							end
					end, L),
	%io:format("~p: Sockets is ~p\n", [?MODULE, Sockets]),
	% Report error.
	lists:foreach(fun ({error, Addr, Reason})->
						  io:format("Failed to open socket on ~p: ~p\n", [Addr, Reason]);
					  (_) -> nothing_todo
				  end, Sockets),
	DownIndex = lists:filtermap(fun ({ok, A, S})	-> {true, {A, S}};
					 (_)	-> false
				 end, Sockets),
	%io:format("~p: DownIndex is ~p\n", [?MODULE, DownIndex]),
    {ok, {DownIndex}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({down, {DAddr, DPort}, WireBin}, {DownIndex}) ->
	[{_Addr, Socket}=H|T] = DownIndex,
	io:format("~p: Going to send(~p, ~p, ~p, ~p)..\n", [?MODULE, Socket, DAddr, DPort, WireBin]),
	gen_udp:send(Socket, DAddr, DPort, WireBin),	% TODO: traffic detection here!
	{noreply, {T++[H]}};
handle_cast(_Msg, State) ->
	io:format("~p: Don't know how to deal with ~p\n", [?MODULE, _Msg]),
    {noreply, State}.

handle_info({udp, _Socket, SAddr, SPort, WireBin}, State) ->
	{ok, WireFrame} = wire_frame:decode(WireBin),
	gen_server:cast(connection_pool, {up, {SAddr, SPort}, WireFrame}),
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

