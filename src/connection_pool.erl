-module(connection_pool).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("wire_frame.hrl").
-include("protocol.hrl").

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
	{ok, Pid} = case lists:keyfind(mode, 1, Config) of
					{ mode, server } ->
						connfsm_control_server:start_link();
					{ mode, client } ->
						connfsm_control_client:start_link();
					_M ->
						io:format("~p: Running mode ~p is unknown.\n", [?MODULE, _M]),
						crash
				end,
	put(?CONNID_CTRL, {Pid}),
	io:format("~p: inited.\n", [?MODULE]),
	{ok, {}}.

handle_call({destroy_conn, ConnID}, _From, State) ->
	case get(ConnID) of
		{Pid} ->
			gen_fsm:send_event(Pid, quit);
		undefined ->
			io:format("~p: connection: ~p not exsist, can't delete.\n", [?MODULE, ConnID])
	end,
	{reply, todo, State};
handle_call({create_conn, ConnCfg}, _From, State) ->
	{ok, Pid} = connfsm_relay:start(ConnCfg),
	{ConnID, _, _, _, _} = ConnCfg,
	put(ConnID, {Pid}),
	io:format("~p: Registered ~p for conn ~p\n", [?MODULE, Pid, ConnID]),
	{reply, ok, State};
handle_call(_Request, _From, State) ->
	io:format("~p: Don't know how to deal with call ~p\n", [?SERVER, _Request]),
    {reply, ok, State}.

handle_cast({up, FromAddr, WireFrame}, State) ->
	case get(WireFrame#wire_frame.conn_id) of
		{Pid} ->
			gen_fsm:send_event(Pid, {up, FromAddr, WireFrame});
		undefined ->
			io:format("Got msg to unknown connection id: ~p, drop it\n", [WireFrame#wire_frame.conn_id])
	end,
	{noreply, State};
handle_cast(_Msg, State) ->
	io:format("~p: Don't know how to deal with cast ~p\n", [?SERVER, _Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
	io:format("~p: Don't know how to deal with info ~p\n", [?SERVER, _Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

