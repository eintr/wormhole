-module(connection_pool).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("msg.hrl").
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

handle_call({create_conn, ConnID}, _From, State) ->
	{ok, Pid} = connfsm_relay:start(ConnID),
	put(ConnID, {Pid}),
	{reply, ok, State};
handle_call(_Request, _From, State) ->
	io:format("~p: Don't know how to deal with call ~p\n", [?SERVER, _Request]),
    {reply, ok, State}.

handle_cast({up, FromAddr, MsgBin}, State) ->
	{ok, Msg} = msg:decode(MsgBin),
	case get(Msg#msg.connection_id) of
		{Pid} ->
			gen_fsm:send_event(Pid, {up, FromAddr, Msg});
		undefined ->
			io:format("Got msg to unknown connection id: ~p\n", [Msg#msg.connection_id])
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

