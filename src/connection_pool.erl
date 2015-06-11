-module(connection_pool).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("msg.hrl").
-include("protocol.hrl").

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

start_link(Flags) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Flags], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Flags]) ->
	{ok, Pid} = connfsm_control:start_link(?CONNID_CTRL),
	put(?CONNID_CTRL, {Pid}),
    {ok, Flags}.

handle_call({create_conn, ConnCfg}, _From, State) ->
	{ok, Pid} = connfsm_relay:start(ConnCfg),
	put(msg:connid_combine(ConnCfg#msg_body_connect.conn_id_server, ConnCfg#msg_body_connect.conn_id_client), {Pid}),
	{reply, ok, State};
handle_call(_Request, _From, State) ->
	io:format("~p: Don't know how to deal with call ~p\n", [?SERVER, _Request]),
    {reply, ok, State}.

handle_cast({up, Msgs}, State) ->
	lists:foreach(fun ({FromAddr, MsgBin})->
						  Msg = msg:decode(MsgBin),
						  case get(Msg#msg.connection_id) of
							  {Pid} ->
								  gen_fsm:event(Pid, {up, FromAddr, Msg});
							  undefined ->
								  io:format("Got msg to unknown connection id: ~p\n", [Msg#msg.connection_id])
						  end
				  end, Msgs),
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

