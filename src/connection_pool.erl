-module(connection_pool).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("msg.hrl").

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

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({create_conn, {Connid}}, _From, State) ->
	{ok, Pid} = connfsm_relay:start(Connid),
	put(Connid, {Pid}),
	{reply, ok, State};
handle_call(_Request, _From, State) ->
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

