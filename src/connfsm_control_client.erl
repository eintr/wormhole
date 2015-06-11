-module(connfsm_control_client).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include("msg.hrl").
-include("protocol.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
		 terminate/3, code_change/4]).
-export([wait_chap_result/2, loop/2, loop/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(State) ->
	Salt = <<"TestSalt">>,
	Username = <<"Username">>,
	Password = <<"password">>,
	ServerAddr = {107,161,16,30},
	ServerPort = 60000,
	ChapMsg = #msg{	connection_id=?CONNID_CTRL,
					code=?CODE_CHAP,
					body= #msg_body_chap{	salt=Salt,
											prefix="10.0.0.0/8",
											md5=crypto:hash(md5, <<Salt/binary, Password/binary>>),
											username=Username }},
	gen_server:cast(fec_pool, {down_push, [{ServerAddr, ServerPort}], msg:encode(ChapMsg)}),
	{ok, wait_chap_result, {{ServerAddr, ServerPort}, State}, 3000}.

wait_chap_result(timeout, {_Server, State}) ->
	io:format("~p: no chap response within 3 seconds.\n", [?MODULE]),
	{stop, "Auth timed out", State};
wait_chap_result({up, {ServerAddr, _}, Msg}, {{ServerAddr, _}, State}) ->
	case Msg#msg.code of
		?CODE_CHAP_CONNECT ->
			ok = gen_server:call(connection_pool, {create_conn, {Msg#msg.body}}),
			{next_state, loop, State};
		?CODE_CHAP_REJECT ->
			{stop, "Auth denied.", State};
		_ ->
			{next_state, wait_chap_result, State}
	end.

loop({up, _FromAddr, Msg}, State) ->
	case Msg#msg.code of
		?CODE_ECHO ->
			io:format("CODE_ECHO is not implemented yet, dropped.\n");
		_Code ->
			io:format("Client control channel doesn't deal with msg ~p, dropped.\n", [_Code])
	end,
	{next_state, loop, State};
loop(_Event, State) ->
	io:format("conn/control: Unknown event: ~p\n", [_Event]),
	{next_state, loop, State}.

loop(_Event, _From, State) ->
	io:format("Unknown event: ~p from ~p\n", [_Event, _From]),
    {reply, unknown_event, loop, State}.

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

