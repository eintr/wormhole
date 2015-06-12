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
	{ok, Filename} = application:get_env(configfile),
	{ok, Config} = file:script(Filename),
	Salt = <<"TestSalt">>,
	{_, _U} = lists:keyfind(username, 1, Config),
	Username = binary:list_to_bin(_U),
	{_, _P} = lists:keyfind(password, 1, Config),
	Password = binary:list_to_bin(_P),
	{_, LocalNetPrefix} = lists:keyfind(localnet_prefix, 1, Config),
	{_, SAddr} = lists:keyfind(server_addr, 1, Config),
	{ok, ServerAddr} = inet:parse_ipv4_address(SAddr),
	{_, ServerPort} = lists:keyfind(server_port, 1, Config),
	ChapMsg = #msg{	connection_id=?CONNID_CTRL,
					code=?CODE_CHAP,
					body= #msg_body_chap{	salt=Salt,
											conn_id_client=10001,
											prefix=LocalNetPrefix,
											md5=crypto:hash(md5, <<Salt/binary, Password/binary>>),
											username=Username }},
	{ok, ChapMsgBin} = msg:encode(ChapMsg),
	gen_server:cast(fec_pool, {down_push, [{ServerAddr, ServerPort}], ChapMsgBin}),
	{ok, wait_chap_result, {{ServerAddr, ServerPort}, State}, 3000}.

wait_chap_result(timeout, {_Server, State}) ->
	io:format("~p: no chap response within 3 seconds.\n", [?MODULE]),
	{stop, "Auth timed out", State};
wait_chap_result({up, {ServerAddr, _}=FromAddr, Msg}, {{ServerAddr, _}, State}) ->
	case Msg#msg.code of
		?CODE_CHAP_CONNECT ->
			io:format("~p: CHAP success, create conn.\n", [?MODULE]),
			Body = Msg#msg.body,
			ConnID = msg:connid_combine(Body#msg_body_connect.conn_id_server, Body#msg_body_connect.conn_id_client),
			LocalIP = Body#msg_body_connect.client_tun_addr,
			PeerIP = Body#msg_body_connect.server_tun_addr,
			%{ConnID, LocalTunIP, PeerTunIP, RemoteAddr, ExtraRouteList}
			ok = gen_server:call(connection_pool, {create_conn, {ConnID, LocalIP, PeerIP, FromAddr, []}}),
			{next_state, loop, State};
		?CODE_CHAP_REJECT ->
			{stop, "Auth denied.", State};
		_ ->
			io:format("~p: Don't know how to deal msg ~p\n", [?MODULE, Msg]),
			{next_state, wait_chap_result, State}
	end;
wait_chap_result(_Msg, State) ->
	io:format("~p: Don't know how to deal with msg ~p\n", [?MODULE, _Msg]),
	{next_state, wait_chap_result, State}.

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

