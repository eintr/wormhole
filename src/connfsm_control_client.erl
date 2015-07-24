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

init([]) ->
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

	{ok, FecEncoderPid} = fec_encoder:start_link({?CONNID_CTRL, 2}),
	put(fec_encoder, FecEncoderPid),
	{ok, FecDecoderPid} = fec_decoder:start_link(),
	put(fec_decoder, FecDecoderPid),

	ChapMsg = #msg{	code=?CODE_CHAP,
					body= #msg_body_chap{	salt=Salt,
											conn_id_client=10001,
											prefix=LocalNetPrefix,
											md5=auth_server:chap_digest(Salt, Password),
											username=Username }},
	push_msg({ServerAddr, ServerPort}, ChapMsg),
	{ok, wait_chap_result, {{ServerAddr, ServerPort}, {FecEncoderPid, FecDecoderPid}}, 3141}.

wait_chap_result(timeout, {_Server, State}) ->
	io:format("~p: no chap response within 3.141 seconds.\n", [?MODULE]),
	{stop, "Auth timed out", State};
wait_chap_result({up, FromAddr, Msg}, State) ->
	case chap_result(FromAddr, Msg) of
		connected -> {next_state, loop, State};
		rejected -> {stop, "Chap auth rejected."};
		ignore -> {next_state, wait_chap_result, State}
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

handle_event({up, FromAddr, WireFrame}, StateName, State) ->
	case gen_fsm:sync_send_event(get(fec_decoder), {FromAddr, WireFrame}) of
		{ok, Msgs} ->
			lists:foreach(fun (Msg)->
								  gen_fsm:send_event(self(), {up, FromAddr, Msg})
						  end, Msgs),
			{next_state, StateName, State};
		pass ->
			{next_state, StateName, State};
		_Result ->
			io:format("~p: Unknown decode result: ~p\n", [?MODULE, _Result]),
			{next_state, StateName, State}
	end;
handle_event(Event, StateName, State) ->
	io:format("~p: Don't know how to process all_state_event: ~p\n", [?MODULE, Event]),
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
push_msg(DstAddr, Msg) ->
	%{ok, MsgBin} = msg:encode(Msg),
	%io:format("~p: Pushing msg: ~p\n", [?MODULE, Msg]),
	{ok, WireBins} = gen_fsm:sync_send_event(get(fec_encoder), {encode_push, Msg}),
	%io:format("~p: encode_push() got reply:~p\n", [?MODULE, {ok, WireBins}]),
	lists:foreach(fun (W)-> 
						  gen_server:cast(transcvr_pool, {down, DstAddr, W})
				  end, WireBins),
	ok.

chap_result(FromAddr, Msg) ->
	case Msg#msg.code of
		?CODE_CHAP_CONNECT ->
			io:format("~p: CHAP success, create conn.\n", [?MODULE]),
			Body = Msg#msg.body,
			ConnID = msg:connid_combine(Body#msg_body_connect.conn_id_server, Body#msg_body_connect.conn_id_client),
			LocalIP = Body#msg_body_connect.client_tun_addr,
			PeerIP = Body#msg_body_connect.server_tun_addr,
			%{ConnID, LocalTunIP, PeerTunIP, RemoteAddr, ExtraRouteList}
			ok = gen_server:call(connection_pool, {create_conn, {ConnID, LocalIP, PeerIP, FromAddr, []}}),
			connected;
		?CODE_CHAP_REJECT ->
			{rejected, "Reason"};
		_ ->
			io:format("~p: Don't know how to deal ~p while expecting chap result.\n", [?MODULE, Msg]),
			ignore
	end.
