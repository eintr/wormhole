-module(connfsm_control_server).
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

-export([init/1, control/2, control/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(State) ->
	io:format("~p: inited.\n", [?MODULE]),
	{ok, FecEncoderPid} = fec_encoder:start_link({?CONNID_CTRL, 2}),
	{ok, FecDecoderPid} = fec_decoder:start_link(),
	put(fec_encoder, FecEncoderPid),
	put(fec_decoder, FecDecoderPid),
	put(server_conn_id, 1),
	{ok, control, State}.

control({up, FromAddr, Msg}, State) ->
	msg_process(FromAddr, Msg),
	{next_state, control, State};
control(_Event, State) ->
	io:format("conn/control: Unknown event: ~p\n", [_Event]),
	{next_state, control, State}.

control(_Event, _From, State) ->
	io:format("Unknown event: ~p from ~p\n", [_Event, _From]),
    {reply, unknown_event, control, State}.

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
handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info({}, StateName, State) ->
    {next_state, StateName, State};
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
	io:format("~p: Pushing msg: ~p\n", [?MODULE, Msg]),
	{ok, WireBins} = gen_fsm:sync_send_event(get(fec_encoder), {encode_push, Msg}),
	lists:foreach(fun (W)->
						  gen_server:cast(transcvr_pool, {down, DstAddr, W})
				  end, WireBins),
	ok.

msg_process(FromAddr, Msg) ->
	case Msg#msg.code of
		?CODE_CHAP ->
			msg_process_chap(FromAddr, Msg);
		?CODE_ECHO ->
			%% TODO
			ok;
		_Code ->
			io:format("Control channel doesn't deal with msg ~p, dropped.\n", [_Code]),
			error
	end.

msg_process_chap(FromAddr, Msg) ->
	%{ConnID, LocalTunIP, PeerTunIP, RemoteAddr, ExtraRouteList}
	#msg_body_chap{
	   conn_id_client = ConnIdClient,
	   salt = Salt,
	   prefix = _Prefix,
	   md5 = MD5,
	   username = UserName} = Msg#msg.body,
	ConnID = msg:connid_combine(get(server_conn_id), ConnIdClient),
	LocalTunIP = {10,255,255,253},	% TODO: Fetch from config.
	A4 = get(server_conn_id) rem 256,
	A3 = (get(server_conn_id) rem 65536) div 256,
	PeerTunIP = {172,17,A3,A4},

	case gen_server:call(auth_server, {auth, {UserName, Salt, MD5}}) of
		{pass, _ExtraInfo} ->
			ok = gen_server:call(connection_pool, {create_conn, {ConnID, LocalTunIP, PeerTunIP, FromAddr, []}}),
			MsgConnect=#msg{code=?CODE_CHAP_CONNECT,
							body=#msg_body_connect{	conn_id_client=ConnIdClient, 
													conn_id_server=get(server_conn_id),
													server_tun_addr=LocalTunIP,
													client_tun_addr=PeerTunIP,
													route_prefixes=[]}},
			push_msg(FromAddr, MsgConnect),
			put(server_conn_id, get(server_conn_id)+1);
		{failed, Reason} ->
			io:format("Auth failed: {~p,~p,~p} => ~p\n", [UserName, Salt, MD5, Reason]),
			MsgReject=#msg{code=?CODE_CHAP_REJECT,
						   body=#msg_body_reject{	conn_id_client=ConnIdClient,reason= <<"CHAP Failed">>}},
			push_msg(FromAddr, MsgReject),
			put(server_conn_id, get(server_conn_id)+1)
	end,
	ok.

