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
	{ok, FecEncoderPid} = fec_encoder:start_link({?CONNID_CTRL}),
	{ok, FecDecoderPid} = fec_decoder:start_link(),
	put(fec_encoder, FecEncoderPid),
	put(fec_decoder, FecDecoderPid),
	put(server_conn_id, 1),
	{ok, control, State}.

control({up, FromAddr, WireFrame}, State) ->
	case gen_fsm:sync_send_event(get(fec_decoder), {FromAddr, WireFrame}) of
		{ok, Msgs} -> lists:foreach(fun (M)-> msg_process(FromAddr, M) end, Msgs);
		pass -> ok;
		_Result ->
			io:format("~p: Unknown decode result from fec_decoder: ~p\n", [?MODULE, _Result]),
			error
	end,
	{next_state, control, State};
control(_Event, State) ->
	io:format("conn/control: Unknown event: ~p\n", [_Event]),
	{next_state, control, State}.

control(_Event, _From, State) ->
	io:format("Unknown event: ~p from ~p\n", [_Event, _From]),
    {reply, unknown_event, control, State}.

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
	%% TODO: Do the real auth!
	%{ConnID, LocalTunIP, PeerTunIP, RemoteAddr, ExtraRouteList}
	MsgChap = Msg#msg.body,
	ConnID = msg:connid_combine(get(server_conn_id), MsgChap#msg_body_chap.conn_id_client),
	LocalTunIP = {10,255,255,253},
	A4 = get(server_conn_id) rem 256,
	A3 = (get(server_conn_id) rem 65536) div 256,
	PeerTunIP = {172,17,A3,A4},

	ok = gen_server:call(connection_pool, {create_conn, {ConnID, LocalTunIP, PeerTunIP, FromAddr, []}}),
	MsgConnect=#msg{code=?CODE_CHAP_CONNECT,
					body=#msg_body_connect{	conn_id_client=MsgChap#msg_body_chap.conn_id_client, 
											conn_id_server=get(server_conn_id),
											server_tun_addr=LocalTunIP,
											client_tun_addr=PeerTunIP,
											route_prefixes=[]}},
	push_msg(FromAddr, MsgConnect),
	put(server_conn_id, get(server_conn_id)+1),
	ok.

