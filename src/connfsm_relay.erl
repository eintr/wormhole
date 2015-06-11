-module(connfsm_relay).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include("msg.hrl").
-include("protocol.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1, start_link/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, relay/2, relay/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(ConnID) ->
    gen_fsm:start({local, ?SERVER}, ?MODULE, [ConnID], []).
start_link(ConnID) ->
    gen_fsm:start({local, ?SERVER}, ?MODULE, [ConnID], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([ConnID]) ->
	{ok, TunPID} = tuncer:create("", [tun, {active, true}]),
	put(tun_ifname, binary:bin_to_list(tuncer:devname(TunPID))),
	put(peeraddr, []),
    {ok, relay, {ConnID, TunPID}}.

relay({up, FromAddr, Msg}, {_ConnID, TunPID}=State) ->
	case Msg#msg.code of
		?CODE_DATA ->
			case lists:member(FromAddr, get(peeraddr)) of
				true -> null;
				false -> put(peeraddr, get(peeraddr)++[FromAddr])
			end	,
			tuncer:send(TunPID, Msg#msg.body#msg_body_data.data),
	    	{next_state, relay, State};
		_Unknown ->
			io:format("Don't know how to deal with msg code ~p\n", [_Unknown]),
	    	{next_state, relay, State}
	end;
relay(_Event, State) ->
	io:format("conn/relay: Unknown event: ~p\n", [_Event]),
    {next_state, relay, State}.

relay(stat, _From, State) ->
	{reply, todo, relay, State};
relay(_Event, _From, State) ->
	io:format("Unknown event: ~p from ~p\n", [_Event, _From]),
    {reply, ok, relay, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info({tuntap, TunPID, TunPktBin}, relay, {ConnID, TunPID}=State) ->
	Msg = #msg{connection_id = ConnID, code=?CODE_DATA, body=#msg_body_data{data=TunPktBin}},
	gen_server:cast(fec_pool, {down, get(peeraddr), Msg}),
    {next_state, relay, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, {_ConnID, TunPID}) ->
	ok = tuncer:close(TunPID),
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

