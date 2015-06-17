-module(wormhole_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	AuthSvr = ?CHILD(auth_server, worker),
	TxRx = ?CHILD(transcvr_pool, worker),
	%FecPool = ?CHILD(fec_pool, worker),
	ConnPool = ?CHILD(connection_pool, worker),
    {ok, { {one_for_one, 5, 10}, [AuthSvr, TxRx, ConnPool]} }.

