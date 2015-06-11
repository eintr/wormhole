-module(auth_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [[{"testuser", "123qweasd", {}}]], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([AccountList]) ->
    {ok, {AccountList}}.

handle_call({auth, {Username, _Salt, _MD5}}, _From, {AccountList}) ->
	case lists:keyfind(Username, 1, AccountList) of
		{ok, {Username, _Password, UserInfo}} ->
			%% TODO: Do the real auth.
			{reply, {pass, UserInfo}, {AccountList}};
		{failed, Reason} ->
			{reply, {failed, Reason}, {AccountList}}
	end;
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

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

