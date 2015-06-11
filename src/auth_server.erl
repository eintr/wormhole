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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
	{ok, Filename} = application:get_env(configfile),
	io:format("~p is initting, with ~p\n", [?MODULE, Filename]),
    {ok, {load_accounts(Filename)}}.

handle_call({reload_conf, Filename}, _From, _) ->
	{reply, ok, {load_accounts(Filename)}};
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
load_accounts(Filename) ->
	{ok, Config} = file:script(Filename),
	{accounts, AccountList} = lists:keyfind(accounts, 1, Config),
	AccountList.

