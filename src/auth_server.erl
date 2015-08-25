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
-export([chap_digest/2]).

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
handle_call({auth, {Username, Salt, MD5}}, _From, {AccountList}) ->
	{reply, do_chap_auth(Username, Salt, MD5, AccountList), {AccountList}};
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
	case lists:keyfind(accounts, 1, Config) of
		{accounts, AccountList} -> AccountList;
		false ->
			io:format("~p: AccountList not found.\n", [?MODULE]),
			[]
	end.

do_chap_auth(Username, Salt, MD5, AccountList) ->
	case lists:keyfind(Username, 1, AccountList) of
		{Username, PlainPassword, UserInfo} ->
			LocalMD5 = chap_digest(Salt, PlainPassword),
			if
				LocalMD5 =:= MD5 ->
					{pass, UserInfo};
				true ->
					{failed, "Wrong password"}
			end;
		false ->
			{failed, "User not found"}
	end.

chap_digest(_Salt, _PlainPassword) ->
	<<"Fake_MD5_Digest.">>.

