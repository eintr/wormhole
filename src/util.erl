-module(util).
-export([system/1]).

system(Str) ->
	PidStr = lists:flatten(io_lib:format("~p", [self()])),
	PidSalt = string:strip(string:strip(PidStr, left, $<), right, $>),
	TimeSalt = string:strip(
				 string:strip(
				   lists:flatten(io_lib:format("~p", [os:timestamp()])),
				   left, ${),
				 right, $}),
	TmpFname = "/tmp/wormhole."++ os:getpid() ++ "." ++ PidSalt ++ TimeSalt,
	CMD = lists:flatten(Str) ++ " > " ++ TmpFname ++ " ; echo $?",
	Codeout = os:cmd(CMD),
	Code = list_to_integer(string:strip(Codeout, right, 10)),
	{ok, OutPutBin} = file:read_file(TmpFname),
	file:delete(TmpFname),
	{Code, binary:bin_to_list(OutPutBin)}.

timestamp_ms() ->
	{A,B,C} = os:timestamp(),
	A*1000000000 + B*1000 + C div 1000.

-ifdef(TEST).
-include("util_test.hrl").
-endif.

