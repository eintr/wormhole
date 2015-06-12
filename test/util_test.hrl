-include_lib("eunit/include/eunit.hrl").

systemcall_case1_test() ->
	Res = util:system("echo -n hello"),
	?assert( Res =:= {0, "hello"} ).

systemcall_case2_test() ->
	Res = util:system("test 1 -eq 2"),
	?assert( Res =:= {1, ""} ).

systemcall_case3_test() ->
	Res = util:system("bash -c 'exit 123'"),
	?assert( Res =:= {123, ""} ).

