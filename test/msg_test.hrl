-include_lib("eunit/include/eunit.hrl").
 
data_decode_test() ->
	?assert(msg:encode(#msg{connection_id=16#100000002,
							code=?CODE_DATA,
							body=#msg_body_data{data = <<16#1, 16#2, 16#3, 16#4, 16#5, 16#6, 16#7>>}})
			=:=
			{ok, <<16#00, 16#00, 16#00, 16#01, 16#00, 16#00, 16#00, 16#02, 16#00, 16#1, 16#2, 16#3, 16#4, 16#5, 16#6, 16#7>>}).

data_encode_test() ->
	?assert(msg:decode(<<16#00, 16#00, 16#00, 16#01, 16#00, 16#00, 16#00, 16#02, 16#00, 16#1, 16#2, 16#3, 16#4, 16#5, 16#6, 16#7>>)
			=:=
			{ok, #msg{connection_id=16#100000002,
							code=?CODE_DATA,
							body=#msg_body_data{data = <<16#1, 16#2, 16#3, 16#4, 16#5, 16#6, 16#7>>}}}).

connid_basic_test() ->
	?assert(connid_split(16#100000002) =:= {1, 2}),
	?assert(msg:connid_combine({1, 5}) =:= 16#100000005),
	?assert(msg:connid_split(connid_combine({1234,5678})) =:= {1234,5678}).
 
%simple_test() ->
%	    ok = application:start(wormhole),
%		    ?assertNot(undefined =:= whereis(wormhole_sup)).

