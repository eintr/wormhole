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

chap_encode_test() ->
	M = #msg{ connection_id=?CONNID_CTRL,
		  code=?CODE_CHAP,
		  body= #msg_body_chap{   salt = <<"SALTSALT">>,
								  conn_id_client = 1,
								  prefix = "10.0.0.0/8",
								  md5 = binary:copy(<<"D">>, 16),
								  username = <<"user1">> }},
	{ok, B} = msg:encode(M),
	TARGET = <<	?CONNID_CTRL:64/unsigned-big-integer,
				?CODE_CHAP:8/unsigned-big-integer,
				1:32/unsigned-big-integer,
				<<"SALTSALT">>/binary,
				<<10, 0, 0, 0, 8>>/binary,
				(binary:copy(<<"D">>, 16))/binary,
				<<"user1">>/binary
			 >>,
	?assert( B =:= TARGET).

chap_decode_test() ->
	M = #msg{ connection_id=?CONNID_CTRL,
		  code=?CODE_CHAP,
		  body= #msg_body_chap{   salt = <<"SALTSALT">>,
								  conn_id_client = 1,
								  prefix = "10.0.0.0/8",
								  md5 = binary:copy(<<"D">>, 16),
								  username = <<"user1">> }},
	B = <<	?CONNID_CTRL:64/unsigned-big-integer,
				?CODE_CHAP:8/unsigned-big-integer,
				1:32/unsigned-big-integer,
				<<"SALTSALT">>/binary,
				<<10, 0, 0, 0, 8>>/binary,
				(binary:copy(<<"D">>, 16))/binary,
				<<"user1">>/binary
			 >>,
	{ok, R} = msg:decode(B),
	?assert( M =:= R).

connid_basic_test() ->
	?assert(connid_split(16#100000002) =:= {1, 2}),
	?assert(msg:connid_combine({1, 5}) =:= 16#100000005),
	?assert(msg:connid_split(connid_combine({1234,5678})) =:= {1234,5678}).
 
%simple_test() ->
%	    ok = application:start(wormhole),
%		    ?assertNot(undefined =:= whereis(wormhole_sup)).

