-include_lib("eunit/include/eunit.hrl").
 
data_decode_test() ->
	M = #msg{
			 code=?CODE_DATA,
			 body=#msg_body_data{data = <<"This is a test content.">>}},
	B = <<?CODE_DATA:8/unsigned-big-integer, <<"This is a test content.">>/binary>>,
	{ok, R} = msg:encode(M),
	?assert( R =:= B ).

data_encode_test() ->
	M = #msg{
			 code=?CODE_DATA,
			 body=#msg_body_data{data = <<"This is a test content.">>}},
	B = <<?CODE_DATA:8/unsigned-big-integer, <<"This is a test content.">>/binary>>,
	{ok, R} = msg:decode(B),
	?assert( R =:= M ).

chap_encode_test() ->
	M = #msg{
		  code=?CODE_CHAP,
		  body= #msg_body_chap{   salt = <<"SALTSALT">>,
								  conn_id_client = 1,
								  prefix = "10.0.0.0/8",
								  md5 = binary:copy(<<"D">>, 16),
								  username = <<"user1">> }},
	{ok, B} = msg:encode(M),
	TARGET = <<
				?CODE_CHAP:8/unsigned-big-integer,
				1:32/unsigned-big-integer,
				<<"SALTSALT">>/binary,
				<<10, 0, 0, 0, 8>>/binary,
				(binary:copy(<<"D">>, 16))/binary,
				<<"user1">>/binary
			 >>,
	?assert( B =:= TARGET).

chap_decode_test() ->
	M = #msg{
		  code=?CODE_CHAP,
		  body= #msg_body_chap{   salt = <<"SALTSALT">>,
								  conn_id_client = 1,
								  prefix = "10.0.0.0/8",
								  md5 = binary:copy(<<"D">>, 16),
								  username = <<"user1">> }},
	B = <<
				?CODE_CHAP:8/unsigned-big-integer,
				1:32/unsigned-big-integer,
				<<"SALTSALT">>/binary,
				<<10, 0, 0, 0, 8>>/binary,
				(binary:copy(<<"D">>, 16))/binary,
				<<"user1">>/binary
			 >>,
	{ok, R} = msg:decode(B),
	?assert( M =:= R).

connect_encode_test() ->
	M = #msg{
				code=?CODE_CHAP_CONNECT,
				body = #msg_body_connect{	conn_id_client = 12345,
											conn_id_server = 23456,
											server_tun_addr= {1,2,3,4},
											client_tun_addr = {2,3,4,5},
											route_prefixes=[]}},
	B = <<
			?CODE_CHAP_CONNECT:8/unsigned-big-integer,
			12345:32/unsigned-big-integer,
			23456:32/unsigned-big-integer,
			<<1,2,3,4>>/binary,
			<<2,3,4,5>>/binary
		>>,
	{ok, R} = msg:encode(M),
	?assert( B =:= R ).

connect_decode_test() ->
	M = #msg{
				code=?CODE_CHAP_CONNECT,
				body = #msg_body_connect{	conn_id_client = 12345,
											conn_id_server = 23456,
											server_tun_addr= {1,2,3,4},
											client_tun_addr = {2,3,4,5},
											route_prefixes=[]}},
	B = <<
			?CODE_CHAP_CONNECT:8/unsigned-big-integer,
			12345:32/unsigned-big-integer,
			23456:32/unsigned-big-integer,
			<<1,2,3,4>>/binary,
			<<2,3,4,5>>/binary
		>>,
	{ok, R} = msg:decode(B),
	?assert( M =:= R ).

reject_codec_test() ->
	M = #msg{
				code=?CODE_CHAP_REJECT,
				body = #msg_body_reject{	conn_id_client = 12345,
											reason = <<"hello">>}},
	B = <<
			?CODE_CHAP_REJECT:8/unsigned-big-integer,
			12345:32/unsigned-big-integer,
			<<"hello">>/binary
		>>,
	{ok, R1} = msg:encode(M),
	{ok, R2} = msg:decode(B),
	?assert( B =:= R1 ),
	?assert( M =:= R2).

connid_basic_test() ->
	?assert(connid_split(16#100000002) =:= {1, 2}),
	?assert(msg:connid_combine(1, 5) =:= 16#100000005),
	?assert(msg:connid_split(msg:connid_combine(1234,5678)) =:= {1234,5678}).
 
%simple_test() ->
%	    ok = application:start(wormhole),
%		    ?assertNot(undefined =:= whereis(wormhole_sup)).

