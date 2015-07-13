-include_lib("eunit/include/eunit.hrl").

fecgid_next_test() ->
	Next_of_0 = fec:next_gid(0),
	?assert( Next_of_0 =:= 1 ),
	Next_of_1 = fec:next_gid(1),
	?assert( Next_of_1 =:= 2 ),
	Next_of_1234 = fec:next_gid(1234),
	?assert( Next_of_1234 =:= 1235 ),
	Next_of_GID_MAX = fec:next_gid(16#ffffff),
	?assert( Next_of_GID_MAX =:= 0 ).

delta_gid_test() ->
	?assert(fec:delta_gid(63453, 63451) =:= 2),
	?assert(fec:delta_gid(0, 16#ffffff) =:= 1),
	?assert(fec:delta_gid(16#ffffff, 0) =:= -1),
	?assert(fec:delta_gid(8765, 7654) =:= 1111),
	?assert(fec:delta_gid(7654, 8765) =:= -1111).

bin_xor_test() ->
	?assert(fec:bin_xor(<<0,0,0>>, <<"abc">>) =:= <<"abc">> ),
	?assert(fec:bin_xor(<<>>, <<"abc">>) =:= <<"abc">> ),
	?assert(fec:bin_xor(<<"abc">>, <<>>) =:= <<"abc">> ),
	?assert(fec:bin_xor(<<"abc">>, <<"abc">>) =:= <<0:24/unsigned-integer>> ),
	?assert(fec:bin_xor(<<"abc">>, <<"abcd">>) =:= <<0:24/unsigned-integer, "d">> ).

encode_w2_1_test() ->
	put(encode_context, #fec_encode_context{suggest_width=2, next_gid=1234}),
	put(conn_id, 12345),
	Msg = <<"HelloHelloHelloHelloHelloHello">>,
	F1 = #wire_frame{
		   conn_id = 12345,
		   fec_info = #fec_info{
						fecg_id = 1234,
						fec_seq = 1,
						fec_gsize = 2,
						fec_payload_size = 15	},
		   payload_cipher = <<"HelloHelloHelloHelloHelloHello">>},
	FP = #wire_frame{
		   conn_id = 12345,
		   fec_info = #fec_info{
						fecg_id = 1234,
						fec_seq = 0,
						fec_gsize = 2,
						fec_payload_size = 15	},
		   payload_cipher = <<"HelloHelloHelloHelloHelloHello">>},
	Res = fec:encode(Msg, 15),
	?assert(Res =:= {ok, [F1, FP]}),
	erase(conn_id),
	erase(encode_context).

encode_w3_1_test() ->
	put(encode_context, #fec_encode_context{suggest_width=3, next_gid=1234}),
	put(conn_id, 12345),
	Msg = <<"HelloHelloHelloHelloHelloHello">>,
	F1 = #wire_frame{
		   conn_id = 12345,
		   fec_info = #fec_info{
						fecg_id = 1234,
						fec_seq = 1,
						fec_gsize = 3,
						fec_payload_size = 15	},
		   payload_cipher = <<"HelloHelloHelloHelloHelloHello">>},
	F2 = #wire_frame{
		   conn_id = 12345,
		   fec_info = #fec_info{
						fecg_id = 1234,
						fec_seq = 2,
						fec_gsize = 3,
						fec_payload_size = 15	},
		   payload_cipher = <<"HelloHelloHelloHelloHelloHello">>},
	FP = #wire_frame{
		   conn_id = 12345,
		   fec_info = #fec_info{
						fecg_id = 1234,
						fec_seq = 0,
						fec_gsize = 3,
						fec_payload_size = 30	},
		   payload_cipher = <<0:240/integer>>},
	Res1 = fec:encode(Msg, 15),
	Res2 = fec:encode(Msg, 15),
	?assert(Res1 =:= need_more),
	?assert(Res2 =:= {ok, [F1, F2, FP]}).

decode_w2_1_test() ->
	Msg = <<"HelloHelloHello">>,
	F = #wire_frame{
		   conn_id = 12345,
		   fec_info = #fec_info{
						fecg_id = 1234,
						fec_seq = 1,
						fec_gsize = 2,
						fec_payload_size = 15	},
		   payload_cipher = <<"HelloHelloHelloHelloHelloHello">>},
	put({decode_context, 1234}, #fec_decode_context{
								   id = 1234,
								   width = 2,
								   timestamp = 2000000000,
								   pool=[]}
	   ),
	R = fec:decode(F),
	erase({decode_context, 1234}),
	%?debugVal(R),
	?assert(R =:= {ok, [Msg]}).

decode_w2_2_test() ->
	Msg = <<"HelloHelloHello">>,
	F = #wire_frame{
		   conn_id = 12345,
		   fec_info = #fec_info{
						fecg_id = 1234,
						fec_seq = 0,
						fec_gsize = 2,
						fec_payload_size = 15	},
		   payload_cipher = <<"HelloHelloHelloHelloHelloHello">>},
	put({decode_context, 1234}, #fec_decode_context{
								   id = 1234,
								   width = 2,
								   timestamp = 2000000000,
								   pool=[]}
	   ),
	R = fec:decode(F),
	erase({decode_context, 1234}),
	%?debugVal(R),
	?assert(R =:= {ok, [Msg]}).

decode_w3_1_test() ->
	put({decode_context, 1234}, #fec_decode_context{
								   id = 1234,
								   width = 3,
								   timestamp = 2000000000,
								   pool=[]}
	   ),
	Msg1 = <<"HelloHelloHello">>,
	F1 = #wire_frame{
		   conn_id = 123456,
		   fec_info = #fec_info{
						fecg_id = 1234,
						fec_seq = 1,
						fec_gsize = 3,
						fec_payload_size = 15	},
		   payload_cipher = <<"HelloHelloHelloHelloHelloHello">>},
	Msg2 = <<"HelloHello">>,
	F2 = #wire_frame{
		   conn_id = 123456,
		   fec_info = #fec_info{
						fecg_id = 1234,
						fec_seq = 2,
						fec_gsize = 3,
						fec_payload_size = 10	},
		   payload_cipher = <<"HelloHelloHelloHelloHelloHello">>},
	?assert(fec:decode(F1) =:= need_more),
	?assert(fec:decode(F2) =:= {ok, [Msg1, Msg2]}),
	erase({decode_context, 1234}).

decode_w3_2_test() ->
	put({decode_context, 1234}, #fec_decode_context{
								   id = 1234,
								   width = 3,
								   timestamp = 2000000000,
								   pool=[]}
	   ),

	Msg1 = <<"HelloHelloHello">>,
	Msg2 = <<"HelloHello">>,

	F1 = #wire_frame{
		   conn_id = 123456,
		   fec_info = #fec_info{
						fecg_id = 1234,
						fec_seq = 1,
						fec_gsize = 3,
						fec_payload_size = 15	},
		   payload_cipher = Msg1},
	F0 = #wire_frame{
		   conn_id = 123456,
		   fec_info = #fec_info{
						fecg_id = 1234,
						fec_seq = 0,
						fec_gsize = 3,
						fec_payload_size = 25	},
		   payload_cipher = <<0,0,0,0,0,0,0,0,0,0, "Hello">>},
	?assert(fec:decode(F1) =:= need_more),
	?assert(fec:decode(F0) =:= {ok, [Msg1, Msg2]}),
	erase({decode_context, 1234}).

