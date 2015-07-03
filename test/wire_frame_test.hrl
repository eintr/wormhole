-include_lib("eunit/include/eunit.hrl").

fec_info_decode_test() ->
	B = <<
		  1:24/unsigned-big-integer,
		  1:8/unsigned-big-integer,
		  2:8/unsigned-big-integer,
		  37:16/unsigned-big-integer>>,
	F = #fec_info{fecg_id=1, fec_seq=1, fec_gsize=2, fec_payload_size=37},
	R = wire_frame:info_decode(B),
	?assert(R =:= F).
 
fec_info_encode_test() ->
	B = <<
		  1:24/unsigned-big-integer,
		  1:8/unsigned-big-integer,
		  2:8/unsigned-big-integer,
		  37:16/unsigned-big-integer>>,
	F = #fec_info{fecg_id=1, fec_seq=1, fec_gsize=2, fec_payload_size=37},
	R = wire_frame:info_encode(F),
	?assert(R =:= B).
 
wire_frame_decode_test() ->
	B = <<	17:64/unsigned-big-integer,
			0,0,1,1,2,0,37,
			0,0,0,0,0,0,0,0,1,0,0,39,17,84,
			101,115,116,83,97,108,116,10,0,0,0,8,15,193,
			39,116,254,54,244,11,42,111,142,209,10,192,
			121,64,117,115,101,114,49>>,
	F = #wire_frame{
		   conn_id=17,
		   fec_info=#fec_info{fecg_id=1, fec_seq=1, fec_gsize=2, fec_payload_size=37},
		   payload_cipher = <<	0,0,0,0,0,0,0,0,1,0,0,39,17,84,
								101,115,116,83,97,108,116,10,0,0,0,8,15,193,
								39,116,254,54,244,11,42,111,142,209,10,192,
								121,64,117,115,101,114,49>>},
	{ok, R} = wire_frame:decode(B),
	?assert(R =:= F).

 
wire_frame_encode_test() ->
	B = <<	17:64/unsigned-big-integer,
			0,0,1,1,2,0,37,
			0,0,0,0,0,0,0,0,1,0,0,39,17,84,
			101,115,116,83,97,108,116,10,0,0,0,8,15,193,
			39,116,254,54,244,11,42,111,142,209,10,192,
			121,64,117,115,101,114,49>>,
	F = #wire_frame{
		   conn_id=17,
		   fec_info=#fec_info{fecg_id=1, fec_seq=1, fec_gsize=2, fec_payload_size=37},
		   payload_cipher = <<	0,0,0,0,0,0,0,0,1,0,0,39,17,84,
								101,115,116,83,97,108,116,10,0,0,0,8,15,193,
								39,116,254,54,244,11,42,111,142,209,10,192,
								121,64,117,115,101,114,49>>},
	{ok, R} = wire_frame:encode(F),
	?assert(R =:= B).

