-include_lib("eunit/include/eunit.hrl").

fec_info_decode_test() ->
	B = <<0,0,1,1,2>>,
	F = #fec_info{fecg_id=1, fec_seq=1, fec_gsize=2},
	R = fec_frame:info_decode(B),
	?assert(R =:= F).
 
fec_info_encode_test() ->
	B = <<0,0,1,1,2>>,
	F = #fec_info{fecg_id=1, fec_seq=1, fec_gsize=2},
	R = fec_frame:info_encode(F),
	?assert(R =:= B).
 
fec_frame_decode_test() ->
	B = <<	0,0,1,1,2,0,0,0,0,0,0,0,0,1,0,0,39,17,84,
		101,115,116,83,97,108,116,10,0,0,0,8,15,193,
		39,116,254,54,244,11,42,111,142,209,10,192,
		121,64,117,115,101,114,49>>,
	F = #fec_frame{	fec_info=#fec_info{fecg_id=1, fec_seq=1, fec_gsize=2},
			payload = <<	0,0,0,0,0,0,0,0,1,0,0,39,17,84,
					101,115,116,83,97,108,116,10,0,0,0,8,15,193,
					39,116,254,54,244,11,42,111,142,209,10,192,
					121,64,117,115,101,114,49>>},
	{ok, R} = fec_frame:decode(B),
	?assert(R =:= F).

 
fec_frame_encode_test() ->
	B = <<	0,0,1,1,2,0,0,0,0,0,0,0,0,1,0,0,39,17,84,
		101,115,116,83,97,108,116,10,0,0,0,8,15,193,
		39,116,254,54,244,11,42,111,142,209,10,192,
		121,64,117,115,101,114,49>>,
	F = #fec_frame{	fec_info=#fec_info{fecg_id=1, fec_seq=1, fec_gsize=2},
			payload = <<	0,0,0,0,0,0,0,0,1,0,0,39,17,84,
					101,115,116,83,97,108,116,10,0,0,0,8,15,193,
					39,116,254,54,244,11,42,111,142,209,10,192,
					121,64,117,115,101,114,49>>},
	{ok, R} = fec_frame:encode(F),
	?assert(R =:= B).

