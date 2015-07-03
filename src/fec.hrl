-record(fec_encode_context, {	next_gid=1,
								intlv_level=1,
								suggest_width=2,
								intlv_pool=[]	}).

-record(fecg_encode_context, {	gid=1,
								width=2,
								party_frame={<<>>, 0},
								pool=[]	}).

-record(fec_decode_context, {id, width, timestamp, pool}).

