-record(fec_info, {
		  fecg_id,
		  fec_seq,
		  fec_gsize
		 }).

-record(fec_frame, {
		  fec_info,
		  payload
		 }).

