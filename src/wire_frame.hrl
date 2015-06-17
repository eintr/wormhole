
-define(GIDMAX, 16#ffffff).

-record(fec_info, {
		  fecg_id,
		  fec_seq,
		  fec_gsize,
		  fec_payload_size
		 }).

-record(wire_frame, {
		  conn_id,
		  fec_info,
		  payload_cipher
		 }).

