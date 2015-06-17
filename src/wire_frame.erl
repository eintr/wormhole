-module(wire_frame).

-include("wire_frame.hrl").

-export([encode/1, decode/1, info_encode/1, info_decode/1]).

info_decode(FecBin) ->
	<<  Fecgid:24/unsigned-big-integer,
		Fecseq:8/unsigned-big-integer,
		Fecgsize:8/unsigned-big-integer,
		FecPayloadSize:16/unsigned-big-integer	>> = FecBin,
	#fec_info{fecg_id=Fecgid, fec_seq=Fecseq, fec_gsize=Fecgsize, fec_payload_size=FecPayloadSize}.

info_encode(I) ->
	<<    (I#fec_info.fecg_id):24/unsigned-big-integer,
		  (I#fec_info.fec_seq):8/unsigned-big-integer,
		  (I#fec_info.fec_gsize):8/unsigned-big-integer,
		  (I#fec_info.fec_payload_size):16/unsigned-big-integer >>.

decode(Packet) ->
	<<  ConnID:64/unsigned-big-integer, Fec:7/binary, PayLoad/binary >> = Packet,
	{ok, #wire_frame{
			conn_id = ConnID,
			fec_info = info_decode(Fec),
			payload_cipher = PayLoad }}.

encode(F) ->
	FecInfo = info_encode(F#wire_frame.fec_info),
	Bin = <<    (F#wire_frame.conn_id):64/unsigned-big-integer,
				FecInfo/binary,
				(F#wire_frame.payload_cipher)/binary >>,
	{ok, Bin}.

-ifdef(TEST).
-include("wire_frame_test.hrl").
-endif.

