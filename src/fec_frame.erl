-module(fec_frame).

-include("fec_frame.hrl").

-export([encode/1, decode/1, info_encode/1, info_decode/1]).

info_decode(FecBin) ->
	<<  Fecgid:24/big-integer,
		Fecseq:8/big-integer,
		Fecgsize:8/big-integer >> = FecBin,
	#fec_info{fecg_id=Fecgid, fec_seq=Fecseq, fec_gsize=Fecgsize}.

info_encode(I) ->
	<<    (I#fec_info.fecg_id):24/big-integer,
		  (I#fec_info.fec_seq):8/big-integer,
		  (I#fec_info.fec_gsize):8/big-integer >>.

decode(Packet) ->
	<<  Fec:5/binary, PayLoad/binary >> = Packet,
	{ok, #fec_frame{
			fec_info=info_decode(Fec),
			payload=PayLoad
		   }}.

encode(F) ->
	Bin = <<    (info_encode(F#fec_frame.fec_info))/binary,
				(F#fec_frame.payload)/binary
		  >>,
	{ok, Bin}.

-ifdef(TEST).
-include("fec_frame_test.hrl").
-endif.

