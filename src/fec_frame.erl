-module(fec_frame).

-include("fec_frame.hrl").

-export([encode/1, decode/1, info_encode/1, info_decode/1]).

info_decode(FecBin) ->
	<<  Fecgid:24/big-integer,
		Fecseq:8/big-integer,
		Fecgsize:8/big-integer >> = FecBin,
	#fec_info{fecg_id=Fecgid, fec_seq=Fecseq, fec_gsize=Fecgsize}.

info_encode(F) ->
	<<    (F#frame.fec_info#fec_info.fecg_id):24/big-integer,
		  (F#frame.fec_info#fec_info.fec_seq):8/big-integer,
		  (F#frame.fec_info#fec_info.fec_gsize):8/big-integer >>.

decode(Packet) ->
	<<  ConnectionID:64/big-integer, Fec:40/big-integer, PayLoad/binary >> = Packet,
	<<  Fecgid:24/big-integer,
		Fecseq:8/big-integer,
		Fecgsize:8/big-integer >> = Fec,
	{ok, #frame{
			connection_id=ConnectionID,
			fec_info=info_decode(Fec),
			payload=PayLoad
		   }}.

encode(F) ->
	Bin = <<    (F#frame.connection_id):64/big-integer,
				(info_encode(F#frame.fec_info)):40/binary,
				(F#frame.payload)/binary
		  >>,
	{ok, Bin}.


