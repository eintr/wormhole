-module(frame).
-export([decode/1, encode/1]).

-include("frame.hrl").
-include("fec.hrl").

decode(Packet) ->
	<<	ConnectionID:64/big-integer, Fec:40/big-integer, PayLoad/binary >> = Packet,
	<<	Fecgid:24/big-integer,
		Fecseq:8/big-integer,
		Fecgsize:8/big-integer >> = Fec,
	{ok, #frame{
			connection_id=ConnectionID,
			fec_info=fec:de(Fec),
			payload=PayLoad
		   }}.

encode(F) ->
	Bin = <<	(F#frame.connection_id):64/big-integer,
				(fec:en(F#frame.fec_info)):40/binary,
				(F#frame.payload)/binary
			>>,
	{ok, Bin}.

