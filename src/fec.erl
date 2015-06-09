-module(fec).

-include("frame.hrl").
-include("fec.hrl").

-export([en/1, de/1]).

de(FecBin) ->
	<<  Fecgid:24/big-integer,
		Fecseq:8/big-integer,
		Fecgsize:8/big-integer >> = FecBin,
	#fec_info{fecg_id=Fecgid, fec_seq=Fecseq, fec_gsize=Fecgsize}.

en(F) ->
	<<    (F#frame.fec_info#fec_info.fecg_id):24/big-integer,
		  (F#frame.fec_info#fec_info.fec_seq):8/big-integer,
		  (F#frame.fec_info#fec_info.fec_gsize):8/big-integer >>.

