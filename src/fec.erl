-module(fec).
-export([encode/2, encode_push/2, decode/1, next_gid/1, delta_gid/2]).

-include("wire_frame.hrl").
-include("fec.hrl").

encode(FramePayload, Size) ->
	Context = get(encode_context),
	FecInfo = #fec_info{fecg_id=get(current_gid), fec_seq=1, fec_gsize=get(gsize), fec_payload_size=Size},
	WireFrame = #wire_frame{conn_id=get(conn_id), fec_info=FecInfo, payload_cipher=FramePayload},
	put(current_gid, next_gid(get(current_gid))),
	put(encode_context, Context),
	{ok, [WireFrame]}.

encode_push(FramePayload, Size) ->
	Context = get(encode_context),
	FecInfo = #fec_info{fecg_id=get(current_gid), fec_seq=1, fec_gsize=get(gsize), fec_payload_size=Size},
	WireFrame = #wire_frame{conn_id=get(conn_id), fec_info=FecInfo, payload_cipher=FramePayload},
	put(current_gid, next_gid(get(current_gid))),
	put(encode_context, Context),
	{ok, [WireFrame]}.

decode(WireFrame) ->
	FecInfo = WireFrame#wire_frame.fec_info,
	case get({decode_context, FecInfo#fec_info.fecg_id}) of
		undefined ->
			Context = #fecg_context{    id = FecInfo#fec_info.fecg_id,
										width = FecInfo#fec_info.fec_gsize,
										timestamp = util:timestamp_ms(),
										pool=[]},
			put({decode_context, FecInfo#fec_info.fecg_id}, Context);
		Context -> ok
	end,
	%%... Do the read FEC magic.
	put({decode_context, FecInfo#fec_info.fecg_id}, Context),
	{ok, [WireFrame#wire_frame.payload_cipher]}.

next_gid(?GIDMAX) ->
	    0;
next_gid(N) ->
	    N+1.

delta_gid(A, B) ->
	D = min(abs(A+(?GIDMAX+1)-B) rem (?GIDMAX+1) , abs(B+(?GIDMAX+1) - A) rem (?GIDMAX+1)),
	if
		(B+D) rem (?GIDMAX+1) == A -> D;
		true -> -D
	end.

-ifdef(TEST).
-include("fec_test.hrl").
-endif.

