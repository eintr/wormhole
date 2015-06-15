-module(fec).
-export([fec_encode/1, fec_encode_push/1, fec_decode/1, next_gid/1, delta_gid/2]).

-include("fec_frame.hrl").
-include("fec.hrl").

fec_encode(FramePayload) ->
	Context = get(encode_context),
	FecInfo = #fec_info{fecg_id=get(current_gid), fec_seq=1, fec_gsize=get(gsize)},
	FecFrame = #fec_frame{fec_info=FecInfo, payload=FramePayload},
	put(current_gid, next_gid(get(current_gid))),
	{ok, [FecFrame], Context}.

fec_encode_push(FramePayload) ->
	Context = get(encode_context),
	FecInfo = #fec_info{fecg_id=get(current_gid), fec_seq=1, fec_gsize=get(gsize)},
	FecFrame = #fec_frame{fec_info=FecInfo, payload=FramePayload},
	%%... Do the read FEC magic.
	put(current_gid, next_gid(get(current_gid))),
	{ok, [FecFrame], Context}.

fec_decode(Frame) ->
	FecInfo = Frame#fec_frame.fec_info,
	Context = get({decode_context, FecInfo#fec_info.fecg_id}),
	%%... Do the read FEC magic.
	put({decode_context, FecInfo#fec_info.fecg_id}, Context),
	{ok, [Frame#fec_frame.payload], Context}.

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

