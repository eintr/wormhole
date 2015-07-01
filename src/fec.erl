-module(fec).
-export([encode/2, encode_push/2, decode/1, next_gid/1, delta_gid/2]).

-include("wire_frame.hrl").
-include("fec.hrl").

-record(fec_decode_context, {id, width, timestamp, pool}).

encode(FramePayload, Size) ->
	Context = case get(encode_context) of
				  undefined -> init_encode_context();
				  C -> C
			  end,
	FecInfo = #fec_info{fecg_id=get(current_gid), fec_seq=1, fec_gsize=get(gsize), fec_payload_size=Size},
	WireFrame = #wire_frame{conn_id=get(conn_id), fec_info=FecInfo, payload_cipher=FramePayload},
	put(current_gid, next_gid(get(current_gid))),
	put(encode_context, Context),
	{ok, [WireFrame]}.

encode_push(FramePayload, Size) ->
	Context = case get(encode_context) of
				  undefined -> init_encode_context();
				  C -> C
			  end,
	FecInfo = #fec_info{fecg_id=get(current_gid), fec_seq=1, fec_gsize=get(gsize), fec_payload_size=Size},
	WireFrame = #wire_frame{conn_id=get(conn_id), fec_info=FecInfo, payload_cipher=FramePayload},
	put(current_gid, next_gid(get(current_gid))),
	put(encode_context, Context),
	{ok, [WireFrame]}.

decode(WireFrame) ->
	FecInfo = WireFrame#wire_frame.fec_info,
	case get({decode_context, FecInfo#fec_info.fecg_id}) of
		undefined ->
			Context = #fec_decode_context{    id = FecInfo#fec_info.fecg_id,
											  width = FecInfo#fec_info.fec_gsize,
											  timestamp = util:timestamp_ms(),
											  pool=[]},
			put({decode_context, FecInfo#fec_info.fecg_id}, Context);
		Context -> ok
	end,
	%%... Do the read FEC magic.
	case lists:keyfind(FecInfo#fec_info.fec_seq, 1, Context#fec_decode_context.pool) of
		{_, _, _} ->
			duplicated;
		false ->
			NewPool = Context#fec_decode_context.pool ++ [{FecInfo#fec_info.fec_seq, FecInfo, WireFrame#wire_frame.payload_cipher}],
			if
				length(NewPool) == Context#fec_decode_context.width ->
					erase({decode_context, FecInfo#fec_info.fecg_id}),
					completed;
				length(NewPool) == Context#fec_decode_context.width-1 ->
					put({decode_context, FecInfo#fec_info.fecg_id},
						Context#fec_decode_context{pool=NewPool}),
					WirePayLoads = pool_decode(NewPool),
					{ok, WirePayLoads};
				true ->
					put({decode_context, FecInfo#fec_info.fecg_id}, Context#fec_decode_context{pool=NewPool}),
					need_more
			end
	end.

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

init_encode_context() ->
	ok.

%[{fec_seq, Data}]
pool_decode(L) ->
	CompleteList = case lists:keyfind(0, 1, L) of
					   {0, I, _} ->
						   MissedPayLoad = lists:foldl(fun
														({_, _, Data}, Xor)->
															bin_xor(Data, Xor)
													end, <<>>, L),
						   MissedLen = lists:foldl(fun ({_, FecInfo, _}, Len)->
														   Len bxor FecInfo#fec_info.fec_payload_size
												   end, 0, L),
						   [MissedSeq] = lists:foldl(fun ({_, FecInfo, _}, SeqL)->
														   SeqL -- [FecInfo#fec_info.fec_seq]
												   end, lists:seq(0, I#fec_info.fec_gsize-1), L),
						   lists:keydelete(0, 1, L) ++ [{MissedSeq, I#fec_info{fec_seq=MissedSeq, fec_payload_size=MissedLen}, MissedPayLoad}];
					   false -> L
				   end,
	% TODO: Sort
	lists:map(fun ({_, I, D})->
					  binary:part(D, {0, I#fec_info.fec_payload_size})
			  end, CompleteList).

bin_xor(Bin1, Bin2) ->
	bin_xor(Bin1, Bin2, <<>>).
bin_xor(<<>>, <<>>, R) ->
	R;
bin_xor(<<Byte1:8/unsigned-integer, Tail1/binary>>, <<>>, R) ->
	bin_xor(Tail1, <<>>, <<R/binary, Byte1:8/unsigned-integer>>);
bin_xor(<<>>, <<Byte2:8/unsigned-integer, Tail2/binary>>, R) ->
	bin_xor(<<>>, Tail2, <<R/binary, Byte2:8/unsigned-integer>>);
bin_xor(<<Byte1:8/unsigned-integer, Tail1/binary>>, <<Byte2:8/unsigned-integer, Tail2/binary>>, R) ->
	bin_xor(Tail1, Tail2, <<R/binary, (Byte1 bxor Byte2):8/unsigned-integer>>).

-ifdef(TEST).
-include("fec_test.hrl").
-endif.

