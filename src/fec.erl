-module(fec).
-export([encode/2, encode_push/2, decode/1, next_gid/1, delta_gid/2]).

-ifdef(TEST).
-export([bin_xor/2]).
-endif.

-include("wire_frame.hrl").
-include("fec.hrl").

encode(FramePayload, Size) ->	% Size is needed here! Since the FramePayload was encrypted!
	Context = complish_intlv_pool(get(encode_context)),
	[H|T] = Context#fec_encode_context.intlv_pool,
	case fecg_encode(H, {FramePayload, Size}) of
		{pass, NewH} ->
			put(encode_context, Context#fec_encode_context{intlv_pool=T++[NewH]}),
			need_more;
		{ok, WireFrames} ->
			put(encode_context, Context#fec_encode_context{intlv_pool=T}),
			{ok, WireFrames}
	end.

encode_push(FramePayload, Size) ->
	Context = get(encode_context),
	FecInfo = #fec_info{
				 fecg_id=Context#fec_encode_context.next_gid,
				 fec_seq=1,
				 fec_gsize=2,
				 fec_payload_size=Size	},
	WireFrame = #wire_frame{conn_id=get(conn_id), fec_info=FecInfo, payload_cipher=FramePayload},
	put(encode_context, Context#fec_encode_context{
						  next_gid=next_gid(Context#fec_encode_context.next_gid)}),
	io:format("~p: encode_push(): ~p\n", [?MODULE, WireFrame]),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

complish_intlv_pool(Context) ->
	D = Context#fec_encode_context.intlv_level - length(Context#fec_encode_context.intlv_pool),
	if
		D > 0 ->
			Next = Context#fec_encode_context.next_gid,
			Range = lists:seq(Next, Next+D),
			NewPool = Context#fec_encode_context.intlv_pool ++ 
			lists:map(fun (N)->
							  #fecg_encode_context{
								 gid=N,
								 width=Context#fec_encode_context.suggest_width}
					  end, Range),
			Context#fec_encode_context{next_gid=Next+D+1, intlv_pool=NewPool};
		true -> Context
	end.

fecg_encode(FecgEncodeContext, {Bin, BinSize}) ->	% FramePayload = {Bin, Size}
	NewPool = FecgEncodeContext#fecg_encode_context.pool ++ [{Bin, BinSize}],
	{Party, TotalSize} = FecgEncodeContext#fecg_encode_context.party_frame,
	NewPartyFrame = {bin_xor(Party, Bin), TotalSize+BinSize},
	NewContext = FecgEncodeContext#fecg_encode_context{party_frame=NewPartyFrame, pool=NewPool},
	if
		length(NewPool) == (NewContext#fecg_encode_context.width - 1) ->	% Fully filled
			{ok, pool_encode(NewContext)};
		true ->
			{pass, FecgEncodeContext#fecg_encode_context{party_frame=NewPartyFrame, pool=NewPool}}
	end.

pool_encode(C) ->
	{B,S} = C#fecg_encode_context.party_frame,
	[#wire_frame{
		conn_id = get(conn_id),
		fec_info = #fec_info{fecg_id = C#fecg_encode_context.gid,
							 fec_seq = 0,
							 fec_gsize = C#fecg_encode_context.width,
							 fec_payload_size = S}, payload_cipher = B}]
	++
	lists:foldl(fun ({Bin, Size}, {N, List})->
						{N+1, List ++ [ #wire_frame{
										   conn_id = get(conn_id),
										   fec_info = #fec_info{fecg_id = C#fecg_encode_context.gid,
																fec_seq = N,
																fec_gsize = C#fecg_encode_context.width,
																fec_payload_size = Size}, payload_cipher = Bin}]}
				end, {1, []}, C#fecg_encode_context.pool).

%[{fec_seq, Data}, ...]
pool_decode(L) ->
	%io:format("Trying to decode ~p\n", [L]),
	CompleteList = case lists:keytake(0, 1, L) of
					   {value, {0, PartyI, PartyData}, L1} ->
						   %io:format("Party frame is {~p, ~p}\n", [PartyI, PartyData]),
						   MissedPayLoad = lists:foldl(fun
														   ({_, _, Data}, Xor)->
															bin_xor(Data, Xor)
													   end, PartyData, L1),
						   %io:format("MissedPayLoad is ~p\n", [MissedPayLoad]),
						   MissedLen = lists:foldl(fun ({_, FecInfo, _}, Len)->
														   Len - FecInfo#fec_info.fec_payload_size
												   end, PartyI#fec_info.fec_payload_size, L1),
						   %io:format("MissedLen is ~p\n", [MissedLen]),
						   [MissedSeq] = lists:foldl(fun ({_, FecInfo, _}, SeqL)->
															 SeqL -- [FecInfo#fec_info.fec_seq]
													 end, lists:seq(1, PartyI#fec_info.fec_gsize-1), L1),
						   %io:format("MissedSeq is ~p\n", [MissedSeq]),
						   L1 ++ [{MissedSeq, PartyI#fec_info{fec_seq=MissedSeq, fec_payload_size=MissedLen}, MissedPayLoad}];
					   false -> L
				   end,
	%io:format("Completed List is ~p\n", [CompleteList]),
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

