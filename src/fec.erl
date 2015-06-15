-module(fec).
-export([fec_encode/2, fec_decode/2, next_gid/1, delta_gid/2]).

-include("fec_frame.hrl").
-include("fec.hrl").

fec_encode(Frame, Context) ->
	{ok, [Frame], Context}.

fec_decode(Frame, Context) ->
	{ok, [Frame], Context}.

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

