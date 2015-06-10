-module(ipaddr).
-export([u32bin_to_addr/1]).
-export([match/2, prefix_parse/1]).

u32bin_to_addr(U32Bin) ->
	<<A:8, B:8, C:8, D:8>> = U32Bin,
	{A,B,C,D}.

prefix_parse(Str) when is_list(Str) ->
	Tokens = string:tokens(Str, "/"),
	case length(Tokens) of
		1 ->
			[Prefix] = Tokens,
			case inet:parse_address(Prefix) of
				{ok, Addr} ->
					{ok, {Addr, 32}};
				_ ->
					{error, "Illegal prefix"}
			end;
		2->
			[P, L] = Tokens,
			{Stat, Prefix} = inet:parse_address(P),
			Len = list_to_integer(L),
			if
				Stat == error ->
					{error, "Illegal prefix"};
				Len>32 ->
					{error, "Illegal prefix length"};
				Len<0 ->
					{error, "Illegal prefix length"};
				true ->
					{ok, {Prefix, Len}}
			end;
		_->
			{error, "Unknown prefix format"}
	end.

match({A1, B1, C1, D1}, {{A2, B2, C2, D2}, Len}) ->
	Num  = (A1*256*256*256 + B1*256*256 + C1*256 + D1) bsr (32-Len),
	Masq = (A2*256*256*256 + B2*256*256 + C2*256 + D2) bsr (32-Len),
	if
		Masq == Num ->
			true;
		true ->
			false
	end.

