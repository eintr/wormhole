-include_lib("eunit/include/eunit.hrl").

fecgid_next_test() ->
	Next_of_0 = fec_node:next_gid(0),
	?assert( Next_of_0 =:= 1 ),
	Next_of_1 = fec_node:next_gid(1),
	?assert( Next_of_1 =:= 2 ),
	Next_of_1234 = fec_node:next_gid(1234),
	?assert( Next_of_1234 =:= 1235 ),
	Next_of_GID_MAX = fec_node:next_gid(16#ffffff),
	?assert( Next_of_GID_MAX =:= 0 ).

delta_gid_test() ->
	?assert(fec_node:delta_gid(63453, 63451) =:= 2),
	?assert(fec_node:delta_gid(0, 16#ffffff) =:= 1),
	?assert(fec_node:delta_gid(8765, 7654) =:= 1111).

