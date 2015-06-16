-module(frame).
-export([decode/1, encode/1]).

-include("frame.hrl").

encode(Frame) ->
	<<(Frame#frame.conn_id):64/unsigned-big-integer, (Frame#frame.payload)/binary>>.

decode(FrameBin) ->
	<< ConnID:64/unsigned-big-integer, Payload/binary>> = FrameBin,
	#frame{conn_id=ConnID, payload=Payload}.



-ifdef(TEST).
-include("frame_test.hrl").
-endif.

