-include_lib("eunit/include/eunit.hrl").

-include("protocol.hrl").
-include("msg.hrl").

start_test() ->
	{ok, _Pid} = fec_encoder:start_link({12345678910, 3}).

encode_width2_test()  ->
	Msg = #msg{code=?CODE_DATA, body=#msg_body_data{data = <<"1234567890">>}},
	F1 = <<12345678910:64/big-unsigned-integer,
		   <<1:24/big-unsigned-integer,
			 1:8/unsigned-big-integer,
			 2:8/unsigned-big-integer,
			 11:16/unsigned-big-integer	>>/binary,
		   <<0:8/unsigned-big-integer,"1234567890">>/binary	>>,
	FP = <<12345678910:64/big-unsigned-integer,
		   <<1:24/big-unsigned-integer,
			 0:8/unsigned-big-integer,
			 2:8/unsigned-big-integer,
			 11:16/unsigned-big-integer	>>/binary,
		   <<0:8/unsigned-big-integer,"1234567890">>/binary	>>,
	{ok, Pid} = fec_encoder:start_link({12345678910, 2}),
	Res = gen_fsm:sync_send_event(Pid, {encode, Msg}),
	?assert(Res =:= {ok, [F1, FP]}).

encode_width3_test()  ->
	Msg1 = #msg{code=?CODE_DATA, body=#msg_body_data{data = <<"1234567890">>}},
	Msg2 = #msg{code=?CODE_DATA, body=#msg_body_data{data = <<"1234567890">>}},
	F1 = <<12345678910:64/big-unsigned-integer,
		   <<1:24/big-unsigned-integer,
			 1:8/unsigned-big-integer,
			 3:8/unsigned-big-integer,
			 11:16/unsigned-big-integer	>>/binary,
		   <<0:8/unsigned-big-integer,"1234567890">>/binary	>>,
	F2 = <<12345678910:64/big-unsigned-integer,
		   <<1:24/big-unsigned-integer,
			 2:8/unsigned-big-integer,
			 3:8/unsigned-big-integer,
			 11:16/unsigned-big-integer	>>/binary,
		   <<0:8/unsigned-big-integer,"1234567890">>/binary	>>,
	FP = <<12345678910:64/big-unsigned-integer,
		   <<1:24/big-unsigned-integer,
			 0:8/unsigned-big-integer,
			 3:8/unsigned-big-integer,
			 22:16/unsigned-big-integer	>>/binary,
		   <<0:8/unsigned-big-integer,0,0,0,0,0,0,0,0,0,0>>/binary	>>,
	{ok, Pid} = fec_encoder:start_link({12345678910, 3}),
	Res1 = gen_fsm:sync_send_event(Pid, {encode, Msg1}),
	?assert(Res1 =:= pass),
	Res2 = gen_fsm:sync_send_event(Pid, {encode, Msg2}),
	?assert(Res2 =:= {ok, [F1, F2, FP]}).

