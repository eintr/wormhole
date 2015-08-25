-module(msg).
-export([encode/1, decode/1, connid_combine/2, connid_split/1]).

-include("msg.hrl").
-include("protocol.hrl").

connid_combine(S, C) ->
	S*16#100000000 + C.

connid_split(ID) ->
	{ID div 16#100000000, ID rem 16#100000000}.

encode(Msg) ->
	Body = Msg#msg.body,
	case Msg#msg.code of
		?CODE_DATA ->
			BodyBin = Body#msg_body_data.data,
			{ok, << (Msg#msg.code):8/unsigned-big-integer,
					BodyBin/binary >>};
		?CODE_CHAP ->
			{ok, {Prefix, Len}} = ipaddr:prefix_parse(Body#msg_body_chap.prefix),
			BodyBin = <<	(Body#msg_body_chap.conn_id_client):32/unsigned-big-integer,
							(Body#msg_body_chap.salt)/binary,
							(ipaddr:addr_to_u32bin(Prefix))/binary,
							Len:8/unsigned-big-integer,
							(Body#msg_body_chap.md5):16/binary,
							(Body#msg_body_chap.username)/binary
					  >>,
			{ok, <<	(Msg#msg.code):8/unsigned-big-integer,
					BodyBin/binary >>};
		?CODE_CHAP_CONNECT ->
			RoutrPrefixBin = ipaddr:addrlist_to_bin(Body#msg_body_connect.route_prefixes),
			TunAddrS = Body#msg_body_connect.server_tun_addr,
			TunAddrC = Body#msg_body_connect.client_tun_addr,
			BodyBin = <<	(Body#msg_body_connect.conn_id_client):32/unsigned-big-integer,
							(Body#msg_body_connect.conn_id_server):32/unsigned-big-integer,
							(ipaddr:addr_to_u32bin(TunAddrS))/binary,
							(ipaddr:addr_to_u32bin(TunAddrC))/binary,
							RoutrPrefixBin/binary >>,
			{ok, <<	(Msg#msg.code):8/unsigned-integer,
					BodyBin/binary >>};
		?CODE_CHAP_REJECT ->
			BodyBin = <<	(Body#msg_body_reject.conn_id_client):32/unsigned-big-integer,
							(Body#msg_body_reject.reason)/binary>>,
			{ok, <<	(Msg#msg.code):8/unsigned-integer,
					BodyBin/binary >>};
		_Code ->
			{error, "Unknown message code."}
	end.

decode(MsgBin) ->
	<<Code:8/big-integer, BodyBin/binary>> = MsgBin,
	case Code of
		?CODE_DATA ->
			Body = #msg_body_data{data=BodyBin},
			{ok, #msg{code=Code, body=Body}};
		?CODE_CHAP ->
			<<ConnIDClient:32/unsigned-big-integer, Salt:8/binary, Prefix:4/binary, PrefixLen:8/big-integer, MD5:16/binary, Username/binary>> = BodyBin,
			{A,B,C,D}=ipaddr:u32bin_to_addr(Prefix),
			PrefixStr = lists:flatten(io_lib:format("~p.~p.~p.~p/~p", [A,B,C,D,PrefixLen])),
			Body = #msg_body_chap{ conn_id_client=ConnIDClient, salt=Salt, prefix=PrefixStr, md5=MD5, username=Username },
			{ok, #msg{code=Code, body=Body}};
		?CODE_CHAP_CONNECT ->
			<<	ConnIDClient:32/unsigned-big-integer,
				ConnIDServer:32/unsigned-big-integer,
				TunAddrServer:4/binary,
				TunAddrClient:4/binary,
				RoutePrefixes/binary	>> = BodyBin,
			Body = #msg_body_connect{	conn_id_client=ConnIDClient,
										conn_id_server=ConnIDServer,
										server_tun_addr=ipaddr:u32bin_to_addr(TunAddrServer),
										client_tun_addr=ipaddr:u32bin_to_addr(TunAddrClient),
										route_prefixes=ipaddr:bin_to_addrlist(RoutePrefixes)},
			{ok, #msg{code=Code, body=Body}};
		?CODE_CHAP_REJECT ->
			<<	ConnIDClient:32/unsigned-big-integer,
				Reason/binary	>> = BodyBin,
			Body = #msg_body_reject{	conn_id_client=ConnIDClient,
										reason = Reason},
			{ok, #msg{code=Code, body=Body}};
		_Code ->
			io:format("Unknown Msg code ~p\n", [_Code]),
			{error, "Unknown code."}
	end.

-ifdef(TEST).
-include("msg_test.hrl").
-endif.

