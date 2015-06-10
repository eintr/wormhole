-module(msg).
-export([encode/1, decode/1]).

-include("msg.hrl").
-include("protocol.hrl").

encode(Msg) ->
	Body = Msg#msg.body,
	case Msg#msg.code of
		?CODE_DATA ->
			BodyBin = Body#msg_body_data.data,
			{ok, <<	(Msg#msg.connection_id):64/unsigned-big-integer,
					(Msg#msg.code):8/unsigned-big-integer,
					BodyBin/binary >>};
		?CODE_CHAP ->
			{ok, {Prefix, Len}} = ipaddr:prefix_parse(Body#msg_body_chap.prefix),
			BodyBin = <<	(Body#msg_body_chap.conn_id_client):32/unsigned-big-integer,
							(Body#msg_body_chap.salt):64/binary,
							Prefix:32/big-integer,
							Len:8/big-integer,
							(Body#msg_body_chap.md5):128/binary,
							(Body#msg_body_chap.username)/binary >>,
			{ok, <<	(Msg#msg.connection_id):64/big-integer,
					(Msg#msg.code):8/big-integer,
					BodyBin/binary >>};
		?CODE_CHAP_CONNECT ->
			BodyBin = <<	(Body#msg_body_connect.conn_id_client):32/unsigned-big-integer,
							(Body#msg_body_connect.conn_id_server):32/unsigned-big-integer,
							(Body#msg_body_connect.server_tun_addr):32/unsigned-big-integer,
							(Body#msg_body_connect.client_tun_addr):32/unsigned-big-integer,
							(Body#msg_body_connect.route_prefixes)/binary >>,
			{ok, <<	(Msg#msg.connection_id):64/big-integer,
					(Msg#msg.code):8/big-integer,
					BodyBin/binary >>};
		_Code ->
			{error, "Unknown message code."}
	end.

decode(MsgBin) ->
	<<ConnectionID:64/big-integer, Code:8/big-integer, BodyBin/binary>> = MsgBin,
	case Code of
		?CODE_DATA ->
			Body = #msg_body_data{data=BodyBin},
			{ok, #msg{connection_id=ConnectionID, code=Code, body=Body}};
		?CODE_CHAP ->
			<<ConnIDClient:32/unsigned-big-integer, Salt:64/binary, Prefix:32/binary, PrefixLen:8/big-integer, MD5:128/binary, Username/binary>> = BodyBin,
			{A,B,C,D}=ipaddr:u32bin_to_addr(Prefix),
			PrefixStr = io_lib:format("~p.~p.~p.~p/~p", [A,B,C,D,PrefixLen]),
			Body = #msg_body_chap{ conn_id_client=ConnIDClient, salt=Salt, prefix=PrefixStr, md5=MD5, username=Username },
			{ok, #msg{connection_id=ConnectionID, code=Code, body=Body}};
		?CODE_CHAP_CONNECT ->
			<<	ConnIDClient:32/unsigned-big-integer,
				ConnIDServer:32/unsigned-big-integer,
				TunAddrServer:32/binary,
				TunAddrClient:32/binary,
				RoutePrefixes/binary	>> = BodyBin,
			{A,B,C,D}=ipaddr:u32bin_to_addr(Prefix),
			PrefixStr = io_lib:format("~p.~p.~p.~p/~p", [A,B,C,D,PrefixLen]),
			Body = #msg_body_connect{	conn_id_client=ConnIDClient,
										conn_id_server=ConnIDServer,
										server_tun_addr=ipaddr:u32bin_to_addr(TunAddrServer),
										client_tun_addr=ipaddr:u32bin_to_addr(TunAddrClient),
										route_prefixes=RoutePrefixes},
			{ok, #msg{connection_id=ConnectionID, code=Code, body=Body}};
		_Code ->
			io:format("Unknown Msg code ~p\n", [_Code]),
			{error, "Unknown code."}
	end.

