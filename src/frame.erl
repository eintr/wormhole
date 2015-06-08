-module(frame).
-export([decode/2, encode/2]).

-import(log, [log/3, log/2]).

-include("frame.hrl").
-include("fec.hrl").

decode(v1, Packet, KVConfig) ->
	<<ConnectionID:64/big-integer, FecInfo:40/big-integer, LineData/binary>> = Packet,
	{ok, <<Code:8/big-integer, PayLoad/binary>>} = localcrypt:decrypt(ConnectionID, LineData),
	case Code of
		?CODE_CHAP ->
			<<>> = BodyData,
			{?CODE_CHAP, };
		?CODE_CHAP_REJECT ->
			<<>> = BodyData,
			{?CODE_CHAP_REJECT, };
		?CODE_DATA ->
			<<?CODE_DATA:8/big-integer, Data/binary>> = PlainBody,
			{?CODE_DATA, BodyData};
		?CODE_CHAP_CONNECT ->
			<<>> = BodyData,
			{?CODE_CHAP_CONNECT, };
		?CODE_ECHO ->
			<<>> = BodyData,
			{?CODE_ECHO, };
		?CODE_CLOSE ->
			<<>> = BodyData,
			{?CODE_CLOSE};
		_Other ->
			io:format("Unknown message code: ~p.\n", [_Other])
	end.

encode(v1, {?CODE_DATA, RawData}, KVConfig) ->
	RawBody = <<	0:1/big-integer,
					FlowID:15/big-integer,
					RawData/binary >>,
	CryptedBody =  mycrypt:encrypt_frame_body(RawBody, CryptFlag, Zip, config:get(sharedkey, Context)),
	{ok, <<1:3/big-integer, CryptFlag:2/big-integer, Zip:1/big-integer, 0:2/big-integer, CryptedBody/binary>>};
encode(v1, {ctl, socket, cert_req, _}, _Context) ->
	Frame_body = <<	1:1/big-integer,
					?OP_SOCKET_CERT_REQ:15/big-integer>>,
	{ok, <<1:3/big-integer, 0:2/big-integer, 0:1/big-integer, 0:2/big-integer, Frame_body/binary>>};
encode(v1, {ctl, socket, cert, {CertificateBin}}, _Context) ->
	Frame_body = <<	1:1/big-integer,
					?OP_SOCKET_CERT:15/big-integer,
					(zlib:zip(CertificateBin))/binary >>,
	{ok, <<1:3/big-integer, 0:2/big-integer, 0:1/big-integer, 0:2/big-integer, Frame_body/binary>>};
encode(v1, {ctl, socket, key_sync, {CRC32, Encrypted_shared_key}}, _Context) ->
	Frame_body = <<	1:1/big-integer,
					?OP_SOCKET_KEY_SYNC:15/big-integer,
					CRC32:32/big-integer,
					Encrypted_shared_key/binary>>,
	{ok, <<1:3/big-integer, 0:2/big-integer, 0:1/big-integer, 0:2/big-integer, Frame_body/binary>>};
encode(v1, {ctl, socket, key_rej, _}, _Context) ->
	Frame_body = <<	1:1/big-integer,
					?OP_SOCKET_KEY_REJ:15/big-integer>>,
	{ok, <<1:3/big-integer, 0:2/big-integer, 0:1/big-integer, 0:2/big-integer, Frame_body/binary>>};
encode(v1, {ctl, pipeline, open, {FlowID, CryptFlag, Zip, MaxDelay, ReplyFlags, Data}}, Context) ->
	Rawbody = <<	1:1/big-integer,
					?OP_PIPELINE_OPEN:15/big-integer,
					FlowID:16/big-integer,
					MaxDelay:16/big-integer,
					ReplyFlags:8/big-integer,
					Data/binary>>,
	Frame_body = mycrypt:encrypt_frame_body(Rawbody, CryptFlag, Zip, config:get(sharedkey, Context)),
	{ok, <<1:3/big-integer, CryptFlag:2/big-integer, Zip:1/big-integer, 0:2/big-integer, Frame_body/binary>>};
encode(v1, {ctl, pipeline, close, {FlowID, CryptFlag, Zip}}, Context) ->
	Raw_body = <<1:1/big-integer, ?OP_PIPELINE_CLOSE:15/big-integer, FlowID:16/big-integer>>,
	Frame_body = mycrypt:encrypt_frame_body(Raw_body, CryptFlag, Zip, config:get(sharedkey, Context)),
	{ok, <<1:3/big-integer, CryptFlag:2/big-integer, Zip:1/big-integer, 0:2/big-integer, Frame_body/binary>>};

% TODO: to be continued.
encode(v1, {ctl, Level, Code, _Args}, _Context) ->
	log(log_error, "Don't know how to encode ~p/~p message.", [Level, Code]),
	{error, "Don't know how to encode."}.

