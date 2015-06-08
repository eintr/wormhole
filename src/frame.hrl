-define(CODE_DATA, 0).
-define(CODE_CHAP, 1).
-define(CODE_CHAP_CONNECT, 2).
-define(CODE_CHAP_REJECT, 3).
-define(CODE_ECHO, 4).
-define(CODE_PING, 4).
-define(CODE_PONG, 5).
-define(CODE_CLOSE, 6).

-define(CONNID_ANY, 16#0000000000000000).
-define(CONNID_INVAL, 16#ffffffffffffffff).

-define(SALT_LENGTH, 8).

-record(body_chap, {
	salt,
	prefix,
	prefix_len,
	md5,
	clientid
}).

