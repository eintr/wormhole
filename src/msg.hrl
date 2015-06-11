-define(SALT_LENGTH, 8).

-record(msg_body_data, {
		  data
		 }).

-record(msg_body_chap, {
		  conn_id_client,
		  salt,
		  prefix,
		  md5,
		  username
		 }).

-record(msg_body_connect, {
		  conn_id_client,
		  conn_id_server,
		  server_tun_addr,
		  client_tun_addr,
		  route_prefixes
		 }).

-record(msg, {
		  connection_id,
		  code,
		  body
		 }).
