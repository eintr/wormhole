-module(fec_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(fec_info, {
		  fecg_id,
		  fec_seq,
		  fec_gsize
		 }).

-record(fec_frame, {
		  seq,
		  data
		 }).

-record(fecg_slot, {
		  fecg_id,
		  time,
		  width,
		  pending_frames=[]
		 })

-record(fec_encode_config, {
		  width=2,
		  interleave=1,
		  timeout=10000,	%% in ms
		  current_gid,
		  pending_fecgs=[]
		 }).

-record(fec_decode_config, {
		  minimal_gid=0,
		  pending_fecgs=[]
		 }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, {#fec_config{}, 0, []}}.

handle_call({config, _}, _From, State) ->
    {reply, todo, State};
handle_call({decode, Packet}, _From, {{Config, PendingFecg}, {Min, PendingFecg}}=State) ->
	{ok, {_ConnID, Fec, LineData}} = decode_fec(Packet),
	case llist:keyfind(Fec#fec_info.fecg_id, 1, PendingFecg) of
		{Fec#fec_info.fecg_id, Fec#fec_info.fec_gsize, Buffer} ->
			NewBuffer = Buffer ++ [{Fec#fec_info.fec_seq, LineData}],
			case buffer_process() of
				{ok, Packets} ->
					{reply, {ok, Packets}, NewState};
				{need_more} ->
					{reply, {need_more}, NewState};
				{too_late} ->
					{reply, {too_late}, NewState}
			end;
		false ->
			Fec
			{}
	end;
handle_call({encode, {ConnID, LineData}, Flags}, _From, State) ->
    {reply, {ok, [Packet]}, State};
handle_call({conn_add, ConnID}, _From, State) ->
    {reply, ok, NewState};
handle_call({conn_add, ConnID, FecConfig}, _From, State) ->
    {reply, ok, NewState};
handle_call({conn_delete, ConnID}, _From, State) ->
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

decode_fec(Packet) ->
	    <<ConnectionID:64/big-integer, FecInfo:40/big-integer, LineData/binary>> = Packet,
		    <<  Fec#fec_info.fecg_id:24/big-integer,
				        Fec#fec_info.fec_seq:8/big-integer,
						        Fec#fec_info.fec_gsize:8/big-integer >> = FecInfo;
    {ok, {ConnectionID, Fec, LineData}}.

encode_fec(Frame, {ConnectionID, Fec, LineData}) ->
	    {}.

