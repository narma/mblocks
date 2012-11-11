-module(minecraft_protocol).

-behaviour(ranch_protocol).

%% ranch export
-export([start_link/4]).

-define(TIMEOUT, 10000).


start_link(ListenerPid, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
	{ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(ListenerPid),
	gen_server:start_link(mblocks_player, [Transport, Socket], _Opt) 
	loop(Socket, Transport).

loop(Socket, Transport) ->
	case Transport:recv(Socket, 0, 1) of
		{ok, PacketId} ->
			{ok, PacketName, Signature} = minecraft_packet:get_header(PacketId),
			{ok, Packet} = read_full_packet(Socket, Signature),
			gen_server:cast(Player, Packet), 
			loop(Socket, Transport);
		_ ->
			ok = Transport:close(Socket)
	end.


read_full_packet(Socket, [TypeParam|TypeParamList]) ->
	%% 1. get max length for read from net
	%%%% [{11, [int, bool, float, string_len]}, {8, [double]}]
	%% 2. Get data from net
	%% 3. parse data and put into accumulator
	%% 3. goto 1
	case minecraft_packet:type_size(TypeParam) of
		T when is_integer(T) -> 
		undefined ->  

read_full_packet(_Socket, [], Output) ->
	lists:reverse(Output);

read_full_packet(Socket, TypeParamList) ->
	Size, ParamList = minecraft_packet:header_size(TypeParamList),
	{ok, Data} = gen_tcp:recv(Socket, Size, ?TIMEOUT);
	Output = decode_values(Data, ParamList)
	Output.

read_value(<<>>, Output) -> 
	lists:reverse(Output);

read_value(Data, [TypeParam|TypeParamList], Output) ->
	<<N:8,Rest/binary>> = Data,
	read_value(Data, [N =:= 1, Output])

read_value(Data, byte) ->
	{ok, <<N:8/signed>>} = gen_tcp:recv(Socket, 1, ?TIMEOUT),
	N.

read_value(Socket, short) ->
	{ok, <<N:16/signed>>} = gen_tcp:recv(Socket, 2, ?TIMEOUT),
	N.
read_value(Socket, int) ->
	{ok, <<N:32/signed>>} = gen_tcp:recv(Socket, 4, ?TIMEOUT),
	N.
read_value(Socket, long) ->
	{ok, <<N:64/signed>>} = gen_tcp:recv(Socket, 8, ?TIMEOUT),
	N.
read_value(Socket, float) ->
	{ok, <<N:32/float>>} = gen_tcp:recv(Socket, 4, ?TIMEOUT),
	N.
read_value(Socket, double) ->
	{ok, <<N:64/float>>} = gen_tcp:recv(Socket, 8, ?TIMEOUT),
	N.

read_value(Socket, string) ->
	{ok, StringLen} = read_value(Socket, short),
	{ok, Data} = gen_tcp:recv(Socket, StringLen, ?TIMEOUT),
	Data.

read_value(Data, string) ->
	<<L:16>>  = Data,
	{ok, Data} = gen_tcp:recv(Socket, L, ?TIMEOUT),
	Data.