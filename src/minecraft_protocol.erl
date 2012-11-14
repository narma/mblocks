-module(minecraft_protocol).

-behaviour(ranch_protocol).

%% ranch export
-export([start_link/4, init/4]).

-define(TIMEOUT, 10000).


start_link(ListenerPid, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
	{ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(ListenerPid),
	%% gen_server:start_link(mblocks_player, [Transport, Socket], _Opt) 
	loop(Socket, Transport).

loop(Socket, Transport) ->
	case Transport:recv(Socket, 1, 5000) of
		{ok, <<PacketId:8>>} ->
			io:format("PacketId: ~p~n", [PacketId]),
			{ok, PacketName, DataTypes} = minecraft_packet:get_header(PacketId),
			io:format("DataTypes: ~p~n", [DataTypes]),
			{ok, Packet} = read_full_packet(Socket, Transport, DataTypes),
			io:format("Packet: ~p~n", [Packet]),
			%% Send packet to worker. 
			loop(Socket, Transport);
		_ ->
			ok = Transport:close(Socket)
	end.


read_full_packet(Socket, Transport, DataTypes) ->
	%% 1. get max length for read from net
	%%%% [{11, [int, bool, float, string]}, {8, [double]}]
	%% 2. Get data from net
	%% 3. parse data and put into accumulator
	%% 3. goto 1
	PacketSizeInfo = minecraft_packet:packet_size(DataTypes),
	io:format("PacketSizeInfo ~p~n", [PacketSizeInfo]),
	read_full_packet(Socket, Transport, DataTypes, PacketSizeInfo, []).

read_full_packet(_Socket, _Transport, [], [], Acc) ->
	{ok, lists:reverse(Acc)};

read_full_packet(Socket, Transport, DataTypes, [Size|PacketSizeInfo], Acc) ->
	case Size of
		Length when is_integer(Length) -> 
			{ok, Data} = Transport:recv(Socket, Length, ?TIMEOUT),
			{ok, TailDataTypes, Out} = decode_values(Data, DataTypes);
		Type -> 
			Out = read_value(Socket, Transport, Type),
			[_|TailDataTypes] = DataTypes
	end,
	read_full_packet(Socket, Transport, TailDataTypes, PacketSizeInfo, Out ++ Acc).
	
decode_values(Data, DataTypes) ->
	decode_values(Data, DataTypes, []).

decode_values(<<>>, DataTypes, Values) -> 
	{ok, DataTypes, lists:reverse(Values)};

decode_values(Data, [Type|DataTypes], Values) ->
	case Type of
		bool -> 
			<<B:8,Rest/binary>> = Data,
			V = B =:= 1;
		byte   ->  <<V:8/signed,Rest/binary>> = Data;
		ubyte  ->  <<V:8,Rest/binary>> = Data;
		short  ->  <<V:16/signed,Rest/binary>> = Data;
		ushort ->  <<V:16,Rest/binary>> = Data;
		int    ->  <<V:32/signed,Rest/binary>> = Data;
		long   ->  <<V:64/signed,Rest/binary>> = Data;
		float  ->  <<V:32/float,Rest/binary>> = Data;
		double ->  <<V:64/float,Rest/binary>> = Data
	end,
	decode_values(Rest, DataTypes, [V|Values]).

read_value(Socket, Transport, string) ->
	{ok, <<Characters:16>>} = Transport:recv(Socket, 2, ?TIMEOUT),
	Size = Characters*2, %% UTF-16
	{ok, Data} = Transport:recv(Socket, Size, ?TIMEOUT),
	[Data].