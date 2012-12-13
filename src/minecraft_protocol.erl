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
	Writer = spawn_link(?MODULE, async_writer, [Transport, Socket]),
	{ok, PlayerPid} = mb_player:start_link([Writer], []),
	loop(Socket, Transport, PlayerPid).

loop(Socket, Transport, PlayerPid) ->
	case Transport:recv(Socket, 1, 5000) of
		{ok, <<PacketId:8>>} ->
			{ok, PacketName, DataTypes} = minecraft_packet:get_header(PacketId),
			{ok, PacketData} = read_full_packet(Socket, Transport, DataTypes),
			Packet = list_to_tuple([PacketName| [PacketId|PacketData]]), % create record
			gen_server:cast(PlayerPid, {pkt, Packet}),                   % it's must be defined in *packets.hrl
			loop(Socket, Transport, PlayerPid);
		_ ->
			ok = Transport:close(Socket)
	end.

% ======================================================================
% decoding
% ======================================================================

read_full_packet(Socket, Transport, DataTypes) ->
	PacketSizeInfo = minecraft_packet:packet_size(DataTypes),
	read_full_packet(Socket, Transport, DataTypes, PacketSizeInfo, []).

read_full_packet(_Socket, _Transport, [], [], Acc) ->
	{ok, lists:reverse(Acc)};

read_full_packet(Socket, Transport, DataTypes, [Size|PacketSizeInfo], Acc) ->
	case Size of
		Length when is_integer(Length) ->
			{ok, BinData} = Transport:recv(Socket, Length, ?TIMEOUT),
			{ok, TailDataTypes, Out} = decode_values(BinData, DataTypes);
		Type when is_atom(Type) ->
			Out = read_value(Socket, Transport, Type),
			[_|TailDataTypes] = DataTypes
	end,
	read_full_packet(Socket, Transport, TailDataTypes, PacketSizeInfo, Out ++ Acc).

decode_values(BinData, DataTypes) ->
	decode_values(BinData, DataTypes, []).

decode_values(<<>>, DataTypes, Values) ->
	{ok, DataTypes, lists:reverse(Values)};

decode_values(BinData, [Type|DataTypes], Values) ->
	case Type of
		bool ->
			<<B:8,Rest/binary>> = BinData,
			V = B =:= 1;
		byte   ->  <<V:8/signed,Rest/binary>> = BinData;
		ubyte  ->  <<V:8,Rest/binary>> = BinData;
		short  ->  <<V:16/signed,Rest/binary>> = BinData;
		ushort ->  <<V:16,Rest/binary>> = BinData;
		int    ->  <<V:32/signed,Rest/binary>> = BinData;
		long   ->  <<V:64/signed,Rest/binary>> = BinData;
		float  ->  <<V:32/float,Rest/binary>> = BinData;
		double ->  <<V:64/float,Rest/binary>> = BinData
	end,
	decode_values(Rest, DataTypes, [V|Values]).

read_value(Socket, Transport, string) ->
	{ok, <<Characters:16>>} = Transport:recv(Socket, 2, ?TIMEOUT),
	Size = Characters*2, %% UTF-16
	{ok, BinData} = Transport:recv(Socket, Size, ?TIMEOUT),
	[BinData].

read_value(Socket, Transport, byte_array) ->
	{ok, <<Size:16>>} = Transport:recv(Socket, 2, ?TIMEOUT),
	{ok, BinData} = Transport:recv(Socket, Size, ?TIMEOUT),
	[BinData].


% ======================================================================
% encoding
% ======================================================================

-spec encode_packet(any()) -> binary().
encode_packet(Packet) ->
	[_PacketName, PacketId | Values] = tuple_to_list(Packet),
	{ok, _, DataTypes} = minecraft_packet:get_header(PacketId),
	encode_values(Values, DataTypes).

encode_values(Values, DataTypes) ->
	encode_values(Values, DataTypes, Out).

encode_values([], [], Out) ->
	list_to_binary(lists:reverse(Out)) .

encode_values([V|Values], [Type|DataTypes], Out) ->
	O = case Type of
		bool -> encode_bool(V);
		byte   ->  <<V:8/signed>>;
		ubyte  ->  <<V:8>>;
		short  ->  <<V:16/signed>>;
		ushort ->  <<V:16>>;
		int    ->  <<V:32/signed>>;
		long   ->  <<V:64/signed>>;
		float  ->  <<V:32/float>>;
		double ->  <<V:64/float>>;
		byte_array -> V
	end,
	encode_values(Values, DataTypes, [O|Out]).

encode_bool(true) ->
	<<1>>.
encode_bool(false) ->
	<<0>>.


%% Private stuff

async_writer(Transport, Socket) ->
    receive
        {stop, Reason} ->
            exit(Reason);
        {packet, Packet} ->
            Transport:send(Socket, encode_packet(Packet)),
            async_writer(Transport, Socket)
    end.
