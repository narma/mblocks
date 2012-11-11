-module(minecraft_packet).

-export([get_header/1, type_size/1, packet_size/1]).

% in bytes
%-spec type_size(DataType::atom()) -> L::non_neg_integer() | atom()
type_size(DataType) when is_atom(DataType) ->
	case DataType of
		bool -> 1;
		byte -> 1;
		short -> 2;
		int -> 4;
		float -> 4;
		double -> 8;
		long -> 8;
		_ -> DataType;
	end.

%-spec packet_size(DataTypes::list()) -> L::list()
packet_size(DataTypes) ->
	packet_size(DataTypes, []).

packet_size([], []) ->
	[];

packet_size([], [0|T]) ->
	lists:reverse(T);

packet_size([], Acc) ->
	lists:reverse(Acc);

packet_size([DataType|DataTypes], []) ->
	case type_size(DataType) of  
		S when is_integer(S) -> packet_size(DataTypes, [S]);
		A -> packet_size(DataTypes, [DataType]} 
	end;

packet_size([DataType|DataTypes], Acc) ->
	[H|T] = Acc,
	case type_size(DataType) of
		S when is_integer(S) -> packet_size(DataTypes, [H+S|T]);
		A -> packet_size(DataTypes, [0|[DataType|T]])
	end.


%% Packets get_headers, from http://www.wiki.vg/Protocol
-spec get_header(Id::non_neg_integer()) -> {ok, PacketName::atom(), [Type::atom()]} | undefined.
get_header(0) -> {ok, keep_alive, [int]}; % 2w
get_header(1) -> {ok, login_request, [int, string, byte, byte, ubyte, ubyte]}; % S-C
get_header(2) -> {ok, handshake, [byte, string, string, int]};
get_header(3) -> {ok, chat_message, [string]}; % 2w
get_header(4) -> {ok, time_update, [long, long]}; % S-C
get_header(5) -> {ok, entiry_equipment, [int, short, slot]}; % S-C
get_header(6) -> {ok, spawn_position, [int, int, int]}; % S-C
get_header(7) -> {ok, use_entity, [int, int, bool]};
get_header(8) -> {ok, update_health, [short, short, float]}; % S-C;
get_header(9) -> {ok, respawn, [int, byte, byte, short, string]}; % S-C
get_header(10) -> {ok, player_on_ground, [bool]};
get_header(11) -> {ok, player_position, [double, double, double, double, bool]};
get_header(12) -> {ok, player_look, [float, float, bool]};
get_header(13) -> {ok, player_pos_and_look, [double, double, double, double, float, float, bool]};
get_header(14) -> {ok, player_digging, [byte, int, byte, int, byte]};
get_header(15) -> {ok, player_block_placement, [int, ubyte, int, byte, slot, byte, byte, byte]};
get_header(16) -> {ok, held_item_change, [short]};
get_header(17) -> {ok, use_bed, [int, byte, int, byte, int]};
get_header(18) -> {ok, animation, [int, byte]};
get_header(19) -> {ok, entity_action, [int, byte]};
get_header(20) -> {ok, spawn_named_entity, [int, string, abs_int, abs_int, abs_int,
			                          byte, byte, short, metadata]};
get_header(21) -> {ok, spawn_dropped_item, [int, slot, abs_int, abs_int, abs_int, byte, byte, byte]};
get_header(22) -> {ok, collect_item, [int, int]};
get_header(23) -> {ok, spawn_object, [int, byte, abs_int, abs_int, abs_int, object_speed]};
get_header(24) -> {ok, spawn_mob, [int, byte, abs_int, abs_int, abs_int, 
						byte, byte, byte, short, short, short, metadata]};
get_header(25) -> {ok, spawn_painting, [int, string, int, int, int, int]};
get_header(26) -> {ok, spawn_experience_orb, [int, abs_int, abs_int, abs_int, short]};
get_header(28) -> {ok, entity_velocity, [int, short, short, short]};
get_header(29) -> {ok, destroy_entity, [byte, {array, int}]};
get_header(30) -> {ok, entity, [int]};
get_header(31) -> {ok, entity_relative_move, [int, byte, byte, byte]};
get_header(32) -> {ok, entity_look, [int, byte, byte]};
get_header(33) -> {ok, entity_look_and_relative_move, [int, byte, byte, byte, byte, byte]};
get_header(34) -> {ok, entity_teleport, [int, int, int, int]}



get_header(Id) -> undefined.