-module(minecraft_packet).

-export([get_header/1, type_size/1, packet_size/1]).

% in bytes
-spec type_size(DataType::atom()) -> non_neg_integer() | atom().
type_size(DataType) when is_atom(DataType) ->
	case DataType of
		bool -> 1;
		byte -> 1;
		ubyte -> 1;
		short -> 2;
		ushort -> 2;
		int -> 4;
		abs_int -> 4;
		float -> 4;
		double -> 8;
		long -> 8;
		_ -> DataType
	end.


-spec packet_size(DataTypes::list()) -> list().
packet_size(DataTypes) when is_list(DataTypes) ->
	packet_size(DataTypes, []);

packet_size(PacketId) when is_integer(PacketId) ->
	 {ok, _, DataTypesList} = get_header(PacketId),
	 packet_size(DataTypesList).

packet_size([], []) ->
	[];

packet_size([], Acc) ->
  [X || X <- lists:reverse(Acc), X =/= 0];

packet_size([DataType|DataTypes], []) ->
	case type_size(DataType) of  
		S when is_integer(S) -> packet_size(DataTypes, [S]);
		_ -> packet_size(DataTypes, [DataType])
	end;

packet_size([DataType|DataTypes], Acc) ->
	[H|T] = Acc,
	case type_size(DataType) of
		S when is_integer(S) -> packet_size(DataTypes, [H+S|T]);
		_ -> packet_size(DataTypes, [0|[DataType|Acc]])
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
get_header(34) -> {ok, entity_teleport, [int, abs_int, abs_int, abs_int, byte, byte]};
get_header(35) -> {ok, entity_head_look, [int, byte]};
get_header(38) -> {ok, entity_status, [int, byte]};
get_header(39) -> {ok, attach_entity, [int, int]};
get_header(40) -> {ok, entity_metadata, [int, metadata]};
get_header(41) -> {ok, entity_effect, [int, byte, byte, short]};
get_header(42) -> {ok, remove_entity_effect, [int, byte]};
get_header(43) -> {ok, set_experience, [float, short, short]};
get_header(51) -> {ok, chunk_data, [int, int, bool, ushort, ushort, int, unsigned_byte_array]};
get_header(52) -> {ok, multi_block_change, [int, int, short, int, multi_block_data]};
get_header(53) -> {ok, block_change, [int, byte, int, short, byte]};
get_header(54) -> {ok, block_action, [int, short, int, byte, byte, short]};
get_header(55) -> {ok, block_break_animation, [int, int, byte]};
get_header(56) -> {ok, map_chunk_bulk, [short, int, byte_array, chunk_bulk_meta_information]};
get_header(60) -> {ok, explosion, [double, double, double, float, int, xyz_array, float, float, float]};
get_header(61) -> {ok, sound_or_particle_effect, [int, int, byte, int, int, bool]};
get_header(62) -> {ok, named_sound_effect, [string, int, int, int, float, byte]};
get_header(70) -> {ok, change_game_state, [byte, byte]};
get_header(71) -> {ok, global_entity, [int, byte, abs_int, abs_int, abs_int]};
get_header(100) -> {ok, open_window, [byte, byte, string, byte]};
get_header(101) -> {ok, close_window, [byte]};
get_header(102) -> {ok, click_window, [byte, short, byte, short, bool, slot]};
get_header(103) -> {ok, set_slot, [byte, short, slot]};
get_header(104) -> {ok, set_window_items, [byte, short, array_of_slots]};
get_header(105) -> {ok, update_window_property, [byte, short, short]};
get_header(106) -> {ok, confirm_transaction, [byte, short, bool]};
get_header(107) -> {ok, creative_inventory_action, [short, slot]};
get_header(108) -> {ok, enchant_item, [byte, byte]};
get_header(130) -> {ok, update_sign, [int, short, int, string, string, string, string]};
get_header(131) -> {ok, item_data, [short, short, ubyte, byte_array]};
get_header(132) -> {ok, update_tile_entity, [int, short, int, byte, short, nbt_data]};
get_header(200) -> {ok, increment_statistic, [int, byte]};
get_header(201) -> {ok, player_list_item, [string, bool, short]};
get_header(202) -> {ok, player_abilities, [byte, byte, byte]};
get_header(203) -> {ok, tabcomplete, [string]};
get_header(204) -> {ok, client_settings, [string, byte, byte, byte, bool]};
get_header(205) -> {ok, client_statuses, [byte]};
get_header(250) -> {ok, plugin_message, [string, short, byte_array]};
get_header(252) -> {ok, encryption_key_response, [short, byte_array, short, byte_array]};
get_header(253) -> {ok, encryption_key_request, [string, short, byte_array, short, byte_array]};
get_header(254) -> {ok, server_list_ping, [byte]};
get_header(255) -> {ok, disconnect_kick, [string]};

get_header(A) -> {undefined, A}.
