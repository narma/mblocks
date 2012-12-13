%% See Protocol Encryption for information on logging in.

-record(login_request, {
  pkt = 1 :: integer(),
  entity_id ::integer(),
  level_type ::binary(),
  game_mode ::integer(),
  dimension ::integer(),
  difficulty ::integer(),
  not_used ::integer(),
  max_players ::integer()
}).


%% Time is based on ticks, where 20 ticks happen every second. There are 24000 ticks in a day, making Minecraft days exactly 20 minutes long.

%% The time of day is based on the timestamp modulo 24000. 0 is sunrise, 6000 is noon, 12000 is sunset, and 18000 is midnight.

%% The default SMP server increments the time by 20 every second.

-record(time_update, {
  pkt = 4 :: integer(),
  age_of_the_world ::integer(),
  time_of_day ::integer()
}).


-record(entity_equipment, {
  pkt = 5 :: integer(),
  entity_id ::integer(),
  slot ::integer(),
  item
}).


%% Sent by the server after login to specify the coordinates of the spawn point (the point at which players spawn at, and which the compass points to). It can be sent at any time to update the point compasses point at.

-record(spawn_position, {
  pkt = 6 :: integer(),
  x ::integer(),
  y ::integer(),
  z ::integer()
}).


%% Sent by the server to update/set the health of the player it is sent to. Added in protocol version 5.

%% Food saturation acts as a food "overcharge". Food values will not decrease while the saturation is over zero. Players logging in automatically get a saturation of 5.0. Eating food increases the saturation as well as the food bar.

-record(update_health, {
  pkt = 8 :: integer(),
  health ::integer(),
  food ::integer(),
  food_saturation ::float()
}).


%% To change the player's dimension (overworld/nether/end), send them a respawn packet with the appropriate dimension, followed by prechunks/chunks for the new dimension, and finally a position and look packet.  You do not need to unload chunks, the client will do it automatically.

-record(respawn, {
  pkt = 9 :: integer(),
  dimension ::integer(),
  difficulty ::integer(),
  game_mode ::integer(),
  world_height ::integer(),
  level_type ::binary()
}).


%% This packet tells that a player goes to bed.

%% The client with the matching  Entity ID will go into bed mode.

%% This Packet is sent to all nearby players including the one sent to bed.

-record(use_bed, {
  pkt = 17 :: integer(),
  entity_id ::integer(),
  unknown ::integer(),
  x_coordinate ::integer(),
  y_coordinate ::integer(),
  z_coordinate ::integer()
}).


%% The only named entities (at the moment) are players (either real or NPC/Bot). This packet is sent by the server when a player comes into visible range, not when a player joins.

%% Servers can, however, safely spawn player entities for players not in visible range. The client appears to handle it correctly.

%% At one point, the Notchian client was not okay with receiving player entity packets, including 0x14, that refer to its own username or EID; and would teleport to the absolute origin of the map and fall through the Void any time it received them. However, in more recent versions, it appears to handle them correctly, by spawning a new entity as directed (though future packets referring to the entity ID may be handled incorrectly).

-record(spawn_named_entity, {
  pkt = 20 :: integer(),
  eid ::integer(),
  player_name ::binary(),
  x ::integer(),
  y ::integer(),
  z ::integer(),
  yaw ::integer(),
  pitch ::integer(),
  current_item ::integer(),
  metadata
}).


%% An 0x15 packet is sent by the server whenever an item on the ground (say a pickaxe thrown on the ground) comes into range of the player. (note: this means range for item vision, not range for pickup!) It used to be sent by the client when an item is dropped from a tile (chest or furnace) or from inventory, but that is now done with the new packets for server-side inventory (see Window click (0x66)).

%% It is completely acceptable for servers to ignore the EID issued by the client in this packet and instead create a new packet with a server-controlled EID when sending this packet out to clients.

-record(spawn_dropped_item, {
  pkt = 21 :: integer(),
  eid ::integer(),
  slot,
  x ::integer(),
  y ::integer(),
  z ::integer(),
  rotation ::integer(),
  pitch ::integer(),
  roll ::integer()
}).


%% Sent by the server when someone picks up an item lying on the ground - its sole purpose appears to be the animation of the item flying towards you. It doesn't destroy the entity in the client memory (0x1D does that), and it doesn't add it to your inventory (0x68 does that). The server only checks for items to be picked up after each Player Position and Player Position & Look packet sent by the client.

-record(collect_item, {
  pkt = 22 :: integer(),
  collected_eid ::integer(),
  collector_eid ::integer()
}).


%% Sent by the server when an Object/Vehicle is created. The throwers entity id is now used for fishing floats too.

-record(spawn_object_vehicle, {
  pkt = 23 :: integer(),
  eid ::integer(),
  type ::integer(),
  x ::integer(),
  y ::integer(),
  z ::integer(),
  object_data
}).


%% Sent by the server when a Mob Entity is Spawned

-record(spawn_mob, {
  pkt = 24 :: integer(),
  eid ::integer(),
  type ::integer(),
  x ::integer(),
  y ::integer(),
  z ::integer(),
  yaw ::integer(),
  pitch ::integer(),
  head_yaw ::integer(),
  velocity_z ::integer(),
  velocity_x ::integer(),
  velocity_y ::integer(),
  metadata
}).


%% This packet shows location, name, and type of painting.

-record(spawn_painting, {
  pkt = 25 :: integer(),
  entity_id ::integer(),
  title ::binary(),
  x ::integer(),
  y ::integer(),
  z ::integer(),
  direction ::integer()
}).


%% Spawns one or more experience orbs. Coordinates are in absolute units.

-record(spawn_experience_orb, {
  pkt = 26 :: integer(),
  entity_id ::integer(),
  x ::integer(),
  y ::integer(),
  z ::integer(),
  count ::integer()
}).


%% This packet is new to version 4 of the protocol, and is believed to be Entity Velocity/Motion.

%% Velocity is believed to be in units of 1/32000 of a block per server tick (200ms);
%% for example, -1343 would move (-1343 / 32000) = -0.04196875 blocks per tick (or -0.20984375 blocks per second).

%% Each axis' velocity is capped between -0.9 and 0.9 blocks per tick (packet values -28800 to 28800).

%% (This packet data values are not fully verified)

-record(entity_velocity, {
  pkt = 28 :: integer(),
  entity_id ::integer(),
  velocity_x ::integer(),
  velocity_y ::integer(),
  velocity_z ::integer()
}).


%% Sent by the server when an list of Entities is to be destroyed on the client.

-record(destroy_entity, {
  pkt = 29 :: integer(),
  entity_count ::integer(),
  entity_ids
}).


%% Most entity-related packets are subclasses of this packet. When sent from the server to the client, it may initialize the entry.

%% For player entities, either this packet or any move/look packet is sent every game tick.
%% So the meaning of this packet is basically that the entity did not move/look since the last such packet.

-record(entity, {
  pkt = 30 :: integer(),
  eid ::integer()
}).


%% This packet is sent by the server when an entity moves less then 4 blocks; if an entity moves more than 4 blocks Entity Teleport should be sent instead.

%% This packet allows at most four blocks movement in any direction, because byte range is from -128 to 127. Movement is an offset of Absolute Int; to convert relative move to block coordinate offset, divide by 32.

-record(entity_relative_move, {
  pkt = 31 :: integer(),
  eid ::integer(),
  dx ::integer(),
  dy ::integer(),
  dz ::integer()
}).


%% This packet is sent by the server when an entity rotates.  Example: "Yaw" field 64 means a 90 degree turn.

-record(entity_look, {
  pkt = 32 :: integer(),
  eid ::integer(),
  yaw ::integer(),
  pitch ::integer()
}).


%% This packet is sent by the server when an entity rotates and moves.
%% Since a byte range is limited from -128 to 127, and movement is offset of Absolute Int,
%% this packet allows at most four blocks movement in any direction. (-128/32 == -4)

-record(entity_look_and_relative_move, {
  pkt = 33 :: integer(),
  eid ::integer(),
  dx ::integer(),
  dy ::integer(),
  dz ::integer(),
  yaw ::integer(),
  pitch ::integer()
}).


%% This packet is sent by the server when an entity moves more than 4 blocks.

-record(entity_teleport, {
  pkt = 34 :: integer(),
  eid ::integer(),
  x ::integer(),
  y ::integer(),
  z ::integer(),
  yaw ::integer(),
  pitch ::integer()
}).


%% Changes the direction an entity's head is facing.

-record(entity_head_look, {
  pkt = 35 :: integer(),
  entity_id ::integer(),
  head_yaw ::integer()
}).


-record(entity_status, {
  pkt = 38 :: integer(),
  entity_id ::integer(),
  entity_status ::integer()
}).


%% This packet is new to version 4 of the protocol, and is believed to be Attach Entity.

%% This packet is sent when a player has been attached to an entity (e.g. Minecart)

%% (This packet data values are not fully verified)

-record(attach_entity, {
  pkt = 39 :: integer(),
  entity_id ::integer(),
  vehicle_id ::integer()
}).


-record(entity_metadata, {
  pkt = 40 :: integer(),
  entity_id ::integer(),
  entity_metadata
}).


-record(entity_effect, {
  pkt = 41 :: integer(),
  entity_id ::integer(),
  effect_id ::integer(),
  amplifier ::integer(),
  duration ::integer()
}).


-record(remove_entity_effect, {
  pkt = 42 :: integer(),
  entity_id ::integer(),
  effect_id ::integer()
}).


%% Sent by the server when the client should change experience levels.

-record(set_experience, {
  pkt = 43 :: integer(),
  experience_bar ::float(),
  level ::integer(),
  total_experience ::integer()
}).


%% See also: Map Format

%% Chunks are sent a column at a time, with some sections optionally missing from each packet (those consisting only of air).

-record(chunk_data, {
  pkt = 51 :: integer(),
  x ::integer(),
  z ::integer(),
  groundup_continuous ::boolean(),
  primary_bit_map ::non_neg_integer(),
  add_bit_map ::non_neg_integer(),
  compressed_size ::integer(),
  compressed_data
}).


-record(multi_block_change, {
  pkt = 52 :: integer(),
  chunk_x ::integer(),
  chunk_z ::integer(),
  record_count ::integer(),
  data_size ::integer(),
  data
}).


-record(block_change, {
  pkt = 53 :: integer(),
  x ::integer(),
  y ::integer(),
  z ::integer(),
  block_type ::integer(),
  block_metadata ::integer()
}).


%% This packet is used for a number of things:

-record(block_action, {
  pkt = 54 :: integer(),
  x ::integer(),
  y ::integer(),
  z ::integer(),
  byte_1 ::integer(),
  byte_2 ::integer(),
  block_id ::integer()
}).


-record(block_break_animation, {
  pkt = 55 :: integer(),
  eid ::integer(),
  x ::integer(),
  destroy_stage ::integer()
}).


%% To reduce the number of bytes this packet is used to send chunks together for better compression results. The packet contains up to 100 chunks (later this might be reduced to 50).

%% The data part is a zlib compressed byte array containing the chunk data. The meta data part specifies which chunks in which order the data part exists of.

%% To split this packet into chunks you need to uncompress the data array. Then you can iterate through the data part. Each part is 10240 * n + 256 bytes. n is the number of sections in the current chunk (this is the number of flags set in the primary bitmap). 10240 is the amount of bytes for each chunk without add bitmap, 256 bytes are used for biomes. The second short in the meta data part is not yet in use. It could specify if the chunk uses the add bitmap part, because it has very high block ids, but not in the current snapshot.

-record(map_chunk_bulk, {
  pkt = 56 :: integer(),
  chunk_count ::integer(),
  chunk_data_length ::integer(),
  data,
  meta_information
}).


%% Sent when an explosion occurs (creepers, TNT, and ghast fireballs).

-record(explosion, {
  pkt = 60 :: integer(),
  x ::float(),
  y ::float(),
  z ::float(),
  radius ::float(),
  record_count ::integer(),
  records,
  player_motion_x ::float(),
  player_motion_y ::float(),
  player_motion_z ::float()
}).


%% Sent when a client is to play a sound or particle effect.

%% By default, the minecraft client adjusts the volume of sound effects based on distance. The final boolean field is used to disable this, and instead the effect is played from 2 blocks away in the correct direction. Currently this is only used for effect 1013 (mob.wither.spawn), and is ignored for any other value by the client.

-record(sound_or_particle_effect, {
  pkt = 61 :: integer(),
  effect_id ::integer(),
  x ::integer(),
  y ::integer(),
  z ::integer(),
  data ::integer(),
  no_volume_decrease ::boolean()
}).


%% Used to play a sound effect on the client.

%% All known sound effect names can be seen here.

-record(named_sound_effect, {
  pkt = 62 :: integer(),
  sound_name ::binary(),
  effect_position_x ::integer(),
  effect_position_y ::integer(),
  effect_position_z ::integer(),
  volume ::float(),
  pitch ::integer()
}).


%% This packet appeared with protocol version 10. Currently, it appears when a bed can't be used as a spawn point and when the rain state changes.  it could have additional uses in the future.

%% The class has an array of strings linked to reason codes 0, 1, 2, and 3 but only the codes for 1 and 2 are null.

-record(change_game_state, {
  pkt = 70 :: integer(),
  reason ::integer(),
  game_mode ::integer()
}).


%% With this packet, the server notifies the client of thunderbolts striking within a 512 block radius around the player. The coordinates specify where exactly the thunderbolt strikes.

-record(global_entity, {
  pkt = 71 :: integer(),
  entity_id ::integer(),
  id ::integer(),
  x ::integer(),
  y ::integer(),
  z ::integer()
}).


%% This is sent to the client when it should open an inventory, such as a chest, workbench, or furnace. This message is not sent anywhere for clients opening their own inventory.

-record(open_window, {
  pkt = 100 :: integer(),
  window_id ::integer(),
  inventory_type ::integer(),
  window_title ::binary(),
  number_of_slots ::integer()
}).


%% Sent by the server when an item in a slot (in a window) is added/removed.

-record(set_slot, {
  pkt = 103 :: integer(),
  window_id ::integer(),
  slot ::integer(),
  slot_data
}).


-record(set_window_items, {
  pkt = 104 :: integer(),
  window_id ::integer(),
  count ::integer(),
  slot_data
}).


-record(update_window_property, {
  pkt = 105 :: integer(),
  window_id ::integer(),
  property ::integer(),
  value ::integer()
}).


%% Sent to specify complex data on an item; currently used only for maps.

-record(item_data, {
  pkt = 131 :: integer(),
  item_type ::integer(),
  item_id ::integer(),
  text_length ::integer(),
  text
}).


%% Essentially a block update on a tile entity.

-record(update_tile_entity, {
  pkt = 132 :: integer(),
  x ::integer(),
  y ::integer(),
  z ::integer(),
  action ::integer(),
  data_length ::integer(),
  nbt_data
}).


-record(increment_statistic, {
  pkt = 200 :: integer(),
  statistic_id ::integer(),
  amount ::integer()
}).


%% Sent by the notchian server to update the user list (<tab> in the client). The server sends one packet per user per tick, amounting to 20 packets/s for 1 online user, 40 for 2, and so forth.

-record(player_list_item, {
  pkt = 201 :: integer(),
  player_name ::binary(),
  online ::boolean(),
  ping ::integer()
}).


%% See Protocol Encryption for information on this packet.

-record(encryption_key_request, {
  pkt = 253 :: integer(),
  server_id ::binary(),
  public_key_length ::integer(),
  public_key,
  verify_token_length ::integer(),
  verify_token
}).


