%% See Protocol Encryption for information on logging in.

-record(handshake, {
  pkt = 2 :: integer(),
  protocol_version ::integer(),
  username ::binary(),
  server_host ::binary(),
  server_port ::integer()
}).


%% This packet is sent from the client to the server when the client attacks or right-clicks another entity (a player, minecart, etc).

%% A Notchian server only accepts this packet if the entity being attacked/used is visible without obstruction and within a 4-unit radius of the player's position.

-record(use_entity, {
  pkt = 7 :: integer(),
  user ::integer(),
  target ::integer(),
  mouse_button ::boolean()
}).


%% This packet is used to indicate whether the player is on ground (walking/swimming), or airborne (jumping/falling).

%% When dropping from sufficient height, fall damage is applied when this state goes from False to True. The amount of damage applied is based on the point where it last changed from True to False. Note that there are several movement related packets containing this state.

%% This packet was previously referred to as Flying

-record(player, {
  pkt = 10 :: integer(),
  on_ground ::boolean()
}).


%% Updates the players XYZ position on the server. 
%% If Stance - Y is less than 0.1 or greater than 1.65, the stance is illegal and the client will be kicked with the message “Illegal Stance”.
%% If the distance between the last known position of the player on the server and the new position set by this packet is greater than 100 units will result in the client being kicked for "You moved too quickly :( (Hacking?)"
%% Also if the absolute number of X or Z is set greater than 3.2E7D the client will be kicked for "Illegal position"

%% 

-record(player_position, {
  pkt = 11 :: integer(),
  x ::float(),
  y ::float(),
  stance ::float(),
  z ::float(),
  on_ground ::boolean()
}).


-record(player_look, {
  pkt = 12 :: integer(),
  yaw ::float(),
  pitch ::float(),
  on_ground ::boolean()
}).


%% Sent when the player mines a block.  A Notchian server only accepts digging packets with coordinates within a 6-unit radius of the player's position.

-record(player_digging, {
  pkt = 14 :: integer(),
  status ::integer(),
  x ::integer(),
  y ::integer(),
  z ::integer(),
  face ::integer()
}).


-record(player_block_placement, {
  pkt = 15 :: integer(),
  x ::integer(),
  y ::non_neg_integer(),
  z ::integer(),
  direction ::integer(),
  held_item,
  cursor_position_x ::integer(),
  cursor_position_y ::integer(),
  cursor_position_z ::integer()
}).


%% Sent when the player changes the slot selection

-record(held_item_change, {
  pkt = 16 :: integer(),
  slot_id ::integer()
}).


%% Sent at least when crouching, leaving a bed, or sprinting.
%% To send action animation to client use 0x28.
%% The client will send this with Action ID = 3 when "Leave Bed" is clicked.

-record(entity_action, {
  pkt = 19 :: integer(),
  eid ::integer(),
  action_id ::integer()
}).


%% This packet is sent by the player when it clicks on a slot in a window.

-record(click_window, {
  pkt = 102 :: integer(),
  window_id ::integer(),
  slot ::integer(),
  mouse_button ::integer(),
  action_number ::integer(),
  shift ::boolean(),
  clicked_item
}).


-record(enchant_item, {
  pkt = 108 :: integer(),
  window_id ::integer(),
  enchantment ::integer()
}).


%% Sent when the player connects, or when settings are changed.

-record(client_settings, {
  pkt = 204 :: integer(),
  locale ::binary(),
  view_distance ::integer(),
  chat_flags ::integer(),
  difficulty ::integer(),
  show_cape ::boolean()
}).


%% Sent when the client is ready to complete login and when the client is ready to respawn after death.

-record(client_statuses, {
  pkt = 205 :: integer(),
  payload ::integer()
}).


%% This packet is used by the multiplayer menu to retrieve MOTD, version, and player counts. For more info see Server List Ping

-record(server_list_ping, {
  pkt = 254 :: integer(),
  magic ::integer()
}).


