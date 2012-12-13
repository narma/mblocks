%% The server will frequently send out a keep-alive, each containing a random ID. The client must respond with the same packet.
%% The Beta server will disconnect a client if it doesn't receive at least one packet before 1200 in-game ticks, and the Beta client will time out the connection under the same conditions. The client may send packets with Keep-alive ID=0.

-record(keep_alive, {
  pkt = 0 :: integer(),
  keepalive_id ::integer()
}).


%% The default server will check the message to see if it begins with a '/'. If it doesn't, the username of the sender is prepended and sent to all other clients (including the original sender). If it does, the server assumes it to be a command and attempts to process it. A message longer than 100 characters will cause the server to kick the client. (As of 1.3.2, the vanilla client appears to limit the text a user can enter to 100 charaters.) This limits the chat message packet length to 103 bytes. Note that this limit does not apply to incoming chat messages as the server may have prepended other information, not limited to, but usually including, a username.

%% A message longer than 119 characters will cause the server and client to print the message "Received string length longer than maximum allowed (X > 119)", with no side effects.

%% For more information, see Chat.

-record(chat_message, {
  pkt = 3 :: integer(),
  message ::binary()
}).


%% A combination of Player Look and Player position.

%% Client to Server

-record(player_position_and_look, {
  pkt = 13 :: integer(),
  x ::float(),
  y ::float(),
  stance ::float(),
  z ::float(),
  yaw ::float(),
  pitch ::float(),
  on_ground ::boolean()
}).


%% A combination of Player Look and Player position.

%% Client to Server

-record(player_position_and_look, {
  pkt = 13 :: integer(),
  x ::float(),
  stance ::float(),
  y ::float(),
  z ::float(),
  yaw ::float(),
  pitch ::float(),
  on_ground ::boolean()
}).


%% Sent whenever an entity should change animation.

-record(animation, {
  pkt = 18 :: integer(),
  eid ::integer(),
  animation ::integer()
}).


%% This packet is sent by the client when closing a window. This packet is sent from the server to the client when a window is forcibly closed, such as when a chest is destroyed while it's open.

%% Note, notchian clients send a close window message with window id 0 to close their inventory even though there is never an Open Window message for inventory.

-record(close_window, {
  pkt = 101 :: integer(),
  window_id ::integer()
}).


%% A packet from the server indicating whether a request from the client was accepted, or whether there was a conflict (due to lag). This packet is also sent from the client to the server in response to a server transaction rejection packet.

-record(confirm_transaction, {
  pkt = 106 :: integer(),
  window_id ::integer(),
  action_number ::integer(),
  accepted ::boolean()
}).


%% While the user is in the standard inventory (i.e., not a crafting bench) on a creative-mode server then the server will send this packet:

-record(creative_inventory_action, {
  pkt = 107 :: integer(),
  slot ::integer(),
  clicked_item
}).


-record(update_sign, {
  pkt = 130 :: integer(),
  x ::integer(),
  y ::integer(),
  z ::integer(),
  text ::binary(),
  text ::binary(),
  text ::binary(),
  text ::binary()
}).


%% The latter 2 bytes are used to indicate the walking and flying sppeds respectively, while the first byte is used to determine the value of 4 booleans.

%% These booleans are whether damage is disabled (god mode), whether the player is flying, whether the player can fly, and whether the player is in creative mode.

%% To get the values of these booleans, simply AND (&) the byte with 1,2,4 and 8 respectively, to get the 0 or 1 bitwise value. To set them OR (|) them with their repspective masks.
%% The vanilla client sends this packet when the player starts/stops flying with the second parameter changed accordingly. All other parameters are ignored by the vanilla server.

-record(player_abilities, {
  pkt = 202 :: integer(),
  flags ::integer(),
  flying_speed ::integer(),
  walking_speed ::integer()
}).


%% Sent C->S when the user presses [tab] while writing text. The payload contains all text behind the cursor.

%% The server responds with an auto-completion of the last word sent to it. In the case of regular chat, this is a player username. Command names and parameters are also supported.

%% In the event of more than one possible completion, the server responds with the options packed into the single string field, separated by a null character. Note that as strings are UTF-16, this is two bytes wide.

-record(tabcomplete, {
  pkt = 203 :: integer(),
  text ::binary()
}).


%% Mods and plugins can use this to send their data. As of 1.3, Minecraft itself uses a number of plugin channels. These internal channels are prefixed with MC|.

-record(plugin_message, {
  pkt = 250 :: integer(),
  channel ::binary(),
  length ::integer(),
  data
}).


%% See Protocol Encryption for information on this packet. Bypassing the encryption is possible, authentication for the player name is still needed if the server is in online mode, but instead of sending this packet, you send Client Statuses instead.

-record(encryption_key_response, {
  pkt = 252 :: integer(),
  shared_secret_length ::integer(),
  shared_secret ::binary(),
  verify_token_length ::integer(),
  verify_token_response ::binary()
}).


%% Sent by the server before it disconnects a client, or by the client before it disconnects from the server. The receiver of this packet assumes that the sender has already closed the connection by the time the packet arrives.

%% Due to race conditions in the client, a local server may need to pause for a short period after sending this packet before closing the connection. An alternative is simply not to close the connection, and wait for the client to do so on receipt of this packet.

-record(disconnect_kick, {
  pkt = 255 :: integer(),
  reason ::binary()
}).


