-module(testdb).
-compile(export_all).

-type(int() ::integer()).

-record(pkt, {id::pos_integer()}).
-record(pkt_player_digging, {
	id = 14 ::pos_integer(),
	status ::byte(),
	x ::int(),
	y ::byte(),
	z ::int(),
	face ::byte()
	}).


info() ->
	record_info(fields, pkt_player_digging),
	record_info(size, pkt_player_digging).
	%% #pkt_player_digging{}.

get() ->
	A =  {pkt_player_digging, 14, 1, 32, 64, 32, 3},
	A#pkt.id.