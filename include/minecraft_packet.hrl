-record(keep_alive, {
	id ::integer()
	}).
-record(login_request, {
	entity_id ::integer(),
	level_type ::binary()
	}).