-record(slot, {
	block_id ::integer(), %% -1 - empty_slot
	item_count ::integer(), %% byte
	item_damage ::integer(), %% short
	length_nbt_data ::integer(), %% short
	}).
