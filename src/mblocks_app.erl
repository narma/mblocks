%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(mblocks_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	{ok, _} = ranch:start_listener(mblocks, 1,
		ranch_tcp, [{port, 25565}], minecraft_protocol, []),
	mblocks_sup:start_link().

stop(_State) ->
	ok.
