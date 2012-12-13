%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(mblocks_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
    mblocks_sup:start_link(),
	{ok, _} = ranch:start_listener(mblocks, 100,
		ranch_tcp, [{port, 25565}], minecraft_protocol, []).

stop(_State) ->
	ok.
