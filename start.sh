#!/bin/sh
erl -pa ebin deps/*/ebin -s mblocks \
	-eval "io:format(\"Run: telnet localhost 25565~n\")."
