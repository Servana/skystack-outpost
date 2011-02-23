%% @author author <tass@skystack.com>
%% @copyright 2010 Tass Skoudros.

%% @doc outpost start up

-module(outpost).
-author('author  <tass@skystack.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start() -> ok
%% @doc Start the skystack_web server.
start() ->
	outpost_deps:ensure(),
	ensure_started(inets),
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(gen_bunny),
	application:start(outpost),
	appmon:start(),
	ok.

%% @spec stop() -> ok
%% @doc Stop the skystack_web server.
stop() ->
	outpost_util:register(unregister),
	application:stop(crypto),
	application:stop(public_key),
	application:stop(ssl),
	application:stop(gen_bunny),
	application:stop(outpost),
    ok.
