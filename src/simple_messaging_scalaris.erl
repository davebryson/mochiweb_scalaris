-module(simple_messaging_scalaris).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
	
%% @spec start() -> ok
%% @doc Start the simple_messaging_scalaris server.
start() ->
    simple_messaging_scalaris_deps:ensure(),
    ensure_started(crypto),
    application:start(simple_messaging_scalaris).

%% @spec stop() -> ok
%% @doc Stop the simple_messaging_scalaris server.
stop() ->
    Res = application:stop(simple_messaging_scalaris),
    application:stop(crypto),
    Res.
