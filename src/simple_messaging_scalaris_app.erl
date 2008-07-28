-module(simple_messaging_scalaris_app).
-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for simple_messaging_scalaris.
start(_Type, _StartArgs) ->
    simple_messaging_scalaris_deps:ensure(),
    simple_messaging_scalaris_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for simple_messaging_scalaris.
stop(_State) ->
    ok.
