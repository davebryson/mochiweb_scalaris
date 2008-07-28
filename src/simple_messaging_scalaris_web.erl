%%%-------------------------------------------------------------------
%%% File    : simple_messaging_scalaris_web.erl
%%% Author  : Dave Bryson
%%% Description : 
%%%
%%% Created : 26 Jul 2008 by Dave Bryson http://weblog.miceda.org
%%%-------------------------------------------------------------------

-module(simple_messaging_scalaris_web).
-author('author Dave Bryson').

-export([start/1, stop/0, loop/1]).

%% External API

start(Options) ->
    Loop = fun (Req) ->
		   ?MODULE:loop(Req)
	   end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
	'GET' ->
	    Params = lists:sort(Req:parse_qs()),
	    handle_request(Req,Path,Params);
	'POST' ->
	    Params = lists:sort(Req:parse_post()),
	    handle_request(Req,Path,Params);
	_ ->
	    Req:not_found()
    end.
	    
%% Write a K:V pair to Scalaris
handle_request(Req,"scalaris/write",[{"key",Key},{"value",Value}]) ->
   Reply = case scalaris_proxy:write(Key,Value) of
		ok -> 
		    "ok";
		_ ->
		    "error"
	    end,
    Req:ok({"text/plain",Reply});
 
%% Read a Value given the key
handle_request(Req,"scalaris/read",[{"key",Key}]) ->
    Reply = case scalaris_proxy:read(Key) of
		{Value,_Version} ->
		    Value;
		_ ->
		    "error"
	    end,
    Req:ok({"text/plain",Reply});

handle_request(Req,_,_) ->
    Req:not_found().



