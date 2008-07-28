%%%-------------------------------------------------------------------
%%% File    : scalaris_proxy.erl
%%% Author  : Dave Bryson
%%% Description : 
%%%
%%% Created : 26 Jul 2008 by Dave Bryson http://weblog.miceda.org
%%%-------------------------------------------------------------------
-module(scalaris_proxy).
-author('author Dave Bryson').
-behaviour(gen_server).

%% API
-export([start/1,write/2,read/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {chordnode=undefined}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(BootNode) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [BootNode], []).


%% Write a Key Value pair to Scalaris via an rpc:call
write(Key,Value) ->
    gen_server:call(?MODULE,{write,Key,Value}).

%% Read a Value from Scalaris
read(Key) ->
    gen_server:call(?MODULE,{read,Key}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([BootNode]) ->
    {ok, #state{chordnode=list_to_atom(BootNode)}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: 
%%--------------------------------------------------------------------
handle_call({write,Key,Value}, _From, #state{chordnode=Node} = State) ->
    Reply = case rpc:call(Node,transstore.transaction_api,single_write,[Key,Value]) of
		commit -> 
		    ok;
		{badrpc, Reason} ->
		    badrpc;
		_Huh  ->
		    fail
	    end,
    {reply, Reply, State};

handle_call({read,Key}, _From, #state{chordnode=Node} = State) ->
    Result = case rpc:call(Node,transstore.transaction_api,quorum_read,[Key]) of
		 {badrpc, _Reason} ->
		     badrpc;
		 {Value,Version} ->
		     {Value,Version};
		 _Huh ->
		     fail
	     end,
    {reply,Result,State}.
    

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
