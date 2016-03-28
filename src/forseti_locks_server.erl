-module(forseti_locks_server).
-author('manuel@altenwald.com').
-author('guillermo@altenwald.com').

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("forseti.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    stop/0
]).

-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(state, {
    keys = dict:new() :: ?DICT_TYPE
}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifndef(TEST).
-define(debugFmt(A,B), (ok)).
-endif.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.

stop() ->
    gen_server:call(?MODULE, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({get, Name, Key}, From, State) ->
    handle_call({get, Name, Key, []}, From, State);

handle_call({get, Name, Key, Args}, From, #state{keys=Keys}=State) ->
    case dict:find({Name,Key}, Keys) of
    error ->
        locks_leader:leader_cast(forseti_locks, {get, Name, Key, Args, From}),
        {noreply, State};
    {ok, {Node,PID}} ->
        case forseti_lib:is_alive(Node, PID) of
        true ->
            {reply, {ok,PID}, State};
        _ ->
            locks_leader:leader_cast(forseti_locks, {get, Name, Key, Args, From}),
            {noreply, State}
        end
    end;

handle_call({find, Name, Key}, From, #state{keys=Keys}=State) ->
    case dict:find({Name, Key}, Keys) of
    error ->
        locks_leader:leader_cast(forseti_locks, {find, Name, Key, From}),
        {noreply, State};
    {ok, {_Node,PID}} ->
        {reply, {ok,PID}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add, Name, Key, Value}, #state{keys=Keys}=State) ->
    {noreply, State#state{keys = dict:store({Name,Key}, Value, Keys)}};

handle_cast({del, Name, Key}, #state{keys=Keys}=State) ->
    {noreply, State#state{keys = dict:erase({Name,Key}, Keys)}};

handle_cast({keys, Keys}, State) ->
    {noreply, State#state{keys=Keys}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
