-module(forseti_locks_server).
-author('manuel@altenwald.com').
-author('guillermo@altenwald.com').

-behaviour(gen_server).

-define(SERVER, ?MODULE).

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
    keys = dict:new() :: dict()
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

handle_call({get_key, Key}, From, State) ->
    handle_call({get_key, Key, []}, From, State);

handle_call({get_key, Key, Args}, From, #state{keys=Keys}=State) ->
    case dict:find(Key, Keys) of
    error ->
        locks_leader:leader_cast(forseti_locks, {get_key, Key, Args, From}),
        {noreply, State};
    {ok, {Node,PID}} ->
        case check(node(), Node, PID) of
        true ->
            {reply, {ok,PID}, State};
        _ ->
            locks_leader:leader_cast(forseti_locks, {get_key, Key, Args, From}),
            {noreply, State}
        end
    end;

handle_call({search, Key}, From, #state{keys=Keys}=State) ->
    case dict:find(Key, Keys) of
    error ->
        locks_leader:leader_cast(forseti_locks, {search, Key, From}),
        {noreply, State}; 
    {ok, {Node,PID}} ->
        {reply, {Node,PID}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add, Key, Value}, #state{keys=Keys}=State) ->
    {noreply, State#state{keys = dict:store(Key, Value, Keys)}};

handle_cast({del, Key}, #state{keys=Keys}=State) ->
    {noreply, State#state{keys = dict:erase(Key, Keys)}};

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

check(_MyNode, undefined, _PID) -> false;
check(_MyNode, _Node, undefined) -> false;
check(Node, Node, PID) -> is_process_alive(PID);
check(_MyNode, Node, PID) -> 
    catch rpc:call(Node, erlang, is_process_alive, [PID]).
