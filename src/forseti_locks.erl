-module(forseti_locks).
-author('manuel@altenwald.com').
-author('guillermo@altenwald.com').

-behaviour(locks_leader).

-define(SERVER, ?MODULE).

-include("forseti.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/1,
    stop/0
]).

-export([
    init/1,
    handle_cast/3,
    handle_call/4,
    handle_info/3,
    handle_leader_call/4,
    handle_leader_cast/3,
    handle_DOWN/3,
    elected/3,
    surrendered/3,
    from_leader/3,
    code_change/4,
    terminate/2
]).

-export([
    choose_node/0,
    get_metrics/0,
    get/3,
    find/2,
    add_call/2
]).

-record(state, {
    nodes :: [atom()],
    keys = dict:new() :: ?DICT_TYPE,
    node_keys = dict:new() :: ?DICT_TYPE,
    calls = dict:new() :: ?DICT_TYPE
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

-spec start_link(Nodes::[atom()]) -> {ok, pid()} | {error, term()}.

start_link(Nodes) ->
    connect_nodes(Nodes),
    application:start(locks),
    locks_leader:start_link(?SERVER, ?SERVER, [Nodes], []).

-spec stop() -> ok.

stop() ->
    locks_leader:call(?MODULE, stop).

-spec choose_node() -> node().

choose_node() ->
    locks_leader:call(?MODULE, choose_node).

-spec get_metrics() -> [{node(), pos_integer()}].

get_metrics() ->
    locks_leader:call(?MODULE, get_metrics).

-spec get(call_name(), key(), Args::[term()]) -> {ok, pid()} | {error, reason()}.

get(Name, Key, Args) ->
    gen_server:call(forseti_locks_server, {get, Name, Key, Args}).

-spec find(call_name, key()) -> {ok, pid()} | undefined.

find(Name, Key) ->
    gen_server:call(forseti_locks_server, {find, Name, Key}).

-spec add_call(call_name(), call()) -> ok.

add_call(Name, Call) ->
    locks_leader:leader_cast(?MODULE, {add_call, Name, Call}).


%% ------------------------------------------------------------------
%% locks_leader Function Definitions
%% ------------------------------------------------------------------

elected(#state{node_keys=NK}=State, _Election, undefined) ->
    NewNK = case dict:find(node(), NK) of
        error ->  dict:store(node(), 0, NK);
        _ -> clean_dict(NK)
    end,
    {ok, {sync, NewNK}, State#state{node_keys= NewNK}};

elected(State, Election, Pid) when is_pid(Pid) ->
    elected(State, Election, node(Pid));

elected(#state{node_keys=NK}=State, Election, Node) ->
    NKeys = case dict:find(Node, NK) of
        error -> dict:store(Node, 0, NK);
        _ -> NK
    end,
    NewState = State#state{node_keys = NKeys},
    locks_leader:broadcast({from_leader, NewState, [Node]}, Election),
    {ok, {sync, NKeys}, NewState}.


surrendered(State, {sync, NK}, _Election) ->
    {ok, State#state{node_keys = NK}}.

handle_leader_call(_Request, _From, State, _Election) ->
    {reply, ok, State}.


handle_leader_cast({find, Name, Key, From}, #state{keys=Keys}=State, _Election) ->
    case dict:find({Name,Key}, Keys) of
    error ->
        gen_server:reply(From, undefined),
        {noreply, State};
    {ok, {_Node,PID}} ->
        gen_server:reply(From, {ok, PID}),
        {noreply, State}
    end;

handle_leader_cast({get,Name,Key,NewArgs,From}, #state{
        keys=Keys, node_keys=NK, nodes=Nodes}=State, _Election) ->
    {Node,PID} = case dict:find({Name,Key}, Keys) of
        error -> {undefined, undefined};
        {ok,{N,P}} -> {N,P}
    end,
    case forseti_lib:is_alive(Node, PID) of
    true ->
        gen_server:reply(From, {ok, PID}),
        {noreply, State};
    _ ->
        try
            Calls = State#state.calls,
            {Module,Function,Args} = forseti_lib:get_call(Name, Calls),
            Params = [Key|Args] ++ NewArgs,
            {NewNode, NewPID} = case rpc:call(
                    choose_node(NK,Nodes), Module, Function, Params) of
                {ok, RetNode, NewP} ->
                    {RetNode, NewP};
                {ok, NewP} ->
                    {node(NewP), NewP};
                {error, {already_started,OldP}} ->
                    {node(OldP), OldP};
                {error, _Reason} ->
                    throw(enoproc)
            end,
            NewKeys = dict:store({Name,Key}, {NewNode,NewPID}, Keys),
            NewNK = dict:update_counter(NewNode, 1, NK),
            NewState = State#state{node_keys=NewNK,keys=NewKeys},
            gen_server:reply(From, {ok, NewPID}),
            gen_server:cast(forseti_locks_server, {add, Name, Key, {NewNode,NewPID}}),
            case NewNode =:= node() of
                true -> link(NewPID);
                false -> ok
            end,
            {ok, {add, Name, Key, {NewNode, NewPID}, NewNK}, NewState}
        catch
            % the remote node is falling down, repeat the action
            _:{badmatch,{badrpc,_}} ->
                locks_leader:leader_cast(?MODULE, {get,Name,Key,NewArgs,From}),
                {noreply, State};
            _:{case_clause, _} ->
                gen_server:reply(From, {error, eprocfails}),
                {noreply, State};
            _:enoproc ->
                gen_server:reply(From, {error, enoproc}),
                {noreply, State}
        end
    end;

handle_leader_cast({free,Node,PID}, #state{node_keys=NK, keys=Keys}=State, _Election) ->
    NameKey = dict:fold(fun
        (K,{N,P},undefined) when N =:= Node andalso P =:= PID -> K;
        (_,_,P) -> P
    end, undefined, Keys),
    case NameKey of
    undefined ->
        {ok, State};
    {Name,Key} ->
        NewKeys = dict:erase(NameKey, Keys),
        NewNK = dict:update_counter(Node, -1, NK),
        NewState = State#state{node_keys=NewNK, keys=NewKeys},
        gen_server:cast(forseti_locks_server, {del, Name, Key}),
        {ok, {del, Name, Key, NewNK}, NewState}
    end;

handle_leader_cast({add_call,Name,{M,F,A}}, State, _Election) ->
    NewCalls = dict:store(Name, {M,F,A}, State#state.calls),
    {ok, {add_call,Name,{M,F,A}}, State#state{calls=NewCalls}};

handle_leader_cast(_Request, State, _Election) ->
    {noreply, State}.


from_leader({del, Name, Key, NodeKeys}, #state{keys=Keys}=State, _Election) ->
    gen_server:cast(forseti_locks_server, {del, Name, Key}),
    {ok, State#state{node_keys=NodeKeys, keys=dict:erase({Name, Key}, Keys)}};

from_leader({add, Name, Key, {Node, PID}=Value, NodeKeys},
        #state{keys=Keys}=State, _Election) when Node =:= node() ->
    link(PID),
    gen_server:cast(forseti_locks_server, {add, Name, Key, {Node, PID}}),
    {ok, State#state{node_keys=NodeKeys, keys=dict:store({Name, Key}, Value, Keys)}};

from_leader({add, Name, Key, {Node, PID}=Value, NodeKeys},
        #state{keys=Keys}=State, _Election) ->
    gen_server:cast(forseti_locks_server, {add, Name, Key, {Node, PID}}),
    {ok, State#state{node_keys=NodeKeys, keys=dict:store({Name, Key}, Value, Keys)}};

from_leader({sync, NK}, #state{} = State, _Election) ->
    gen_server:cast(forseti_locks_server, {keys, State#state.keys}),
    {ok, State#state{node_keys=NK}};

from_leader(#state{}=State, _OldState, _Election) ->
    gen_server:cast(forseti_locks_server, {keys, State#state.keys}),
    {ok, State};

from_leader({add_call, Name, Call}, State, _Election) ->
    NewCalls = dict:store(Name, Call, State#state.calls),
    {ok, State#state{calls=NewCalls}};

from_leader(_Info, State, _Election) ->
    {ok, State}.


handle_DOWN(Pid, State, Election) when is_pid(Pid) ->
    handle_DOWN(node(Pid), State, Election);

handle_DOWN(Node, #state{keys=Keys,node_keys=NK}=State, _Election) ->
    NewNK = dict:erase(Node, NK),
    NewKeys = dict:filter(fun
        (_Key, {N,_P}) when N =/= Node -> true;
        (_Key, _Value) -> false
    end, Keys),
    {ok, State#state{keys=NewKeys,node_keys=NewNK}}.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Nodes]) ->
    process_flag(trap_exit, true),
    {ok, #state{nodes=Nodes}}.

handle_call(get_metrics, _From, #state{node_keys=NK}=State, _Election) ->
    {reply, dict:to_list(NK), State};

handle_call(choose_node, _From, #state{node_keys=NK,nodes=Nodes}=State, _Election) ->
    {reply, choose_node(NK, Nodes), State};

handle_call(stop, _From, State, _Election) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State, _Election) ->
    {reply, ok, State}.

handle_cast(_Msg, State, _Election) ->
    {noreply, State}.

handle_info({'EXIT', PID, _Info}, State, _Election) ->
    locks_leader:leader_cast(?MODULE, {free, node(), PID}),
    {noreply, State};

handle_info(_Info, State, _Election) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Election, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

choose_node(NK, Nodes) ->
    A = Nodes,
    B = [node()|nodes()],
    Check = A -- (A -- B),
    NewNodeKeys = lists:foldl(fun(Node, NKs) ->
        case dict:find(Node, NK) of
        error ->
            dict:store(Node, 0, NKs);
        {ok, Value} ->
            dict:store(Node, Value, NKs)
        end
    end, dict:new(), Check),
    KeyMin = dict:fold(fun
        (Key,Val,{KeyMin,ValMin}) when Val < ValMin ->
            case lists:member(Key, Check) of
                true -> {Key,Val};
                false -> {KeyMin,ValMin}
            end;
        (_Key,_Val,{KeyMin,ValMin}) ->
            {KeyMin,ValMin};
        (Key,Val,undefined) ->
            case lists:member(Key, Check) of
                true -> {Key,Val};
                false -> undefined
            end
    end, undefined, NewNodeKeys),
    case KeyMin of
        undefined -> node();
        {K,_} -> K
    end.

connect_nodes(Nodes) ->
    [ net_kernel:connect_node(Node) || Node <- Nodes ].

clean_dict(NK) ->
    LNodes = get_active_nodes([node()|nodes()]),
    KList= dict:to_list(NK),
    F = fun(Node, Acc) ->
        case lists:keysearch(Node, 1, KList) of
        {value, Nk} -> Acc ++ [Nk];
        _ -> Acc
        end
    end,
    DelNodes = [ DN || {DN, _}  <- (KList -- lists:foldl(F,[],LNodes))],
    lists:foldl(fun(Node, K) ->
        dict:erase(Node, K)
    end,NK,DelNodes).


get_active_nodes(Nodes) ->
    connect_nodes(Nodes),
    nodes().
