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
    start_link/2,
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
    get_key/1,
    get_key/2,
    search_key/1
]).

-record(state, {
    nodes :: [atom()],
    keys = dict:new() :: ?DICT_TYPE,
    node_keys = dict:new() :: ?DICT_TYPE,
    module :: atom(),
    function :: atom(),
    args :: [any()]
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

-spec start_link(
    {Module :: atom(), Function :: atom(), Args :: [any()]},
    Nodes::[atom()]) -> {ok, pid()} | {error, term()}.

start_link({_M,_F,_A}=Launch, Nodes) ->
    connect_nodes(Nodes),
    application:start(locks),
    locks_leader:start_link(?SERVER, ?SERVER, [Launch, Nodes], []).

-spec stop() -> ok.

stop() ->
    locks_leader:call(?MODULE, stop).

-spec choose_node() -> node().

choose_node() ->
    locks_leader:call(?MODULE, choose_node).

-spec get_metrics() -> [{node(), pos_integer()}].

get_metrics() ->
    locks_leader:call(?MODULE, get_metrics).

-spec get_key(Key::term()) -> {node(), pid()} | {error, Reason::atom()}.

get_key(Key) ->
    gen_server:call(forseti_locks_server, {get_key, Key}).

-spec get_key(
    Key::term(), Args::[term()]) ->
    {node(), pid()} | {error, Reason::atom()}.

get_key(Key, Args) ->
    gen_server:call(forseti_locks_server, {get_key, Key, Args}).

-spec search_key(Key::term()) -> {node(), pid()} | undefined.

search_key(Key) ->
    gen_server:call(forseti_locks_server, {search, Key}).

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


handle_leader_cast({search, Key, From}, #state{keys=Keys}=State, _Election) ->
    case dict:find(Key, Keys) of
    error ->
        gen_server:reply(From, undefined),
        {noreply, State};
    {ok, {Node,PID}} ->
        gen_server:reply(From, {Node,PID}),
        {noreply, State}
    end;

handle_leader_cast({get_key,Key,NewArgs,From}, #state{
        keys=Keys, node_keys=NK, nodes=Nodes,
        module=Module, function=Function, args=Args}=State, _Election) ->

    {Node,PID} = case dict:find(Key, Keys) of
        error -> {undefined, undefined};
        {ok,{N,P}} -> {N,P}
    end,
    case forseti_lib:is_alive(Node, PID) of
    true ->
        gen_server:reply(From, {ok, PID}),
        {noreply, State};
    _ ->
        try
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
            NewKeys = dict:store(Key, {NewNode,NewPID}, Keys),
            NewNK = increment(NewNode, NK),
            NewState = State#state{node_keys=NewNK,keys=NewKeys},
            gen_server:reply(From, {ok, NewPID}),
            gen_server:cast(forseti_locks_server, {add, Key, {NewNode,NewPID}}),
            case NewNode =:= node() of
                true -> link(NewPID);
                false -> ok
            end,
            {ok, {add, Key, {NewNode, NewPID}, NewNK}, NewState}
        catch
            % the remote node is falling down, repeat the action
            _:{badmatch,{badrpc,_}} ->
                locks_leader:leader_cast(?MODULE, {get_key,Key,From}),
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
    Key = dict:fold(fun
        (K,{N,P},undefined) when N =:= Node andalso P =:= PID -> K;
        (_,_,P) -> P
    end, undefined, Keys),
    case Key of
    undefined ->
        {ok, State};
    Key ->
        NewKeys = dict:erase(Key, Keys),
        NewNK = case dict:find(Node, NK) of
            error -> NK;
            {ok,Value} -> dict:store(Node, Value-1, NK)
        end,
        NewState = State#state{node_keys=NewNK, keys=NewKeys},
        gen_server:cast(forseti_locks_server, {del, Key}),
        {ok, {del, Key, NewNK}, NewState}
    end;

handle_leader_cast(_Request, State, _Election) ->
    {noreply, State}.


from_leader({del, Key, NodeKeys}, #state{keys=Keys}=State, _Election) ->
    gen_server:cast(forseti_locks_server, {del, Key}),
    {ok, State#state{node_keys=NodeKeys, keys=dict:erase(Key, Keys)}};

from_leader({add, Key, {Node, PID}=Value, NodeKeys},
        #state{keys=Keys}=State, _Election) when Node =:= node() ->
    link(PID),
    gen_server:cast(forseti_locks_server, {add, Key, {Node, PID}}),
    {ok, State#state{node_keys=NodeKeys, keys=dict:store(Key, Value, Keys)}};

from_leader({add, Key, {Node, PID}=Value, NodeKeys},
        #state{keys=Keys}=State, _Election) ->
    gen_server:cast(forseti_locks_server, {add, Key, {Node, PID}}),
    {ok, State#state{node_keys=NodeKeys, keys=dict:store(Key, Value, Keys)}};

from_leader({sync, NK}, #state{} = State, _Election) ->
    gen_server:cast(forseti_locks_server, {keys, State#state.keys}),
    {ok, State#state{node_keys=NK}};

from_leader(#state{}=State, _OldState, _Election) ->
    gen_server:cast(forseti_locks_server, {keys, State#state.keys}),
    {ok, State};

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

init([{Module,Function,Args}, Nodes]) ->
    process_flag(trap_exit, true),
    {ok, #state{
        nodes=Nodes,
        module=Module,
        function=Function,
        args=Args}}.

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

increment(Node, NK) ->
    case dict:find(Node, NK) of
        error ->
            dict:store(Node, 1, NK);
        {ok, Value} ->
            dict:store(Node, Value+1, NK)
    end.

connect_nodes([]) -> ok;
connect_nodes([Node|Nodes]) ->
    net_kernel:connect_node(Node),
    connect_nodes(Nodes).


clean_dict(NK) ->
    LNodes = get_active_nodes([node()|nodes()]),
    KList= dict:to_list(NK),
    F = fun(Node, Acc) ->
        case lists:keysearch(Node, 1, KList) of
        {value, Nk} -> Acc ++ [Nk];
        _ -> Acc
        end end,
    DelNodes = [ DN || {DN, _}  <- (KList -- lists:foldl(F,[],LNodes))],
    lists:foldl(fun(Node, K) ->
        dict:erase(Node, K)
    end,NK,DelNodes).


get_active_nodes(Nodes) ->
    lists:foldl(fun(Node, Acc) ->
        case net_kernel:connect_node(Node) of
        true -> Acc ++ [Node];
        _ -> Acc
    end end, [], Nodes).