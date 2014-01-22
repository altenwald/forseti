-module(forseti).
-behaviour(gen_leader).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/1,
    stop/0,

    get_less_used_node/0,
    search_key/1,
    save_key/3,
    get_metrics/0,
    get_key/1,

    setter_all_key/2
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

-record(state, {
    nodes :: [atom()],
    keys = dict:new() :: dict(),
    node_keys = dict:new() :: dict(),
    module :: atom()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(Nodes::[atom()]) -> {ok, pid()} | {error, term()}.

start_link(Nodes) ->
    lager:debug("Configuring gen_leader with nodes: ~p~n", [Nodes]),
    gen_leader:start_link(?SERVER, Nodes, [], ?MODULE, [Nodes], []).

-spec stop() -> ok.

stop() ->
    gen_leader:call(?MODULE, stop).

get_less_used_node() ->
    gen_leader:call(?MODULE, get_node).

search_key(Key) ->
    gen_leader:call(?MODULE, {search, Key}).

save_key(Key, Node, PID) ->
    case node() of
    Node ->
        gen_leader:cast(?MODULE, {reserve, Key, PID});
    _ ->
        rpc:call(Node, gen_leader, cast, [?MODULE, {reserve, Key, PID}])
    end. 

get_metrics() ->
    gen_leader:call(?MODULE, get_metrics).

get_key(Key) ->
    gen_leader:call(?MODULE, {get_key, Key}).

setter_all_key(Var, Value) ->
    gen_leader:call(?MODULE, {setter_all_key, Var, Value}).

%% ------------------------------------------------------------------
%% gen_leader Function Definitions
%% ------------------------------------------------------------------

elected(State, _Election, undefined) ->
    lager:info("elected this node (~p) as leader!", [node()]),
    {ok, State, State};
 
elected(#state{node_keys=NK}=State, Election, Node) ->
    lager:info("added node ~p as candidate and worker!", [Node]),
    NewState = State#state{node_keys=dict:store(Node, 0, NK)},
    gen_leader:broadcast({from_leader, NewState}, [Node], Election),
    {ok, NewState, NewState}.
 

surrendered(State, _Synch, Election) ->
    lager:debug("add this node as candidate in list: ~p", 
        [gen_leader:candidates(Election)]),
    {ok, State}.
 

handle_leader_call(_Request, _From, State, _Election) ->
    {reply, ok, State}.
 

handle_leader_cast({search, Key, From}, #state{keys=Keys}=State, _Election) ->
    lager:debug("search in leader for ~p from ~p~n", [Key, From]),
    case dict:find(Key, Keys) of
    error ->
        gen_leader:reply(From, undefined), 
        {noreply, State}; 
    {ok, {Node,PID,_Ref}} ->
        gen_leader:reply(From, {Node,PID}), 
        {noreply, State}
    end;

handle_leader_cast({get_key,Key,From}, #state{keys=Keys, node_keys=NK, nodes=Nodes}=State, _Election) ->
    lager:debug("search in leader for getting ~p from ~p~n", [Key, From]),
    {Node,PID} = case dict:find(Key, Keys) of
        error -> {undefined, undefined};
        {ok,{N,P,_Ref}} -> {N,P}
    end,
    case check(node(), Node, PID) of
    true ->
        gen_leader:reply(From, {ok, PID}),
        {noreply, State};
    _ ->
        OwnNode = node(),
        try
            {ok, NewNode, NewPID} = case get_node(NK,Nodes) of
                OwnNode ->
                    forseti_key_sup:start_child(Key);
                RemoteNode ->
                    lager:debug("create on ~p~n", [RemoteNode]),
                    rpc:call(RemoteNode, forseti_key_sup, start_child, [Key])
            end,
            gen_leader:reply(From, {ok, NewPID}),
            NewKeys = dict:store(Key, {NewNode,NewPID,undefined}, Keys),
            NewState = State#state{keys=NewKeys},
            {ok, NewState, NewState}
        catch 
            % the remote node is falling down, repeat the action
            _:{badmatch,{badrpc,_}} ->
                gen_leader:leader_cast(?MODULE, {get_key,Key,From}),
                {noreply, State}
        end
    end;

handle_leader_cast({reserve,Node,Key,PID,Ref}, #state{node_keys=NK}=State, _Election) ->
    NewNK = increment(Node, NK),
    Keys = dict:store(Key, {Node,PID,Ref}, State#state.keys),
    NewState = State#state{node_keys=NewNK,keys=Keys},
    {ok, NewState, NewState};

handle_leader_cast({free,Node,Ref}, #state{node_keys=NK, keys=Keys}=State, _Election) ->
    NewKeys = dict:filter(fun
        (_K,{_N,_P,R}) when R =:= Ref -> false;
        (_K, _V) -> true
    end, Keys),
    NewNK = case dict:find(Node, NK) of
        error -> NK;
        {ok,Value} -> dict:store(Node, Value-1, NK)
    end,
    NewState = State#state{node_keys=NewNK, keys=NewKeys},
    {ok, NewState, NewState};

handle_leader_cast(_Request, State, _Election) ->
    {noreply, State}.
 

from_leader(#state{}=State, _OldState, _Election) ->
    {ok, State};

from_leader(Info, State, _Election) ->
    lager:debug("message from the leader: ~p~n", [Info]),
    {ok, State}.
 

handle_DOWN(Node, #state{keys=Keys,node_keys=NK}=State, _Election) ->
    lager:debug("fallen node: ~p~n", [Node]),
    NewNK = dict:erase(Node, NK),
    NewKeys = dict:filter(fun
        (_Key, {N,_P,_R}) when N =/= Node -> true;
        (_Key, _Value) -> false
    end, Keys), 
    NewState = State#state{keys=NewKeys,node_keys=NewNK},
    {ok, NewState, NewState}.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Nodes]) ->
    lager:debug("Configured nodes: ~p~n", [Nodes]),
    {ok, #state{nodes=Nodes}}.

handle_call({setter_all_key, Var, Value}, _From, #state{module=Module, keys=Keys}=State, _Election) ->
    {reply, dict:fold(fun(_Key, {_Node,PID,_Ref}, Acc) ->
        catch Module:setter(PID, Var, Value),
        Acc + 1
    end, 0, Keys), State};

handle_call(get_metrics, _From, #state{node_keys=NK}=State, _Election) ->
    {reply, dict:to_list(NK), State};

handle_call({get_key, Key}, From, #state{keys=Keys}=State, _Election) ->
    lager:debug("querying for getting to node ~p about Key=~p~n", [node(), Key]),
    case dict:find(Key, Keys) of
    error ->
        gen_leader:leader_cast(?MODULE, {get_key, Key, From}),
        {noreply, State};
    {ok, {Node,PID,_Ref}} ->
        case check(node(), Node, PID) of
        true ->
            {reply, {ok,PID}, State};
        _ ->
            gen_leader:leader_cast(?MODULE, {get_key, Key, From}),
            {noreply, State}
        end
    end;

handle_call({search, Key}, From, #state{keys=Keys}=State, _Election) ->
    case dict:find(Key, Keys) of
    error ->
        gen_leader:leader_cast(?MODULE, {search, Key, From}),
        {noreply, State}; 
    {ok, {Node,PID,_Ref}} ->
        {reply, {Node,PID}, State}
    end;

handle_call(get_node, _From, #state{node_keys=NK,nodes=Nodes}=State, _Election) ->
    {reply, get_node(NK, Nodes), State};

handle_call(stop, _From, State, _Election) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State, _Election) ->
    {reply, ok, State}.

handle_cast({reserve, Key, PID}, #state{}=State, _Election) ->
    Ref = monitor(process, PID),
    gen_leader:leader_cast(?MODULE, {reserve, node(), Key, PID, Ref}), 
    {noreply, State};

handle_cast(_Msg, State, _Election) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _Object, _Info}, State, _Election) ->
    gen_leader:leader_cast(?MODULE, {free, node(), Ref}), 
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

check(_MyNode, undefined, _PID) -> false;
check(_MyNode, _Node, undefined) -> false;
check(Node, Node, PID) -> is_process_alive(PID);
check(MyNode, Node, PID) -> 
    lager:debug("mynode=~p ; node=~p ; pid=~p~n", [MyNode,Node,PID]),
    catch rpc:call(Node, erlang, is_process_alive, [PID]).

get_node(NK, Nodes) ->
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
