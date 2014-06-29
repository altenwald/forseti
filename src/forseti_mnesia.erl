-module(forseti_mnesia).
-author('manuel@altenwald.com').

-behaviour(gen_server).

-export([
    start_link/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    choose_node/0,
    get_metrics/0,
    get_key/2,
    get_key/1,
    search_key/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifndef(TEST).
-define(debugFmt(A,B), (ok)).
-endif.

-type method() :: transaction | dirty.
-type key() :: term().

-record(forseti_processes, {
    key :: term(),
    node = node() :: node(),
    process :: pid()
}).

-record(forseti_nodes, {
    node :: node(),
    proc_len = 0 :: pos_integer()
}).


-spec start_link(mfa(), [node()]) -> {ok, pid()} | {error, term()}.

start_link(Call, Nodes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Call, Nodes], []). 

-spec choose_node() -> node().

choose_node() ->
    ActiveFull = get_metrics(),
    KeyMin = lists:foldl(fun
        ({Key,Val},{KeyMin,ValMin}) when Val < ValMin ->
            {Key,Val};
        ({_Key,_Val},{KeyMin,ValMin}) ->
            {KeyMin,ValMin};
        ({Key,Val},undefined) ->
            {Key,Val}
    end, undefined, ActiveFull),
    case KeyMin of
        undefined -> node();
        {K,_} -> K
    end.

-spec get_metrics() -> [{node(), pos_integer()}].

get_metrics() ->
    MnesiaConf = application:get_env(forseti, mnesia, []),
    Method = proplists:get_value(method, MnesiaConf, transaction),
    get_metrics(Method).

-spec get_key(Key::term()) -> {node(), pid()} | {error, Reason::atom()}.

get_key(Key) ->
    get_key(Key, []).

-spec get_key(Key::term(), Args::[term()]) ->
    {node(), pid()} | {error, Reason::atom()}.

get_key(Key, Args) ->
    MnesiaConf = application:get_env(forseti, mnesia, []),
    {ok, {M,F,A}} = application:get_env(forseti, call),
    Method = proplists:get_value(method, MnesiaConf, transaction),
    Params = [Key|A] ++ Args,
    get_key(Method, Key, M, F, Params).

-spec search_key(Key::term()) -> {node(), pid()} | undefined.

search_key(Key) ->
    MnesiaConf = application:get_env(forseti, mnesia, []),
    Method = proplists:get_value(method, MnesiaConf, transaction),
    find_key(Method, Key).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([_Call, Nodes]) ->
    process_flag(trap_exit, true), 
    init_db(Nodes),
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, State}.

handle_cast({link,PID}, State) ->
    link(PID),
    ?debugFmt("link pid=~p (for node=~p) in node=~p",
        [PID, node(PID), node()]),
    {noreply, State}.

handle_info({'EXIT', PID, _Info}, State) ->
    MnesiaConf = application:get_env(forseti, mnesia, []),
    Method = proplists:get_value(method, MnesiaConf, transaction),
    ?debugFmt("release pid=~p (from node=~p) by node=~p",
        [PID, node(PID), node()]),
    release(Method, node(PID), PID),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec init_db([node()]) -> ok.

init_db([]) ->
    mnesia:create_schema([node()]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:start(),
    mnesia:create_table(forseti_processes, [
        {attributes, record_info(fields, forseti_processes)}]),
    mnesia:create_table(forseti_nodes, [
        {attributes, record_info(fields, forseti_nodes)}]),
    FN = #forseti_nodes{node=node()},
    ?debugFmt("saving ~p", [FN]),
    mnesia:dirty_write(FN),
    ok;

init_db([Node|Nodes]) when Node =:= node() ->
    init_db(Nodes);

init_db([Node|Nodes]) ->
    net_kernel:connect_node(Node), 
    case catch rpc:call(Node, mnesia, system_info, [running_db_nodes]) of
        NodeList when length(NodeList) >= 1 ->
            mnesia:delete_schema([node()]),
            mnesia:start(),
            mnesia:change_config(extra_db_nodes, [Node]),
            mnesia:change_table_copy_type(schema, node(), disc_copies), 
            mnesia:add_table_copy(forseti_processes, node(), ram_copies),
            mnesia:add_table_copy(forseti_nodes, node(), ram_copies), 
            FN = #forseti_nodes{node=node()},
            ?debugFmt("saving ~p", [FN]),
            mnesia:dirty_write(FN),
            ok;
        _ ->
            init_db(Nodes)
    end.

get_key(transaction, Key, M, F, Params) ->
    {atomic, Result} = mnesia:transaction(fun() -> 
        FindKey = case mnesia:read(forseti_processes, Key) of
            [#forseti_processes{node=N, process=P}] -> {N,P};
            _ -> undefined
        end,
        case FindKey of
        {Node,PID} ->
            case forseti_lib:is_alive(Node, PID) of
            true ->
                {Node,PID};
            false ->
                ?debugFmt("process DIE! ~p in ~p, regenerating...", [PID,Node]),
                generate_process(transaction,Key,M,F,Params)
            end;
        undefined ->
            mnesia:write_lock_table(forseti_processes), 
            generate_process(transaction,Key,M,F,Params)
        end
    end),
    Result;

get_key(dirty, Key, M, F, Params) ->
    case find_key(dirty, Key) of
    {Node,PID} ->
        case forseti_lib:is_alive(Node, PID) of
        true ->
            {Node,PID};
        false ->
            ?debugFmt("process DIE! ~p in ~p, regenerating...", [PID,Node]),
            generate_process(dirty,Key,M,F,Params)
        end;
    undefined ->
        generate_process(dirty,Key,M,F,Params)
    end.

-spec release(method(), node(), key()) -> ok.

release(transaction, Node, PID) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        Match = [{
            #forseti_processes{key = '$1',node = '$2',process = '$3'},
            [{'andalso',{'=:=','$2',Node},{'=:=','$3',PID}}],
            ['$1']
        }],
        [Key] = mnesia:select(forseti_processes, Match),
        mnesia:delete({forseti_processes, Key}),
        [#forseti_nodes{proc_len=PL}=FN] = mnesia:read(forseti_nodes, Node),
        ?debugFmt("release from mnesia(transaction): ~p", [FN]),
        mnesia:write(FN#forseti_nodes{proc_len=PL-1})
    end),
    ok;

release(dirty, Node, PID) ->
    Match = [{
        #forseti_processes{key = '$1',node = '$2',process = '$3'},
        [{'andalso',{'=:=','$2',Node},{'=:=','$3',PID}}],
        ['$1']
    }],
    [Key] = mnesia:dirty_select(forseti_processes, Match),
    mnesia:dirty_delete({forseti_processes, Key}),
    [#forseti_nodes{proc_len=PL}=FN] = 
        mnesia:dirty_read(forseti_nodes, Node),
    mnesia:dirty_write(FN#forseti_nodes{proc_len=PL-1}),
    ok.

-spec find_key(method(), key()) -> {node(), pid()} | undefined.

find_key(transaction, Key) ->
    {atomic, Reply} = mnesia:transaction(fun() ->
        case mnesia:read(forseti_processes, Key) of
            [#forseti_processes{node=N, process=P}] -> {N,P};
            _ -> undefined
        end
    end),
    Reply;

find_key(dirty, Key) ->
    case mnesia:dirty_read(forseti_processes, Key) of
        [#forseti_processes{node=N, process=P}] -> {N,P};
        _ -> undefined
    end.

-spec generate_process(method(), key(), M::atom(), F::atom(), A::[term()]) ->
    {node(), pid()}.

generate_process(Method, Key, M, F, A) ->
    Node = choose_node(),
    case rpc:call(Node, M, F, A) of
    {ok, RetNode, NewP} ->
        store_process(Method, Key, RetNode, NewP),
        gen_server:cast({?MODULE, RetNode}, {link, NewP}),
        {RetNode, NewP};
    {ok, NewP} ->
        store_process(Method, Key, node(NewP), NewP),
        gen_server:cast({?MODULE, node(NewP)}, {link, NewP}),
        {node(NewP), NewP};
    {error, {already_started,OldP}} ->
        store_process(Method, Key, node(OldP), OldP),
        {node(OldP), OldP};
    Error ->
        {error, enoproc}
    end.

-spec store_process(method(), key(), node(), pid()) -> ok.

store_process(transaction, Key, Node, PID) ->
    mnesia:write(#forseti_processes{key=Key, node=Node, process=PID}),
    case mnesia:read(forseti_nodes, Node) of
    [#forseti_nodes{proc_len=PL}=FN] ->
        mnesia:write(FN#forseti_nodes{proc_len=PL+1});
    [] ->
        mnesia:write(#forseti_nodes{node=Node, proc_len=1})
    end,
    ok;

store_process(dirty, Key, Node, PID) ->
    mnesia:dirty_write(#forseti_processes{key=Key, node=Node, process=PID}),
    case mnesia:dirty_read(forseti_nodes, Node) of
    [#forseti_nodes{proc_len=PL}=FN] ->
        mnesia:dirty_write(FN#forseti_nodes{proc_len=PL+1});
    [] ->
        mnesia:dirty_write(#forseti_nodes{node=Node, proc_len=1})
    end,
    ok.

-spec get_metrics(method()) -> [{node(), pos_integer()}].

get_metrics(dirty) ->
    Active = [node()|nodes()],
    lists:foldl(fun(Node, NK) ->
        case lists:member(Node, Active) of
        true ->
            [#forseti_nodes{node=N,proc_len=L}] = 
                mnesia:dirty_read(forseti_nodes, Node),
            [{N, L}|NK];
        false ->
            NK
        end
    end, [], mnesia:dirty_all_keys(forseti_nodes));

get_metrics(transaction) ->
    Active = [node()|nodes()],
    {atomic, ActiveFull} = mnesia:transaction(fun() ->
        lists:foldl(fun(Node, NK) ->
            case lists:member(Node, Active) of
            true ->
                [#forseti_nodes{node=N,proc_len=L}] = 
                    mnesia:read(forseti_nodes, Node),
                [{N, L}|NK];
            false ->
                NK
            end
        end, [], mnesia:all_keys(forseti_nodes))
    end),
    ActiveFull.
