-module(forseti_mnesia).
-author('manuel@altenwald.com').

-behaviour(gen_server).

-export([
    start_link/1,
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
    get/3,
    find/2,
    add_call/2
]).

-include("forseti.hrl").

-record(forseti_processes, {
    key :: term(),
    node = node() :: node(),
    process :: pid()
}).

-record(forseti_nodes, {
    node :: node(),
    proc_len = 0 :: pos_integer()
}).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifndef(TEST).
-define(debugFmt(A,B), (ok)).
-endif.

-spec start_link([node()]) -> {ok, pid()} | {error, term()}.

start_link(Nodes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Nodes], []).

-spec choose_node() -> node().

choose_node() ->
    {KeyMin,_} = lists:foldl(fun
        ({Key,Val},{_,undefined}) ->
            {Key,Val};
        ({Key,Val},{_KeyMin,ValMin}) when Val < ValMin ->
            {Key,Val};
        ({_Key,_Val},{KeyMin,ValMin}) ->
            {KeyMin,ValMin}
    end, {node(),undefined}, get_metrics()),
    KeyMin.

-spec get_metrics() -> [{node(), pos_integer()}].

get_metrics() ->
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

-spec get(call_name(), key(), Args::[term()]) ->
    {ok, pid()} | {error, reason()}.

get(Name, Key, Args) ->
    gen_server:call(?MODULE, {get, Name, Key, Args}).

-spec find(call_name(), key()) -> {ok, pid()} | undefined.

find(Name, Key) ->
    gen_server:call(?MODULE, {find, Name, Key}).

-spec add_call(call_name(), call()) -> ok.

add_call(Name, Call) ->
    gen_server:cast(?MODULE, {add_call, Name, Call}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Nodes]) ->
    process_flag(trap_exit, true),
    init_db(Nodes),
    {ok, dict:new()}.

handle_call({find, Name, Key}, _From, Call) ->
    {atomic, Reply} = mnesia:transaction(fun() ->
        case find_key(Name, Key) of
            {Node, PID} ->
                case forseti_lib:is_alive(Node, PID) of
                    true -> {ok, PID};
                    false -> undefined
                end;
            _ ->
                undefined
        end
    end),
    {reply, Reply, Call};

handle_call({get, Name, Key, Args}, _From, Calls) ->
    {M,F,A} = forseti_lib:get_call(Name, Calls),
    Params = [Key|A] ++ Args,
    {atomic, Result} = mnesia:transaction(fun() ->
        case find_key(Name, Key) of
        {Node,PID} ->
            case forseti_lib:is_alive(Node, PID) of
            true ->
                {ok,PID};
            false ->
                generate_process(Name,Key,M,F,Params)
            end;
        undefined ->
            generate_process(Name,Key,M,F,Params)
        end
    end),
    {reply, Result, Calls};

handle_call(stop, _From, State) ->
    {stop, normal, State}.

handle_cast({link,PID}, State) ->
    link(PID),
    {noreply, State};

handle_cast({add_call, Name, Call}, Calls) ->
    {noreply, dict:store(Name, Call, Calls)}.

handle_info({'EXIT', PID, _Info}, State) ->
    release(node(PID), PID),
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
            mnesia:dirty_write(FN),
            ok;
        _ ->
            init_db(Nodes)
    end.

-spec release(node(), key()) -> ok.

release(Node, PID) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        Match = [{
            #forseti_processes{key = '$1',node = '$2',process = '$3'},
            [{'andalso',{'=:=','$2',Node},{'=:=','$3',PID}}],
            ['$1']
        }],
        [Key] = mnesia:select(forseti_processes, Match),
        mnesia:delete({forseti_processes, Key}),
        mnesia:dirty_update_counter(forseti_nodes, Node, -1),
        ok
    end),
    ok.

-spec generate_process(call_name(), key(), M::atom(), F::atom(), A::[term()]) ->
    {node(), pid()}.

generate_process(Name, Key, M, F, A) ->
    Node = choose_node(),
    case rpc:call(Node, M, F, A) of
    {ok, NewP} ->
        store_process(Name, Key, node(NewP), NewP),
        gen_server:cast({?MODULE, node(NewP)}, {link, NewP}),
        {ok, NewP};
    {error, {already_started,OldP}} ->
        store_process(Name, Key, node(OldP), OldP),
        {ok, OldP};
    _Error ->
        {error, enoproc}
    end.

-spec store_process(call_name(), key(), node(), pid()) -> ok.

store_process(Name, Key, Node, PID) ->
    mnesia:write(#forseti_processes{key={Name,Key}, node=Node, process=PID}),
    mnesia:dirty_update_counter(forseti_nodes, Node, 1),
    ok.

-spec find_key(call_name(), key()) -> {node(), pid()} | undefined.

find_key(Name, Key) ->
    case mnesia:read(forseti_processes, {Name,Key}) of
        [#forseti_processes{node=N, process=P}] -> {N,P};
        _ -> undefined
    end.
