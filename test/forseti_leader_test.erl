-module(forseti_leader_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(PROCESSES, 999).

-define(NODE_TEST, forseti_leader@localhost).

-define(NODE1, forseti1_leader@localhost).
-define(NODE2, forseti2_leader@localhost).
-define(NODE3, forseti3_leader@localhost).

-define(NODE1_SHORT, forseti1_leader).
-define(NODE2_SHORT, forseti2_leader).
-define(NODE3_SHORT, forseti3_leader).

-define(NODES_T, [?NODE1, ?NODE2, ?NODE3]).

%% -- code for the pool

start_link(<<"delay",_/integer>>) ->
    timer:sleep(2000),
    {ok, spawn_link(fun() ->
        receive _ -> ok end
    end)};

start_link(throw_error) ->
    throw(enoproc);

start_link(ret_error) ->
    {error, notfound};

start_link(_Key) ->
    {ok, spawn_link(fun() ->
        receive _ -> ok end
    end)}.

start_link(_Key, arg1, arg2, arg3) ->
    {ok, spawn_link(fun() ->
        receive _ -> ok end
    end)}.

%% -- generator

generator_test_() ->
    {foreach,
        fun start/0,
        fun stop/1, [
            fun basic_test/1,
            fun args_test/1,
            fun load_test/1,
            fun lock_test/1,
            fun ret_error/1,
            fun throw_error/1
        ]
    }.

%% -- initilizer and finisher

init_forseti(ParentPID, Paths, Call, Nodes) ->
    code:set_path(Paths),
    {ok, PID} = forseti:start_link(gen_leader, Call, Nodes),
    ParentPID ! {ok, self(), PID},
    receive ok -> ok end.

start() ->
    net_kernel:start([?NODE_TEST, shortnames]),

    Call = {?MODULE, start_link, []},
    Args = [self(), code:get_path(), Call, ?NODES_T],
    PIDs = lists:map(fun(Node) ->
        ShortName = list_to_atom(hd(string:tokens(atom_to_list(Node),"@"))),
        slave:start(localhost, ShortName),
        timer:sleep(500),
        spawn(fun() ->
            rpc:call(Node, ?MODULE, init_forseti, Args)
        end),
        receive {ok, InitPID, _PID} -> InitPID end
    end, ?NODES_T),
    timer:sleep(1000),
    PIDs.

stop(PIDs) ->
    [ PID ! ok || PID <- PIDs ],
    [ slave:stop(N) || N <- nodes() ],
    net_kernel:stop(),
    ok.

%% -- tests

basic_test(_) ->
    ?_assert(begin
        ?assertEqual(undefined, rpc:call(?NODE1, forseti, search_key, [<<"notfound">>])),
        ?assertMatch({ok,_PID}, rpc:call(?NODE1, forseti, get_key, [<<"newkey">>])),
        {_Node,PID} = rpc:call(?NODE1, forseti, search_key, [<<"newkey">>]),
        PID ! ok,
        timer:sleep(500),
        ?assertEqual(undefined, rpc:call(?NODE1, forseti, search_key, [<<"newkey">>])),
        true
    end).

args_test(_) ->
    ?_assert(begin
        ?assertEqual(undefined, rpc:call(?NODE1, forseti, search_key, [<<"argskey">>])),
        Args = [arg1, arg2, arg3],
        ?assertMatch({_Node,_PID}, rpc:call(?NODE1, forseti, get_key, [<<"argskey">>, Args])),
        {_Node,PID} = rpc:call(?NODE1, forseti, search_key, [<<"argskey">>]),
        PID ! ok,
        timer:sleep(500),
        ?assertEqual(undefined, rpc:call(?NODE1, forseti, search_key, [<<"argskey">>])),
        true
    end).

load_test(_) ->
    [{timeout, 60, ?_assert(begin
        [ rpc:call(?NODE1, forseti, get_key, [N]) || N <- lists:seq(1,?PROCESSES) ],
        FullNodes = rpc:call(?NODE1, forseti, get_metrics, []),
        ?debugFmt("FullNodes: ~p~n", [FullNodes]),
        ?assertEqual((?PROCESSES div 3), proplists:get_value(?NODE1, FullNodes)),
        ?assertEqual((?PROCESSES div 3), proplists:get_value(?NODE2, FullNodes)),
        ?assertEqual((?PROCESSES div 3), proplists:get_value(?NODE3, FullNodes)),

        ?assertNotEqual(undefined, rpc:call(?NODE1, forseti, search_key, [((?PROCESSES + 1) div 5)])),
        ?assertNotEqual(undefined, rpc:call(?NODE1, forseti, search_key, [((?PROCESSES + 1) div 2)])),
        ?assertNotEqual(undefined, rpc:call(?NODE1, forseti, search_key, [(((?PROCESSES + 1) div 10) * 9)])),
        true
    end)},
    {timeout, 60, ?_assert(begin
        lists:foreach(fun(Key) ->
            {_Node,PID} = rpc:call(?NODE1, forseti, get_key, [Key]),
            PID ! ok
        end, lists:seq(1, ?PROCESSES)),
        timer:sleep(500),
        EmptyNodes = rpc:call(?NODE3, forseti, get_metrics, []),
        ?debugFmt("metrics: ~p~n", [EmptyNodes]),
        ?assertEqual(0, proplists:get_value(?NODE1, EmptyNodes)),
        ?assertEqual(0, proplists:get_value(?NODE2, EmptyNodes)),
        ?assertEqual(0, proplists:get_value(?NODE3, EmptyNodes)),
        true
    end)}].

lock_test(_) ->
    [{timeout, 60, ?_assert(begin
        ParentPID = self(),
        spawn(fun() ->
            lists:foreach(fun(N) ->
                Key = <<"delay",N/integer>>,
                ?debugFmt("B> generating key = ~p~n", [Key]),
                rpc:call(?NODE1, forseti, get_key, [Key]),
                ?debugFmt("<B generated key = ~p~n", [Key])
            end, lists:seq(1,4)),
            ParentPID ! ok
        end),
        timer:sleep(4000),
        {_,S1,_} = os:timestamp(),
        Seq = lists:seq(1, 2),
        lists:foreach(fun(N) ->
            Key = <<"delay",N/integer>>,
            ?debugFmt(">> request existent key = ~p~n", [Key]),
            rpc:call(?NODE1, forseti, get_key, [Key]),
            ?debugFmt("<< requested existent key = ~p~n", [Key])
        end, Seq ++ Seq ++ Seq ++ Seq ++ Seq),
        ?assertEqual(undefined, rpc:call(?NODE1, forseti, search_key, [<<"delay",4/integer>>])),
        {_,S2,_} = os:timestamp(),
        receive
            ok -> ok
        end,
        (S1 + 6) > S2
    end)},
    {timeout, 60, ?_assert(begin
        lists:foreach(fun(N) ->
            Key = <<"delay",N/integer>>,
            {_Node,PID} = rpc:call(?NODE1, forseti, get_key, [Key]),
            PID ! ok
        end, lists:seq(1,4)),
        timer:sleep(500),
        EmptyNodes = rpc:call(?NODE3, forseti, get_metrics, []),
        ?debugFmt("metrics: ~p~n", [EmptyNodes]),
        0 =:= proplists:get_value(?NODE1, EmptyNodes) andalso
        0 =:= proplists:get_value(?NODE2, EmptyNodes) andalso
        0 =:= proplists:get_value(?NODE3, EmptyNodes)
    end)}].

ret_error(_) ->
    ?_assert(begin
        ?assertMatch({error,_}, rpc:call(?NODE1, forseti, get_key, [ret_error])),
        true
    end).

throw_error(_) ->
    ?_assert(begin
        ?assertMatch({error,_}, rpc:call(?NODE1, forseti, get_key, [throw_error])),
        true
    end).
