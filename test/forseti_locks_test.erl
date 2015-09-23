-module(forseti_locks_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(PROCESSES, 999).

-define(NODE1, forseti1_locks@localhost).
-define(NODE2, forseti2_locks@localhost).
-define(NODE3, forseti3_locks@localhost).

-define(NODE1_SHORT, forseti1_locks).
-define(NODE2_SHORT, forseti2_locks).
-define(NODE3_SHORT, forseti3_locks).

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
    ?debugFmt("start_link args ~n ", []),
    {ok, spawn_link(fun() ->
        receive _ -> ok end
    end)}.

%% -- generator

generator_test_() ->
    ?debugFmt("Init generator test ~n ", []),
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

start() ->
    ?debugFmt("START ~n", []),
    net_kernel:start([?NODE1, shortnames]),
    slave:start(localhost, ?NODE2_SHORT),
    slave:start(localhost, ?NODE3_SHORT),

    Call = {?MODULE, start_link, []},
    Nodes = [node()|nodes()],
    ?debugFmt("configuring nodes = ~p~n", [Nodes]),
    timer:sleep(1000),
    PID1 = spawn(fun() ->
        forseti:start_link(locks, Call, Nodes),
        receive ok -> ok end
    end),
    slave:start(localhost, ?NODE2_SHORT),
    timer:sleep(1000),
    PID2 = spawn(?NODE2, fun() -> 
        forseti:start_link(locks, Call, Nodes),
        receive ok -> ok end
    end),
    slave:start(localhost, ?NODE3_SHORT),
    timer:sleep(1000),
    PID3 = spawn(?NODE3, fun() -> 
        forseti:start_link(locks, Call, Nodes),
        receive ok -> ok end
    end),
    timer:sleep(500),
    [PID1, PID2, PID3].

stop(PIDs) ->
    [ PID ! ok || PID <- PIDs ],
    [ slave:stop(N) || N <- nodes() ],
    net_kernel:stop(),
    ok.

%% -- tests

basic_test(_) ->
    ?debugFmt("basic_tests ~n", []),
    ?_assert(begin
        ?assertEqual(undefined, forseti:search_key(<<"notfound">>)),
        ?assertMatch({ok,_PID}, forseti:get_key(<<"newkey">>)),
        {_Node,PID} = forseti:search_key(<<"newkey">>),
        PID ! ok,
        timer:sleep(500),
        ?assertEqual(undefined, forseti:search_key(<<"newkey">>)),
        true
    end).

args_test(_) ->
    ?debugFmt("arg test ~n", []),
    ?_assert(begin
        ?assertEqual(undefined, forseti:search_key(<<"argskey">>)),
        Args = [arg1, arg2, arg3],
        ?assertMatch({_Node,_PID}, forseti:get_key(<<"argskey">>, Args)),
        {_Node,PID} = forseti:search_key(<<"argskey">>), 
        PID ! ok,
        timer:sleep(500),
        ?assertEqual(undefined, forseti:search_key(<<"argskey">>)),
        true
    end).

load_test(_) ->
    ?debugFmt("load test ~n", []),
    [{timeout, 60, ?_assert(begin
        [ forseti:get_key(N) || N <- lists:seq(1,?PROCESSES) ],
        FullNodes = forseti:get_metrics(),
        ?debugFmt("FullNodes: ~p~n", [FullNodes]),
        ?assertEqual((?PROCESSES div 3), proplists:get_value(?NODE1, FullNodes)),
        ?assertEqual((?PROCESSES div 3), proplists:get_value(?NODE2, FullNodes)),
        ?assertEqual((?PROCESSES div 3), proplists:get_value(?NODE3, FullNodes)),

        ?assertNotEqual(undefined, forseti:search_key((?PROCESSES + 1) div 5)),
        ?assertNotEqual(undefined, forseti:search_key((?PROCESSES + 1) div 2)),
        ?assertNotEqual(undefined, forseti:search_key(((?PROCESSES + 1) div 10) * 9)),
        true
    end)},
    {timeout, 60, ?_assert(begin
        lists:foreach(fun(Key) ->
            {_Node,PID} = forseti:get_key(Key),
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
    ?debugFmt("lock_tests ~n", []),
    [{timeout, 60, ?_assert(begin
        ParentPID = self(),
        spawn(fun() ->
            lists:foreach(fun(N) ->
                Key = <<"delay",N/integer>>,
                ?debugFmt("B> generating key = ~p~n", [Key]),
                forseti:get_key(Key),
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
            forseti:get_key(Key),
            ?debugFmt("<< requested existent key = ~p~n", [Key])
        end, Seq ++ Seq ++ Seq ++ Seq ++ Seq),
        ?assertEqual(undefined, forseti:search_key(<<"delay",4/integer>>)),
        {_,S2,_} = os:timestamp(),
        receive 
            ok -> ok 
        end,
        (S1 + 6) > S2
    end)},
    {timeout, 60, ?_assert(begin
        lists:foreach(fun(N) ->
            Key = <<"delay",N/integer>>,
            {_Node,PID} = forseti:get_key(Key),
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
        ?assertMatch({error,_}, forseti:get_key(ret_error)),
        true
    end).

throw_error(_) ->
    ?_assert(begin
        ?assertMatch({error,_}, forseti:get_key(throw_error)),
        true
    end).
