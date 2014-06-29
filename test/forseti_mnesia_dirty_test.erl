-module(forseti_mnesia_dirty_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(PROCESSES, 999).

-define(NODE_TEST, forseti_mnesia@localhost).

-define(NODE1, forseti1_dirty@localhost).
-define(NODE2, forseti2_dirty@localhost).
-define(NODE3, forseti3_dirty@localhost).

-define(NODE1_SHORT, forseti1_dirty).
-define(NODE2_SHORT, forseti2_dirty).
-define(NODE3_SHORT, forseti3_dirty).

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

start() ->
    ?debugFmt("----- START", []),
    net_kernel:start([?NODE_TEST, shortnames]),
    slave:start(localhost, ?NODE1_SHORT),
    slave:start(localhost, ?NODE2_SHORT),
    slave:start(localhost, ?NODE3_SHORT),

    Call = {forseti_mnesia_test, start_link, []},
    Nodes = nodes(),
    ?debugFmt("configuring nodes = ~p~n", [Nodes]),
    timer:sleep(1000),
    ParentPID = self(),
    spawn(?NODE1, fun() ->
        mnesia:stop(),
        mnesia:delete_schema([node()]),
        forseti:start_link(mnesia, 20, 10, Call, Nodes),
        application:set_env(forseti, mnesia, [{method, dirty}]),
        ParentPID ! ok,
        receive ok -> ok end
    end),
    timer:sleep(500),
    spawn(?NODE2, fun() -> 
        mnesia:stop(),
        mnesia:delete_schema([node()]),
        forseti:start_link(mnesia, 20, 10, Call, Nodes),
        application:set_env(forseti, mnesia, [{method, dirty}]),
        ParentPID ! ok,
        receive ok -> ok end
    end),
    timer:sleep(500),
    spawn(?NODE3, fun() -> 
        mnesia:stop(),
        mnesia:delete_schema([node()]),
        forseti:start_link(mnesia, 20, 10, Call, Nodes),
        application:set_env(forseti, mnesia, [{method, dirty}]),
        ParentPID ! ok,
        receive ok -> ok end
    end),
    [ receive ok -> ok end || _ <- lists:seq(1,3) ],
    ok.

stop(_) ->
    ?debugFmt("===== STOP", []),
    [ slave:stop(N) || N <- nodes() ],
    net_kernel:stop(),
    timer:sleep(1000),
    ok.

%% -- tests

basic_test(_) ->
    ?_assert(begin
        ?assertEqual(undefined, rpc:call(?NODE1, forseti, search_key, [<<"notfound">>])),
        ?assertMatch({_Node,_PID}, rpc:call(?NODE1, forseti, get_key, [<<"newkey">>])),
        {_Node,PID} = rpc:call(?NODE1, forseti, search_key, [<<"newkey">>]),
        PID ! ok,
        timer:sleep(500),
        ?assertEqual(undefined, rpc:call(?NODE1, forseti, search_key, [<<"newkey">>])),
        true
    end).

args_test(_) ->
    ?_assert(begin
        ?assertEqual(undefined, rpc:call(?NODE2, forseti, search_key, [<<"argskey">>])),
        Args = [arg1, arg2, arg3],
        ?assertMatch({_Node,_PID}, rpc:call(?NODE2, forseti, get_key, [<<"argskey">>, Args])),
        {_Node,PID} = rpc:call(?NODE2, forseti, search_key, [<<"argskey">>]), 
        PID ! ok,
        timer:sleep(500),
        ?assertEqual(undefined, rpc:call(?NODE1, forseti, search_key, [<<"argskey">>])),
        true
    end).

load_test(_) ->
    [{timeout, 60, ?_assert(begin
        [ rpc:call(?NODE1, forseti, get_key, [N]) || N <- lists:seq(1,?PROCESSES) ],
        timer:sleep(1000),
        FullNodes = rpc:call(?NODE2, forseti, get_metrics, []),
        ?debugFmt("full nodes = ~p~n", [FullNodes]),
        ?assertEqual((?PROCESSES div 3), proplists:get_value(?NODE1, FullNodes)),
        ?assertEqual((?PROCESSES div 3), proplists:get_value(?NODE2, FullNodes)),
        ?assertEqual((?PROCESSES div 3), proplists:get_value(?NODE3, FullNodes)),

        ?assertNotEqual(undefined, rpc:call(?NODE1, forseti, search_key, [(?PROCESSES + 1) div 5])),
        ?assertNotEqual(undefined, rpc:call(?NODE1, forseti, search_key, [(?PROCESSES + 1) div 2])),
        ?assertNotEqual(undefined, rpc:call(?NODE1, forseti, search_key, [((?PROCESSES + 1) div 10) * 9])),
        true
    end)},
    {timeout, 60, ?_assert(begin
        lists:foreach(fun(Key) ->
            {_Node,PID} = rpc:call(?NODE1, forseti, get_key, [Key]),
            PID ! ok
        end, lists:seq(1, ?PROCESSES)),
        timer:sleep(1000),
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
        {T1,S1,_} = os:timestamp(),
        Seq = lists:seq(1, 2), 
        lists:foreach(fun(N) ->
            Key = <<"delay",N/integer>>,
            ?debugFmt(">> request existent key = ~p~n", [Key]),
            rpc:call(?NODE2, forseti, get_key, [Key]),
            ?debugFmt("<< requested existent key = ~p~n", [Key])
        end, Seq ++ Seq ++ Seq ++ Seq ++ Seq),
        ?assertEqual(undefined, rpc:call(?NODE3, forseti, search_key, [<<"delay",4/integer>>])),
        {T2,S2,_} = os:timestamp(),
        receive 
            ok -> ok 
        end,
        ((T1 * 1000000) + S1 + 10) > ((T2 * 1000000) + S2)
    end)},
    {timeout, 60, ?_assert(begin
        ?debugFmt(" ** CLEANING processes...", []),
        lists:foreach(fun(N) ->
            Key = <<"delay",N/integer>>,
            {_Node,PID} = rpc:call(?NODE1, forseti, get_key, [Key]),
            PID ! ok
        end, lists:seq(1,4)),
        timer:sleep(1000),
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
