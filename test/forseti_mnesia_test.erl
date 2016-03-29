-module(forseti_mnesia_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(PROCESSES, 99).

-define(NODE_TEST, forseti_mnesia@localhost).

-define(NODE1, forseti1_mnesia@localhost).
-define(NODE2, forseti2_mnesia@localhost).
-define(NODE3, forseti3_mnesia@localhost).
-define(NODE_OFF, forseti_off_mnesia@localhost).

-define(NODES_T, [?NODE1, ?NODE2, ?NODE3, ?NODE_OFF]).

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
            fun throw_error/1,
            fun started_error/1
        ]
    }.

%% -- initilizer and finisher

init_forseti(ParentPID, Paths, Call, Nodes) ->
    lists:foreach(fun(Path) ->
        code:add_patha(Path)
    end, Paths),
    {ok, _} = rpc:call(?NODE_TEST, cover, start, [[node()]]),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    timer:sleep(500),
    forseti:start_link(mnesia, Call, Nodes),
    ParentPID ! ok,
    receive ok -> ok end.

start() ->
    net_kernel:start([?NODE_TEST, shortnames]),
    slave:start(localhost, forseti_common:short_name(?NODE1)),
    slave:start(localhost, forseti_common:short_name(?NODE2)),
    slave:start(localhost, forseti_common:short_name(?NODE3)),
    slave:start(localhost, forseti_common:short_name(?NODE_OFF)),

    timer:sleep(1000),
    Call = {forseti_common, start_link, []},
    Args = [self(), code:get_path(), Call, ?NODES_T],
    lists:foreach(fun(Node) ->
        rpc:cast(Node, ?MODULE, init_forseti, Args),
        timer:sleep(500)
    end, nodes()),
    [ receive ok -> ok end || _ <- lists:seq(1,3) ],
    timer:sleep(500),
    slave:stop(?NODE_OFF),
    ok.

stop(_) ->
    cover:flush(nodes()),
    [ slave:stop(N) || N <- nodes() ],
    net_kernel:stop(),
    timer:sleep(1000),
    ok.

%% -- tests

basic_test(_) ->
    forseti_common:basic_test(?NODE1).

args_test(_) ->
    forseti_common:args_test(?NODE2).

load_test(_) ->
    forseti_common:load_test(?PROCESSES, ?NODE1, ?NODE2, ?NODE3).

ret_error(_) ->
    forseti_common:ret_error(?NODE1).

throw_error(_) ->
    forseti_common:throw_error(?NODE1).

started_error(_) ->
    forseti_common:started_error(?NODE1).

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
        ?assertEqual(undefined, rpc:call(?NODE3, forseti, search_key,
            [<<"delay",4/integer>>])),
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
