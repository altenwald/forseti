-module(forseti_mnesia_tests).
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

start() ->
    net_kernel:start([?NODE_TEST, shortnames]),

    Call = {forseti_common, start_link, []},
    Parent = self(),
    Paths = code:get_path(),
    lists:foreach(fun(Node) ->
        ShortName = forseti_common:short_name(Node),
        slave:start(localhost, ShortName),
        timer:sleep(500),
        {ok, _} = cover:start(Node),
        lists:foreach(fun(Path) ->
            rpc:call(Node, code, add_pathz, [Path])
        end, Paths),
        spawn(Node, fun() ->
            mnesia:stop(),
            mnesia:delete_schema([node()]),
            timer:sleep(500),
            {ok, PID} = forseti:start_link(mnesia, Call, ?NODES_T),
            Parent ! {ok, self(), PID},
            receive ok -> ok end
        end)
    end, ?NODES_T),
    lists:foreach(fun(_) ->
        receive {ok, _InitPID, _PID} -> ok end
    end, ?NODES_T),
    % kill node off
    timer:sleep(500),
    slave:stop(?NODE_OFF),
    timer:sleep(1000),
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
                rpc:call(?NODE1, forseti, get_key, [Key])
            end, lists:seq(1,4)),
            ParentPID ! ok
        end),
        timer:sleep(4000),
        {T1,S1,_} = os:timestamp(),
        Seq = lists:seq(1, 2),
        lists:foreach(fun(N) ->
            Key = <<"delay",N/integer>>,
            rpc:call(?NODE2, forseti, get_key, [Key])
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
        lists:foreach(fun(N) ->
            Key = <<"delay",N/integer>>,
            {_Node,PID} = rpc:call(?NODE1, forseti, get_key, [Key]),
            PID ! ok
        end, lists:seq(1,4)),
        timer:sleep(1000),
        EmptyNodes = rpc:call(?NODE3, forseti, get_metrics, []),
        0 =:= proplists:get_value(?NODE1, EmptyNodes) andalso
        0 =:= proplists:get_value(?NODE2, EmptyNodes) andalso
        0 =:= proplists:get_value(?NODE3, EmptyNodes)
    end)}].
