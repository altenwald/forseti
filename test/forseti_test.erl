-module(forseti_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(PROCESSES, 999).

%% -- code for the pool

start_link(<<"delay",_/binary>>) ->
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

%% -- generator

generator_test_() ->
    {foreach,
        fun start/0,
        fun stop/1, [
            fun basic_test/1,
            fun load_test/1,
            fun lock_test/1,
            fun ret_error/1,
            fun throw_error/1
        ]
    }.

%% -- initilizer and finisher

start() ->
    net_kernel:start([forseti1@localhost, shortnames]),
    slave:start(localhost, forseti2),
    slave:start(localhost, forseti3),

    Call = {forseti_test, start_link, []},
    Nodes = [node()|nodes()],
    ?debugFmt("configuring nodes = ~p~n", [Nodes]),
    timer:sleep(1000),
    forseti:start_link(Call, Nodes),
    spawn(forseti2@localhost, fun() -> 
        forseti:start_link(Call, Nodes),
        receive ok -> ok end
    end),
    spawn(forseti3@localhost, fun() -> 
        forseti:start_link(Call, Nodes),
        receive ok -> ok end
    end),
    timer:sleep(500),
    ok.

stop(_) ->
    %[ rpc:call(Node, forseti, stop, []) || Node <- [node()|nodes()] ],
    [ slave:stop(N) || N <- nodes() ],
    net_kernel:stop(),
    ok.

%% -- tests

basic_test(_) ->
    ?_assert(begin
        ?assertEqual(undefined, forseti:search_key(<<"notfound">>)),
        ?assertMatch({_Node,_PID}, forseti:get_key(<<"newkey">>)),
        {_Node,PID} = forseti:search_key(<<"newkey">>),
        PID ! ok,
        timer:sleep(500),
        ?assertMatch(undefined, forseti:search_key(<<"newkey">>)),
        true
    end).

load_test(_) ->
    [{timeout, 60, ?_assert(begin
        [ forseti:get_key(N) || N <- lists:seq(1,?PROCESSES) ],
        FullNodes = forseti:get_metrics(),
        ?assertEqual((?PROCESSES div 3), proplists:get_value(forseti1@localhost, FullNodes)),
        ?assertEqual((?PROCESSES div 3), proplists:get_value(forseti2@localhost, FullNodes)),
        ?assertEqual((?PROCESSES div 3), proplists:get_value(forseti3@localhost, FullNodes)),

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
        EmptyNodes = rpc:call(forseti3@localhost, forseti, get_metrics, []),
        ?debugFmt("metrics: ~p~n", [EmptyNodes]),
        ?assertEqual(0, proplists:get_value(forseti1@localhost, EmptyNodes)),
        ?assertEqual(0, proplists:get_value(forseti2@localhost, EmptyNodes)),
        ?assertEqual(0, proplists:get_value(forseti3@localhost, EmptyNodes)),
        true
    end)}].

lock_test(_) ->
    [{timeout, 60, ?_assert(begin
        ParentPID = self(),
        PID1 = spawn(fun() ->
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
        EmptyNodes = rpc:call(forseti3@localhost, forseti, get_metrics, []),
        ?debugFmt("metrics: ~p~n", [EmptyNodes]),
        0 =:= proplists:get_value(forseti1@localhost, EmptyNodes) andalso
        0 =:= proplists:get_value(forseti2@localhost, EmptyNodes) andalso
        0 =:= proplists:get_value(forseti3@localhost, EmptyNodes)
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
