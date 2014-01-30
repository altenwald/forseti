-module(forseti_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% -- code for the pool

start_link(Key) ->
    ?debugFmt("waiter for ~p~n", [Key]),
    {ok, spawn_link(fun() ->
        receive _ -> ok end
    end)}.

%% -- generator

generator_test_() ->
    {foreach,
        fun start/0,
        fun stop/1, [
            fun basic_test/1,
            fun load_test/1
        ]
    }.

%% -- initilizer and finisher

start() ->
    application:start(lager),

    net_kernel:start([forseti1@localhost, shortnames]),
    slave:start(localhost, forseti2),
    slave:start(localhost, forseti3),

    Call = {forseti_test, start_link, []},
    Nodes = [node()|nodes()],
    forseti:start_link(Call, Nodes),
    rpc:call(forseti2@localhost, forseti, start_link, [Call, Nodes]),
    rpc:call(forseti3@localhost, forseti, start_link, [Call, Nodes]),
    ok.

stop(_) ->
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
    ?_assert(begin
        [ forseti:get_key(N) || N <- lists:seq(1,9999) ],
        Nodes = forseti:get_metrics(),
        ?assertEqual(3333, proplists:get_value(forseti1@localhost, Nodes)),
        ?assertEqual(3333, proplists:get_value(forseti2@localhost, Nodes)),
        ?assertEqual(3333, proplists:get_value(forseti3@localhost, Nodes)),

        ?assertNotEqual(undefined, forseti:search_key(200)),
        ?assertNotEqual(undefined, forseti:search_key(500)),
        ?assertNotEqual(undefined, forseti:search_key(900)),
        true
    end).
