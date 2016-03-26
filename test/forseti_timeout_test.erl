%%%-------------------------------------------------------------------
%%% @author pepe
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. oct 2014 10:09
%%%-------------------------------------------------------------------
-module(forseti_timeout_test).
-author("pepe").

%% API
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(PROCESSES, 999).

-define(NODE, forseti_leader@localhost).

-define(NODE1_SHORT, forseti_leader).

%% -- generator

generator_test_() ->
    {foreach,
        fun start/0,
        fun stop/1, [
        fun basic_test/1,
        fun timeout_test/1,
        fun ret_error/1,
        fun throw_error/1
    ]
    }.

%% -- initilizer and finisher

start() ->
    net_kernel:start([?NODE, shortnames]),

    Call = {forseti_timeout, start_link, []},
    Nodes = [node()],
    ?debugFmt("configuring nodes = ~p~n", [Nodes]),
    timer:sleep(1000),
    forseti:start_link(Call, Nodes),
    timer:sleep(500),
    ok.

stop(_) ->
    %[ rpc:call(Node, forseti, stop, []) || Node <- [node()|nodes()] ],
    [slave:stop(N) || N <- nodes()],
    net_kernel:stop(),
    ok.

%% -- tests

timeout_test(_) ->
    ?_assert(begin
                 ?debugFmt("get_key: ~p",[forseti:get_key(<<"newkey">>)]),
                 ?debugFmt("result: ~p",[forseti_timeout:timeout(<<"newkey">>)]),
                 ?assertEqual(ok, forseti_timeout:timeout(<<"newkey">>)),
                 ?assertEqual(ok, forseti_timeout:timeout(<<"newkey">>)),
                 ?assertMatch({_Node, _PID}, forseti:get_key(<<"newkey">>)),
                 {_Node, PID} = forseti:search_key(<<"newkey">>),
                 PID ! {timeout,<<"newkey">>},
                 timer:sleep(500),
                 ?assertEqual(undefined, forseti:search_key(<<"newkey">>)), %% MUST FAIL
                 true
             end).

basic_test(_) ->
    ?_assert(begin
                 ?assertEqual(undefined, forseti:search_key(<<"notfound">>)),
                 ?assertMatch({_Node, _PID}, forseti:get_key(<<"newkey">>)),
                 {_Node, PID} = forseti:search_key(<<"newkey">>),
                 PID ! {timeout,<<"newkey">>},
                 timer:sleep(500),
                 ?assertEqual(undefined, forseti:search_key(<<"newkey">>)),
                 true
             end).

ret_error(_) ->
    ?_assert(begin
                 ?assertMatch({error, _}, forseti:get_key(ret_error)),
                 true
             end).

throw_error(_) ->
    ?_assert(begin
                 ?assertMatch({error, _}, forseti:get_key(throw_error)),
                 true
             end).
