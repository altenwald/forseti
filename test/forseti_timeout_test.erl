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

-define(NODE, forseti_timeout@localhost).

-define(NODE1_SHORT, forseti_timeout).

%% -- generator

generator_test_() ->
    {foreach,
        fun start/0,
        fun stop/1, [
            fun timeout_test/1
        ]
    }.

%% -- initilizer and finisher

start() ->
    net_kernel:start([?NODE, shortnames]),

    Call = {forseti_timeout, start_link, []},
    Nodes = [node()],
    timer:sleep(1000),
    forseti:start_link(gen_leader, Call, Nodes),
    timer:sleep(500),
    ok.

stop(_) ->
    net_kernel:stop(),
    ok.

%% -- tests

timeout_test(_) ->
    {timeout, 60, ?_assert(begin
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
             end)
    }.
