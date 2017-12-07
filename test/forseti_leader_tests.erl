-module(forseti_leader_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PROCESSES, 999).

-define(NODE_TEST, forseti_leader@localhost).

-define(NODE1, forseti1_leader@localhost).
-define(NODE2, forseti2_leader@localhost).
-define(NODE3, forseti3_leader@localhost).
-define(NODE_OFF, forseti_off_leader@localhost).

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
            {ok, PID} = forseti:start_link(gen_leader, Call, ?NODES_T),
            Parent ! {ok, self(), PID},
            receive ok -> ok end
        end)
    end, ?NODES_T),
    PIDs = lists:map(fun(_) ->
        receive {ok, InitPID, _PID} -> InitPID end
    end, ?NODES_T),
    % kill node off
    timer:sleep(500),
    slave:stop(?NODE_OFF),
    timer:sleep(1000),
    PIDs.

stop(PIDs) ->
    cover:flush(nodes()),
    [ PID ! ok || PID <- PIDs ],
    [ slave:stop(N) || N <- nodes() ],
    net_kernel:stop(),
    timer:sleep(1000),
    ok.

%% -- tests

basic_test(_) ->
    forseti_common:basic_test(?NODE1).

args_test(_) ->
    forseti_common:args_test(?NODE1).

load_test(_) ->
    forseti_common:load_test(?PROCESSES, ?NODE1, ?NODE2, ?NODE3).

lock_test(_) ->
    forseti_common:lock_test(?NODE1, ?NODE2, ?NODE3).

ret_error(_) ->
    forseti_common:ret_error(?NODE1).

throw_error(_) ->
    forseti_common:throw_error(?NODE1).

started_error(_) ->
    forseti_common:started_error(?NODE1).
