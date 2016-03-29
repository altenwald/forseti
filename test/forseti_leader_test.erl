-module(forseti_leader_test).
-compile([export_all]).

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

init_forseti(ParentPID, Paths, Call, Nodes) ->
    lists:foreach(fun(Path) ->
        code:add_patha(Path)
    end, Paths),
    {ok, _} = rpc:call(?NODE_TEST, cover, start, [[node()]]),
    {ok, PID} = forseti:start_link(gen_leader, Call, Nodes),
    ParentPID ! {ok, self(), PID},
    receive ok -> ok end.

start() ->
    net_kernel:start([?NODE_TEST, shortnames]),

    Call = {forseti_common, start_link, []},
    Args = [self(), code:get_path(), Call, ?NODES_T],
    PIDs = lists:map(fun(Node) ->
        ShortName = forseti_common:short_name(Node),
        slave:start(localhost, ShortName),
        timer:sleep(500),
        spawn(fun() ->
            rpc:call(Node, ?MODULE, init_forseti, Args)
        end),
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
