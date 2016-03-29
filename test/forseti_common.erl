-module(forseti_common).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% -- code for the pool

gen_proc() ->
    spawn_link(fun() ->
        receive _ -> ok end
    end).

start_link(<<"delay",_/integer>>) ->
    timer:sleep(2000),
    {ok, gen_proc()};

start_link(started) ->
    PID = spawn_link(fun() ->
        receive {ok,P} -> P ! already_started end
    end),
    {error, {already_started, PID}};

start_link(throw_error) ->
    throw(enoproc);

start_link(ret_error) ->
    {error, notfound};

start_link(_Key) ->
    {ok, gen_proc()}.

start_link(_Key, arg1, arg2, arg3) ->
    {ok, gen_proc()}.

%% -- helpers

short_name(Node) ->
    list_to_atom(hd(string:tokens(atom_to_list(Node),"@"))).

%% -- tests

basic_test(NODE1) ->
    ?_assert(begin
        ?assertEqual(undefined, rpc:call(NODE1, forseti, search_key, [<<"notfound">>])),
        ?assertMatch({ok,_PID}, rpc:call(NODE1, forseti, get_key, [<<"newkey">>])),
        {_Node,PID} = rpc:call(NODE1, forseti, search_key, [<<"newkey">>]),
        PID ! ok,
        timer:sleep(500),
        ?assertEqual(undefined, rpc:call(NODE1, forseti, search_key, [<<"newkey">>])),
        true
    end).

args_test(NODE1) ->
    ?_assert(begin
        ?assertEqual(undefined, rpc:call(NODE1, forseti, search_key, [<<"argskey">>])),
        Args = [arg1, arg2, arg3],
        ?assertMatch({ok,_PID}, rpc:call(NODE1, forseti, get_key, [<<"argskey">>, Args])),
        {ok,PID} = rpc:call(NODE1, forseti, search_key, [<<"argskey">>]),
        PID ! ok,
        timer:sleep(500),
        ?assertEqual(undefined, rpc:call(NODE1, forseti, search_key, [<<"argskey">>])),
        true
    end).

load_test(PROCESSES, NODE1, NODE2, NODE3) ->
    [{timeout, 60, ?_assert(begin
        [ rpc:call(NODE1, forseti, get_key, [N]) || N <- lists:seq(1,PROCESSES) ],
        timer:sleep(1000),
        FullNodes = rpc:call(NODE1, forseti, get_metrics, []),
        ?debugFmt("FullNodes: ~p~n", [FullNodes]),
        ?assertEqual((PROCESSES div 3), proplists:get_value(NODE1, FullNodes)),
        ?assertEqual((PROCESSES div 3), proplists:get_value(NODE2, FullNodes)),
        ?assertEqual((PROCESSES div 3), proplists:get_value(NODE3, FullNodes)),

        ?assertNotEqual(undefined, rpc:call(NODE1, forseti, search_key, [((PROCESSES + 1) div 5)])),
        ?assertNotEqual(undefined, rpc:call(NODE1, forseti, search_key, [((PROCESSES + 1) div 2)])),
        ?assertNotEqual(undefined, rpc:call(NODE1, forseti, search_key, [(((PROCESSES + 1) div 10) * 9)])),
        true
    end)},
    {timeout, 60, ?_assert(begin
        lists:foreach(fun(Key) ->
            {_Node,PID} = rpc:call(NODE1, forseti, get_key, [Key]),
            PID ! ok
        end, lists:seq(1, PROCESSES)),
        timer:sleep(1000),
        EmptyNodes = rpc:call(NODE3, forseti, get_metrics, []),
        ?debugFmt("metrics: ~p~n", [EmptyNodes]),
        ?assertEqual(0, proplists:get_value(NODE1, EmptyNodes)),
        ?assertEqual(0, proplists:get_value(NODE2, EmptyNodes)),
        ?assertEqual(0, proplists:get_value(NODE3, EmptyNodes)),
        true
    end)}].

lock_test(NODE1, NODE2, NODE3) ->
    [{timeout, 60, ?_assert(begin
        ParentPID = self(),
        spawn(fun() ->
            lists:foreach(fun(N) ->
                Key = <<"delay",N/integer>>,
                ?debugFmt("B> generating key = ~p~n", [Key]),
                rpc:call(NODE1, forseti, get_key, [Key]),
                ?debugFmt("<B generated key = ~p~n", [Key])
            end, lists:seq(1,4)),
            ParentPID ! ok
        end),
        timer:sleep(4000),
        T1 = os:timestamp(),
        Seq = lists:seq(1, 2),
        lists:foreach(fun(N) ->
            Key = <<"delay",N/integer>>,
            ?debugFmt(">> request existent key = ~p~n", [Key]),
            rpc:call(NODE1, forseti, get_key, [Key]),
            ?debugFmt("<< requested existent key = ~p~n", [Key])
        end, Seq ++ Seq ++ Seq ++ Seq ++ Seq),
        ?assertEqual(undefined, rpc:call(NODE1, forseti, search_key, [<<"delay",4/integer>>])),
        T2 = os:timestamp(),
        receive
            ok -> ok
        end,
        (timer:now_diff(T2, T1) div 1000000) < 10
    end)},
    {timeout, 60, ?_assert(begin
        lists:foreach(fun(N) ->
            Key = <<"delay",N/integer>>,
            {_Node,PID} = rpc:call(NODE1, forseti, get_key, [Key]),
            PID ! ok
        end, lists:seq(1,4)),
        timer:sleep(1000),
        EmptyNodes = rpc:call(NODE3, forseti, get_metrics, []),
        ?debugFmt("metrics: ~p~n", [EmptyNodes]),
        0 =:= proplists:get_value(NODE1, EmptyNodes) andalso
        0 =:= proplists:get_value(NODE2, EmptyNodes) andalso
        0 =:= proplists:get_value(NODE3, EmptyNodes)
    end)}].

started_error(NODE1) ->
    ?_assert(begin
        {ok,PID} = rpc:call(NODE1, forseti, get_key, [started]),
        PID ! {ok, self()},
        receive already_started -> ok end,
        true
    end).

ret_error(NODE1) ->
    ?_assert(begin
        ?assertMatch({error,_}, rpc:call(NODE1, forseti, get_key, [ret_error])),
        true
    end).

throw_error(NODE1) ->
    ?_assert(begin
        ?assertMatch({error,_}, rpc:call(NODE1, forseti, get_key, [throw_error])),
        true
    end).
