-module(forseti).
-author('manuel@altenwald.com').

-export([
    start_link/1,
    start_link/2,
    start_link/3,
    start_link/4,
    start_link/5,

    add_call/2,

    get_less_used_node/0,
    search_key/1,
    get_metrics/0,
    get_key/1,
    get_key/2,

    get/2,
    get/3,
    find/2
]).

-include("forseti.hrl").

-spec start_link([node()]) -> {ok, pid()} | {error, reason()}.

start_link(Nodes) ->
    start_link(locks, ?MAX_RETRIES, ?MAX_TIME, [], Nodes).

-spec start_link(call(), [node()]) -> {ok, pid()} | {error, reason()}.

start_link(Call, Nodes) ->
    start_link(locks, ?MAX_RETRIES, ?MAX_TIME, Call, Nodes).

-spec start_link(backend(), call(), [node()]) -> {ok, pid()} | {error, reason()}.

start_link(Backend, Call, Nodes) ->
    start_link(Backend, ?MAX_RETRIES, ?MAX_TIME, Call, Nodes).

-spec start_link(max_retries(), max_time(), call(), [node()]) ->
    {ok, pid()} | {error, reason()}.

start_link(MaxR, MaxT, Call, Nodes) ->
    start_link(locks, MaxR, MaxT, Call, Nodes).

-spec start_link(backend(), max_retries(), max_time(), call(), [node()]) ->
    {ok, pid()} | {error, reason()}.

start_link(Backend, MaxR, MaxT, Call, Nodes) ->
    application:set_env(forseti, max_retries, MaxR),
    application:set_env(forseti, max_time, MaxT),
    application:set_env(forseti, call, Call),
    application:set_env(forseti, nodes, Nodes),
    application:set_env(forseti, backend, Backend),
    forseti_app:start(normal, []).

-spec add_call(call_name(), call()) -> ok.

add_call(Name, {M,F,A}) when is_atom(Name) ->
    Module = forseti_app:get_mod_backend(),
    Module:add_call(Name, {M,F,A}).

-spec get_less_used_node() -> node().

get_less_used_node() ->
    Module = forseti_app:get_mod_backend(),
    Module:choose_node().

-spec search_key(Key :: any()) -> {ok, PID :: pid()} | undefined.

%@deprecated
search_key(Key) ->
    find(default, Key).

-spec get_metrics() -> [node_metrics()].

get_metrics() ->
    Module = forseti_app:get_mod_backend(),
    Module:get_metrics().

-spec get_key(Key :: any()) -> {node(), pid()} | {error, reason()}.

%@deprecated
get_key(Key) ->
    get_key(Key, []).

-spec get_key(Key :: any(), Args :: [term()]) ->
    {node(), pid()} | {error, Reason::atom()}.

%@deprecated
get_key(Key, Args) ->
    get(default, Key, Args).

-spec get(Name :: call_name(), Key :: any()) ->
    {ok, pid()} | {error, Reason::atom()}.

get(Name, Key) ->
    get(Name, Key, []).

-spec get(Name :: call_name(), Key :: any(), Args :: [term()]) ->
    {ok, pid()} | {error, Reason::atom()}.

get(Name, Key, Args) ->
    Module = forseti_app:get_mod_backend(),
    Module:get(Name, Key, Args).

-spec find(Name :: call_name(), Key :: any()) -> {ok, pid()} | {error, reason()}.

find(Name, Key) ->
    Module = forseti_app:get_mod_backend(),
    Module:find(Name, Key).
