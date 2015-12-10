-module(forseti).
-author('manuel@altenwald.com').

-export([
    start_link/2,
    start_link/3,
    start_link/4,
    start_link/5,

    get_less_used_node/0,
    search_key/1,
    get_metrics/0,
    get_key/1,
    get_key/2,
    get_key/3
]).

-include("forseti.hrl").

start_link(Call, Nodes) ->
    start_link(gen_leader, ?MAX_RETRIES, ?MAX_TIME, Call, Nodes).

start_link(Backend, Call, Nodes) ->
    start_link(Backend, ?MAX_RETRIES, ?MAX_TIME, Call, Nodes).

start_link(MaxR, MaxT, Call, Nodes) ->
    start_link(gen_leader, MaxR, MaxT, Call, Nodes).

start_link(Backend, MaxR, MaxT, Call, Nodes) ->
    application:set_env(forseti, max_retries, MaxR),
    application:set_env(forseti, max_time, MaxT),
    application:set_env(forseti, call, Call),
    application:set_env(forseti, nodes, Nodes),
    application:set_env(forseti, backend, Backend),  
    forseti_app:start(normal, []).

-spec get_less_used_node() -> node().

get_less_used_node() ->
    Module = forseti_app:get_mod_backend(),
    Module:choose_node().

-spec search_key(Key :: any()) -> {Node :: node(), PID :: pid()} | undefined.

search_key(Key) ->
    Module = forseti_app:get_mod_backend(),
    Module:search_key(Key).

-type node_metrics() :: {node(), integer()}.

-spec get_metrics() -> [node_metrics()].

get_metrics() ->
    Module = forseti_app:get_mod_backend(),
    Module:get_metrics().

-spec get_key(Key :: any()) -> pid() | {error, Reason::atom()}.

get_key(Key) ->
    Module = forseti_app:get_mod_backend(),
    Module:get_key(Key).

-spec get_key(Key :: any(), Args :: [term()]) ->
    pid() | {error, Reason::atom()}.

get_key(Key, Args) ->
    Module = forseti_app:get_mod_backend(),
    Module:get_key(Key, Args).

get_key(Mod, Key, Args) ->
    Module = forseti_app:get_mod_backend(),
    Module:get_key(Mod, Key, Args).