-module(forseti).

-export([
    start_link/2,
    start_link/4,

    get_less_used_node/0,
    search_key/1,
    get_metrics/0,
    get_key/1
]).

-define(MAX_RETRIES, 20).
-define(MAX_TIME, 10).

start_link(Call, Nodes) ->
	start_link(?MAX_RETRIES, ?MAX_TIME, Call, Nodes).

start_link(MaxR, MaxT, Call, Nodes) ->
	forseti_sup:start_link(MaxR, MaxT, Call, Nodes).

-spec get_less_used_node() -> node().

get_less_used_node() ->
    gen_leader:call(forseti_leader, choose_node).

-spec search_key(Key :: any()) -> {Node :: node(), PID :: pid()} | undefined.

search_key(Key) ->
    gen_server:call(forseti_server, {search, Key}).

-type node_metrics() :: {node(), integer()}.

-spec get_metrics() -> [node_metrics()].

get_metrics() ->
    gen_leader:call(forseti_leader, get_metrics).

-spec get_key(Key :: any()) -> pid() | {error, Reason::atom()}.

get_key(Key) ->
    gen_server:call(forseti_server, {get_key, Key}).

