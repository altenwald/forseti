-module(forseti_app).
-author('manuel@altenwald.com').

-behaviour(application).

%% Application callbacks
-export([
    start/2,
    stop/1
]).

% public API
-export([
    get_mod_backend/0,
    get_mod_backend/1
]).

-include("forseti.hrl").

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    Conf = application:get_all_env(forseti),
    MaxRetries = proplists:get_value(max_retries, Conf, ?MAX_RETRIES),
    MaxTime = proplists:get_value(max_time, Conf, ?MAX_RETRIES),
    Call = proplists:get_value(call, Conf),
    Nodes = proplists:get_value(nodes, Conf, [node()]),
    Backend = proplists:get_value(backend, Conf, locks),
    case Call of
    undefined ->
        {ok, self()};
    _ ->
        forseti_sup:start_link(Backend, MaxRetries, MaxTime, Call, Nodes)
    end.

-spec stop(State :: term()) -> term().
stop(_State) ->
    ok.

-spec get_mod_backend() -> atom().
get_mod_backend() ->
    get_mod_backend(application:get_env(forseti, backend)).

-spec get_mod_backend(atom() | {ok, atom()}) -> atom().
get_mod_backend(gen_leader) -> forseti_leader;
get_mod_backend(mnesia) -> forseti_mnesia;
get_mod_backend(locks) -> forseti_locks;
get_mod_backend({ok, gen_leader}) -> forseti_leader;
get_mod_backend({ok, mnesia}) -> forseti_mnesia;
get_mod_backend({ok, locks}) -> forseti_locks;
get_mod_backend(undefined) -> throw(ebackendnotdefined).

%%%===================================================================
%%% Internal functions
%%%===================================================================
