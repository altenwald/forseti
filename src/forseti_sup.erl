-module(forseti_sup).
-author('manuel@altenwald.com').

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, A), {I, {I, start_link, A}, transient, brutal_kill, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Backend, MaxR, MaxT, Nodes) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,
        [Backend, MaxR, MaxT, Nodes]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Backend, MaxR, MaxT, Nodes]) ->
    Module = forseti_app:get_mod_backend(Backend),
    Children = servers(Module, Nodes),
    {ok, {{one_for_all, MaxR, MaxT}, Children}}.

servers(forseti_leader, Nodes) -> [
    ?CHILD(forseti_leader_server, []),
    ?CHILD(forseti_leader, [Nodes])
];

servers(forseti_locks, Nodes) -> [
    ?CHILD(forseti_locks_server, []),
    ?CHILD(forseti_locks, [Nodes])
];

servers(Module, Nodes) -> [
    ?CHILD(Module, [Nodes])
].
