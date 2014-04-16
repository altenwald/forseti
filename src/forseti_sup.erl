-module(forseti_sup).

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

start_link(MaxR, MaxT, Call, Nodes) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MaxR, MaxT, Call, Nodes]).

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
init([MaxR, MaxT, Call, Nodes]) ->
    {ok, {{one_for_all, MaxR, MaxT}, [
        ?CHILD(forseti_leader, [Call, Nodes]),
        ?CHILD(forseti_server, [])
    ]}}.
