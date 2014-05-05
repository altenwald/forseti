-module(forseti_app).

-behaviour(application).

%% Application callbacks
-export([
    start/2,
    stop/1
]).

-include("forseti.hrl").

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    Conf = application:get_all_env(),
    MaxRetries = proplists:get_value(max_retries, Conf, ?MAX_RETRIES),
    MaxTime = proplists:get_value(max_time, Conf, ?MAX_RETRIES),
    Call = proplists:get_value(call, Conf),
    Nodes = proplists:get_value(nodes, Conf, [node()]),
    case Call of
    undefined ->
        {ok, self()};
    _ ->
        forseti_sup:start_link(MaxRetries, MaxTime, Call, Nodes)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
