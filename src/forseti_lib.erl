-module(forseti_lib).
-author('manuel@altenwald.com').

-export([
    is_alive/2,
    is_alive/3
]).

-spec is_alive(node(), pid()) -> boolean().

is_alive(Node, PID) ->
    is_alive(node(), Node, PID).

-spec is_alive(node(), node(), pid()) -> boolean().

is_alive(_MyNode, undefined, _PID) -> false;
is_alive(_MyNode, _Node, undefined) -> false;
is_alive(Node, Node, PID) -> is_process_alive(PID);
is_alive(_MyNode, Node, PID) -> 
    (catch rpc:call(Node, erlang, is_process_alive, [PID])) =:= true.
