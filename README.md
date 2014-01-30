forseti
=======

[Forseti](http://en.wikipedia.org/wiki/Forseti) is a balancer and distribution system for Erlang processes. The basic idea for this system is keep a basic leader/worker structure for keep a process dictionary. When is needed a new process, the request should be done to the leader, the leader spawns a new process in a worker (less used worker) and do that the worker monitor the new process. If the process die or finish its running, the worker removes the process from the dictionary (sends a request for that to the leader).

When is needed to access to an existent process, the search is done in the workers and if the search is not ok, then the worker send the params to the leader. If the leader doesn't find the process then can do two things:

 * Create a new process and return its PID.
 * Return 'undefined'. 

The implementation is very easy:

```erlang
Call = {test_forseti, start_link, []},
Nodes = [node1@server1, node2@server2],
forseti:start_link(Call, Nodes),
```

The basic implementation for `test_forseti` should be a gen_server with a start_link as following:

```erlang
start_link(_Key) ->
    gen_server:start_link(?MODULE, [], []).
```

The function passed as param in the form `{M,F,A}` should has the first param as `Key`.

For get a PID you can use the following function:

```erlang
{Node,PID} = forseti:get_key(<<"mykey1">>),
```

If you only want to search a key and in case this not exist returns `undefined` you can use:

```erlang
case forseti:search_key(<<"mykey1">>) of
    undefined -> {error, notfound};
    {_Node,PID} -> {ok, PID}
end
```

Enjoy!