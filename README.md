forseti
=======

[![Build Status](https://api.travis-ci.org/altenwald/forseti.png)](https://travis-ci.org/altenwald/forseti)

[Forseti](http://en.wikipedia.org/wiki/Forseti) is a balancer and distribution system for Erlang processes. The basic idea for this system is keep a basic leader/worker structure for keep a process dictionary. When is needed a new process, the request should be done to the leader, the leader spawns a new process in a worker (less used worker) and do that the worker monitor the new process. If the process die or finish its running, the worker removes the process from the dictionary (sends a request for that to the leader).

When is needed to access to an existent process, the search is done in the workers and if the search is not ok, then the worker send the params to the leader. If the leader doesn't find the process then can do two things:

 * Create a new process and return its PID.
 * Return `undefined`.

> IMPORTANT: forseti is designed using C (consistency) and A (availability) from the [C-A-P theorem](http://en.wikipedia.org/wiki/CAP_theorem). We prefer to use forseti in a private network with a controlled connection between nodes. If you need to use forseti with connection between NOC (even if you use a VPN) is not recommended because you perhaps need to use somthing with P (partition-tolerant) and [evetual consistency](http://en.wikipedia.org/wiki/Eventual_consistency).

## Getting started

The implementation is very easy:

You can configure it in reltool.config like an OTP application.

An example config (usually app.config)

**IMPORTANT** if you use this way, you'll be sure forseti is loaded after all the needed resources for start to launch processes.

```erlang
{forseti, [
    %% max_retries and max_time defines the maximum restart frequency for the supervisor
    {max_retries, 20},                           %% 20 by default, no mandatory
    {max_time, 10 },                             %% 10 by default, no mandatory
    {nodes, ['node1@server1', 'node2@server2']}, %% [node()] by default, no mandatory
    {call, {test_forseti, start_link, []} }      %% mandatory
]}
```

Else you can use it in this way

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

Or passing more args to the init function:

```erlang
{Node,PID} = forseti:get_key(<<"mykey1">>, [make_ref()]),
```

In this case, the function to be called will be `start_link/2`.

If you only want to search a key and in case this not exist returns `undefined` you can use:

```erlang
case forseti:search_key(<<"mykey1">>) of
    undefined -> {error, notfound};
    {_Node,PID} -> {ok, PID}
end
```

Enjoy!

## Backends

Use only gen_server has several pros and cons so, I added more backends (with more pros and cons too) to let you decide what's the better implementation for your development.

### gen_leader

The first backend I use. It's faster than others, hasn't got SPOF (Single Point Of Failure) but you need to shutdown the cluster to add new nodes. And you can only use `gen_leader` once by node. If you plan to use `gen_leader` for your specific implementation, you should use another backend.

This backend is used by default so if you use the system as above, you're using it. But if you want to put in the configuration file or in the `start_link` function explicitly, you can do it as follow:

```erlang
forseti:start_link(gen_leader, Call, Nodes)
```

Or in the configuration file:

```erlang
{forseti, [
    {backend, gen_leader},
    {nodes, [nodes()]},
    {call, mfa()}
]}
```

### mnesia

As is implemented in `ejabberd`, you can use `mnesia` as the store for the processes. This backend lets you add and remove nodes from the cluster without restart the whole cluster. The worst part is the latency. If you plan to use forseti for high load requesting location for processes, creating and removing processes, perhaps you should use another backend.

**IMPORTANT**: the `lock_test` was not working for mnesia backend because takes a lot of time to release all the processes at the same time and mnesia is not blocked in the meantime. It's dangerous to use this backend if you use it for very short live processes.

To use this backend:

```erlang
forseti:start_link(mnesia, Call, Nodes)
```

In configuration file:

```erlang
{forseti, [
    {backend, mnesia},
    {nodes, [nodes()]},
    {call, mfa()}
]}
```

### locks

A scalable, deadlock-resolving resource locker, based on an algorithm designed by Ulf Wiger. With this backend you can add a new nodes without shutdown the cluster, but you need to start the system with one single node, after you can add more nodes.

To use this backend:
```erlang
forseti:start_link(locks, Call, Nodes)
```
In configuration file:

```erlang
{forseti, [
    {backend, locks},
    {nodes, [nodes()]},
    {call, mfa()}
]}
```
