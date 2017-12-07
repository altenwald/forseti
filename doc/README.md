

# Forseti #

Copyright (c) 2014-2017 Altenwald Solutions, S.L.

__Authors:__ "Manuel Rubio" ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

[![Build Status](https://img.shields.io/travis/altenwald/forseti/master.svg)](https://travis-ci.org/altenwald/forseti)
[![Codecov](https://img.shields.io/codecov/c/github/altenwald/forseti.svg)](https://codecov.io/gh/altenwald/forseti)
[![License: LGPL 2.1](https://img.shields.io/github/license/altenwald/forseti.svg)](https://raw.githubusercontent.com/altenwald/forseti/master/COPYING)
[![Hex](https://img.shields.io/hexpm/v/forseti.svg)](https://hex.pm/packages/forseti)

[Forseti](http://en.wikipedia.org/wiki/Forseti) is a balancer and distribution system for Erlang processes. The basic idea for this system is keep a basic leader/worker structure to store a process dictionary. When is needed a new process, the request should be done to the leader, the leader spawns a new process in a worker (less used worker) and the worker starts to monitor the new process. If the process dies or finishes, the worker removes the process from the dictionary (sends a request for that to the leader).

When is needed to access to an existent process, the search is done in the workers and if the search is unsuccessful, the worker send the params to the leader. If the leader doesn't find the process then it has two options:

* Creates a new process and returns its PID (if you used `forseti:get_key/1`).
* Returns `undefined` (if you used `forseti:search/1`).

> IMPORTANT: forseti is designed using C (consistency) and A (availability) from the [C-A-P theorem](http://en.wikipedia.org/wiki/CAP_theorem). We prefer to use forseti in a private network with a controlled connection between nodes. If you need to use different NOCs (even if you use a VPN) is not recommended to use forseti. In this situation you'll require partition-tolerant and [eventual consistency](http://en.wikipedia.org/wiki/Eventual_consistency).


### <a name="Getting_started">Getting started</a> ###

The implementation is very easy. You can configure it in `reltool.config` or in case you use **relx** you can add it to the `.app` file only. An example config (usually `app.config` or `sys.config`):

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

Otherwise you can use it in this way:

```erlang
Call = {test_forseti, start_link, []},
Nodes = [node1@server1, node2@server2],
forseti:start_link(Call, Nodes),
```

The basic implementation for `test_forseti` should be a **gen_server** with a `start_link` as follows:

```erlang
start_link(_Key) ->
    gen_server:start_link(?MODULE, [], []).
```

The function passed as a param in the form `{M,F,A}` should has the first param as `Key`.

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


### <a name="Multi_Call_Configuration">Multi Call Configuration</a> ###

When you need to use forseti for more than one group of processes in the same virtual machine, you need to configure several calls to initiate the new processes and even a way to avoid collisions to the other configurations.

Note that if you don't use this new set of functions, you are using `default` keyword to locate your unique configuration actually.

The configuration could be in this way:

```erlang
{forseti, [
    %% max_retries and max_time defines the maximum restart frequency for the supervisor
    {max_retries, 20},                           %% 20 by default, no mandatory
    {max_time, 10 },                             %% 10 by default, no mandatory
    {nodes, ['node1@server1', 'node2@server2']}, %% [node()] by default, no mandatory
    {call, [                                     %% mandatory
        {default, {test_forseti, start_link, []}},
        {users, {users, start_link, []}}
    ]}
]}
```

Using `start_link` you can start forseti using this way:

```erlang
forseti:start_link([node1@server1, node2@server2]),
forseti:add_call(default, {test_forseti, start_link, []}),
forseti:add_call(users, {users, start_link, []}),
```

For the first configuration (`default`) you can still continue using the functions you saw in the previous sections. If you want to use explicitly `default` or if you want to use `users`, you need to do it in this way:

```erlang
{ok,PID} = forseti:get(default, <<"mykey1">>),
{ok,PID} = forseti:find(default, <<"mykey1">>),
```

The new functions are `forseti:get/2`, `forseti:get/3` and `forseti:find/2`.


### <a name="Backends">Backends</a> ###

Use only gen_server has several pros and cons so, I added more backends (with more pros and cons too) to let you decide what's the better implementation for your development.


#### <a name="gen_leader">gen_leader</a> ####

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


#### <a name="mnesia">mnesia</a> ####

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


#### <a name="locks">locks</a> ####

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



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="forseti.md" class="module">forseti</a></td></tr>
<tr><td><a href="forseti_app.md" class="module">forseti_app</a></td></tr>
<tr><td><a href="forseti_leader.md" class="module">forseti_leader</a></td></tr>
<tr><td><a href="forseti_leader_server.md" class="module">forseti_leader_server</a></td></tr>
<tr><td><a href="forseti_lib.md" class="module">forseti_lib</a></td></tr>
<tr><td><a href="forseti_locks.md" class="module">forseti_locks</a></td></tr>
<tr><td><a href="forseti_locks_server.md" class="module">forseti_locks_server</a></td></tr>
<tr><td><a href="forseti_mnesia.md" class="module">forseti_mnesia</a></td></tr>
<tr><td><a href="forseti_sup.md" class="module">forseti_sup</a></td></tr></table>

