

# Module forseti_mnesia #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_call-2">add_call/2</a></td><td></td></tr><tr><td valign="top"><a href="#choose_node-0">choose_node/0</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_metrics-0">get_metrics/0</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_call-2"></a>

### add_call/2 ###

<pre><code>
add_call(Name::<a href="#type-call_name">call_name()</a>, Call::<a href="#type-call">call()</a>) -&gt; ok
</code></pre>
<br />

<a name="choose_node-0"></a>

### choose_node/0 ###

<pre><code>
choose_node() -&gt; node()
</code></pre>
<br />

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="find-2"></a>

### find/2 ###

<pre><code>
find(Name::<a href="#type-call_name">call_name()</a>, Key::<a href="#type-key">key()</a>) -&gt; {ok, pid()} | undefined
</code></pre>
<br />

<a name="get-3"></a>

### get/3 ###

<pre><code>
get(Name::<a href="#type-call_name">call_name()</a>, Key::<a href="#type-key">key()</a>, Args::[term()]) -&gt; {ok, pid()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

<a name="get_metrics-0"></a>

### get_metrics/0 ###

<pre><code>
get_metrics() -&gt; [{node(), pos_integer()}]
</code></pre>
<br />

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(X1, From, Call) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(X1, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(X1, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Nodes::[node()]) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

