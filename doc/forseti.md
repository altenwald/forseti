

# Module forseti #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_call-2">add_call/2</a></td><td></td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_key-1">get_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_key-2">get_key/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_less_used_node-0">get_less_used_node/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_metrics-0">get_metrics/0</a></td><td></td></tr><tr><td valign="top"><a href="#search_key-1">search_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-5">start_link/5</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_call-2"></a>

### add_call/2 ###

<pre><code>
add_call(Name::<a href="#type-call_name">call_name()</a>, X2::<a href="#type-call">call()</a>) -&gt; ok
</code></pre>
<br />

<a name="find-2"></a>

### find/2 ###

<pre><code>
find(Name::<a href="#type-call_name">call_name()</a>, Key::any()) -&gt; {ok, pid()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Name::<a href="#type-call_name">call_name()</a>, Key::any()) -&gt; {ok, pid()} | {error, Reason::atom()}
</code></pre>
<br />

<a name="get-3"></a>

### get/3 ###

<pre><code>
get(Name::<a href="#type-call_name">call_name()</a>, Key::any(), Args::[term()]) -&gt; {ok, pid()} | {error, Reason::atom()}
</code></pre>
<br />

<a name="get_key-1"></a>

### get_key/1 ###

<pre><code>
get_key(Key::any()) -&gt; {node(), pid()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

<a name="get_key-2"></a>

### get_key/2 ###

<pre><code>
get_key(Key::any(), Args::[term()]) -&gt; {node(), pid()} | {error, Reason::atom()}
</code></pre>
<br />

<a name="get_less_used_node-0"></a>

### get_less_used_node/0 ###

<pre><code>
get_less_used_node() -&gt; node()
</code></pre>
<br />

<a name="get_metrics-0"></a>

### get_metrics/0 ###

<pre><code>
get_metrics() -&gt; [<a href="#type-node_metrics">node_metrics()</a>]
</code></pre>
<br />

<a name="search_key-1"></a>

### search_key/1 ###

<pre><code>
search_key(Key::any()) -&gt; {ok, PID::pid()} | undefined
</code></pre>
<br />

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Nodes::[node()]) -&gt; {ok, pid()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Call::<a href="#type-call">call()</a>, Nodes::[node()]) -&gt; {ok, pid()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

<a name="start_link-3"></a>

### start_link/3 ###

<pre><code>
start_link(Backend::<a href="#type-backend">backend()</a>, Call::<a href="#type-call">call()</a>, Nodes::[node()]) -&gt; {ok, pid()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

<a name="start_link-4"></a>

### start_link/4 ###

<pre><code>
start_link(MaxR::<a href="#type-max_retries">max_retries()</a>, MaxT::<a href="#type-max_time">max_time()</a>, Call::<a href="#type-call">call()</a>, Nodes::[node()]) -&gt; {ok, pid()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

<a name="start_link-5"></a>

### start_link/5 ###

<pre><code>
start_link(Backend::<a href="#type-backend">backend()</a>, MaxR::<a href="#type-max_retries">max_retries()</a>, MaxT::<a href="#type-max_time">max_time()</a>, Call::<a href="#type-call">call()</a>, Nodes::[node()]) -&gt; {ok, pid()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

