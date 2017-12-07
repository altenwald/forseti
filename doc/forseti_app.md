

# Module forseti_app #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`application`](application.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_mod_backend-0">get_mod_backend/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_mod_backend-1">get_mod_backend/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_mod_backend-0"></a>

### get_mod_backend/0 ###

<pre><code>
get_mod_backend() -&gt; atom()
</code></pre>
<br />

<a name="get_mod_backend-1"></a>

### get_mod_backend/1 ###

<pre><code>
get_mod_backend(X1::atom() | {ok, atom()}) -&gt; atom()
</code></pre>
<br />

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(StartType::normal | {takeover, node()} | {failover, node()}, StartArgs::term()) -&gt; {ok, pid()} | {ok, pid(), State::term()} | {error, Reason::term()}
</code></pre>
<br />

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(State::term()) -&gt; term()
</code></pre>
<br />

