-define(MAX_RETRIES, 20).
-define(MAX_TIME, 10).

-ifdef(OLD_DICT_TYPE).
-define(DICT_TYPE, (dict())).
-else.
-define(DICT_TYPE, (dict:dict())).
-endif.

-type call() :: {module(), atom(), [term()]}.
-type call_name() :: atom().
-type backend() :: gen_leader | locks | mnesia.
-type max_retries() :: pos_integer().
-type max_time() :: pos_integer().

-type reason() :: atom().

-type node_metrics() :: {node(), integer()}.

-type key() :: term().
