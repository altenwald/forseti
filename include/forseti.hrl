-define(MAX_RETRIES, 20).
-define(MAX_TIME, 10).

-ifdef(NEW_DICT_TYPE).
-define(DICT_TYPE, (dict:dict())).
-else.
-define(DICT_TYPE, (dict())).
-endif.
