-ifdef(DEBUG).
-define(dbg(S, As), ok).
-else.
-define(dbg(S, As), (begin io:fwrite(user, <<"\t~s:~w:~w: ~s\n">>, [?FILE, ?LINE, self(), io_lib:format((S), (As))]), ok end)).
-endif.
