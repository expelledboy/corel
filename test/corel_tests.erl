-module(corel_tests).
-include_lib("eunit/include/eunit.hrl").

assert_test() ->
    ?assertEqual(ok, corel:assert(fun() -> ok end)),
    ?assertEqual("result", corel:assert(fun() -> {ok,"result"} end)),
    ?assertThrow("issue", corel:assert(fun() -> {error,"issue"} end)),

    ?assertEqual(ok, corel:assert(fun() -> true end,exception)),
    ?assertThrow(exception, corel:assert(fun() -> false end,exception)),
    ?assertError({assert,not_boolean}, corel:assert(fun() -> "term" end,exception)).

timestamp_test() ->
    Now = {1404,599956,660656},
    DateTime = calendar:now_to_local_time(Now),
    ?assertEqual("2014-07-06 00:39:16", corel:timestamp(DateTime)),
    ?assertEqual("2014-07-06 00:39:16.660656", corel:timestamp(Now)).
