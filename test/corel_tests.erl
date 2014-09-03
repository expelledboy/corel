%%------------------------------------------------------------------------------

-module(corel_tests).
-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------

assert_test_() ->
  [
   ?_assertEqual(ok, corel:assert(fun() -> ok end)),
   ?_assertEqual("result", corel:assert(fun() -> {ok,"result"} end)),
   ?_assertThrow("issue", corel:assert(fun() -> {error,"issue"} end)),

   ?_assertEqual(ok, corel:assert(fun() -> true end,exception)),
   ?_assertThrow(exception, corel:assert(fun() -> false end,exception)),
   ?_assertError({assert,not_boolean}, corel:assert(fun() -> "term" end,exception))
  ].

timestamp_test_() ->
  Now = {1404,599956,660656},
  DateTime = calendar:now_to_local_time(Now),
  [
   ?_assertEqual("2014-07-06 00:39:16", corel:timestamp(DateTime)),
   ?_assertEqual("2014-07-06 00:39:16.660656", corel:timestamp(Now))
  ].

%%------------------------------------------------------------------------------
