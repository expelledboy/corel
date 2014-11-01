-module(pickle_tests).
-include_lib("eunit/include/eunit.hrl").

pickle_test() ->
    Data = {random, term},
    File = "/tmp/pickle.test",
    file:delete(File),
    ?assertEqual(ok, pickle:dump(File, Data)),
    ?assertEqual(Data, pickle:load(File)),
    ?assertEqual(ok, file:delete(File)).
