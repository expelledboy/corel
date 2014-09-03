%%--------------------------------------------------------------------
%% File        -> test/pickle_tests.erl
%% Author      -> Anthony Jackson
%% Description -> Serializing terms to and from disk
%%--------------------------------------------------------------------

-module(pickle_tests).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% tests
%%--------------------------------------------------------------------

pickle_test_() ->
  Data = {random, term},
  File = "/tmp/pickle.test",
  [
   {"check the file content is identical to the input",
    fun() ->
        file:delete(File),
        ?assertEqual(ok, pickle:dump(File, Data)),
        ?assertEqual(Data, pickle:load(File)),
        ?assertEqual(ok, file:delete(File))
    end
   }
  ].

%%--------------------------------------------------------------------
%% Copyright: gmail -> expelledboy - All rights reserved
%% vim: set ft=erlang :
%%--------------------------------------------------------------------
