%%------------------------------------------------------------------------------

-module(type_tests).
-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------

which_test_() ->
  [
   ?_assertEqual(boolean, type:which(true)),
   ?_assertEqual(atom, type:which(atom)),
   ?_assertEqual(list, type:which([list])),
   ?_assertEqual(integer, type:which(123)),
   ?_assertEqual(tuple, type:which({tuple})),
   ?_assertEqual(binary, type:which(<<"binary">>)),
   ?_assertEqual(float, type:which(1.23)),
   ?_assertEqual(pid, type:which(self())),
   ?_assertEqual(function, type:which(fun()-> type end)),
   ?_assertEqual(reference, type:which(make_ref()))
  ].

to_atom_test_() ->
  [
   ?_assertEqual(atom, type:to_atom(atom)),
   ?_assertEqual(string, type:to_atom("string")),
   ?_assertEqual(binary, type:to_atom(<<"binary">>))
   % TODO must create a sain version of float_to_list
   % ?_assertEqual('1.23', type:to_atom(1.23))
  ].

to_list_test_() ->
  [
   ?_assertEqual("list", type:to_list("list")),
   ?_assertEqual("atom", type:to_list(atom)),
   ?_assertEqual("123", type:to_list(123)),
   ?_assertEqual([tuple], type:to_list({tuple})),
   ?_assertEqual("binary", type:to_list(<<"binary">>))
  ].

to_integer_test_() ->
  [
   ?_assertEqual(123, type:to_integer(123)),
   ?_assertEqual(123, type:to_integer("123")),
   ?_assertEqual(123, type:to_integer('123')),
   ?_assertEqual(123, type:to_integer(<<"123">>))
  ].

%%------------------------------------------------------------------------------
