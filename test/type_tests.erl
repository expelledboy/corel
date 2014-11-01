-module(type_tests).
-include_lib("eunit/include/eunit.hrl").

which_test() ->
    ?assertEqual(boolean, type:which(true)),
    ?assertEqual(atom, type:which(atom)),
    ?assertEqual(list, type:which([list])),
    ?assertEqual(integer, type:which(123)),
    ?assertEqual(tuple, type:which({tuple})),
    ?assertEqual(binary, type:which(<<"binary">>)),
    ?assertEqual(float, type:which(1.23)),
    ?assertEqual(pid, type:which(self())),
    ?assertEqual(function, type:which(fun()-> type end)),
    ?assertEqual(reference, type:which(make_ref())).

to_atom_test() ->
    %% TODO must create a sain version of float_to_list
    %% ?assertEqual('1.23', type:to_atom(1.23))
    ?assertEqual(atom, type:to_atom(atom)),
    ?assertEqual(string, type:to_atom("string")),
    ?assertEqual(binary, type:to_atom(<<"binary">>)).

to_list_test() ->
    ?assertEqual("list", type:to_list("list")),
    ?assertEqual("atom", type:to_list(atom)),
    ?assertEqual("123", type:to_list(123)),
    ?assertEqual([tuple], type:to_list({tuple})),
    ?assertEqual("binary", type:to_list(<<"binary">>)).

to_integer_test() ->
    ?assertEqual(123, type:to_integer(123)),
    ?assertEqual(123, type:to_integer("123")),
    ?assertEqual(123, type:to_integer('123')),
    ?assertEqual(123, type:to_integer(<<"123">>)).
