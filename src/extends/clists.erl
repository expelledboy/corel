-module(clists).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([ remove_duplicates/1, randomize/1 ]).

%% @doc remove duplicate terms in list preserving order
remove_duplicates([])    -> [];
remove_duplicates([H|T]) -> [H | [X || X <- remove_duplicates(T), X /= H]].

randomize(List) when is_list(List) ->
    [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])].

%% ===================================================================
%% eunit tests
%% ===================================================================

remove_duplicates_test() ->
    List = [1,5,1,2,3,4,5,5,6],
    ?assertEqual([1,5,2,3,4,6],remove_duplicates(List)).

randomize_test() ->
    List = [1,2,3,4,5],
    ?assert(randomize(List) =/= List orelse randomize(List) =/= List).
