-module(cstring).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([
         is_made_of_characters/2,
         remove_characters/2,
         format/2, format/3
        ]).

is_made_of_characters(String,CharList) when is_list(String), is_list(CharList) ->
    lists:all(fun(Char)-> lists:member(Char,CharList) end,String).

remove_characters(String,CharList) when is_list(String), is_list(CharList) ->
    lists:filter(fun(Char)-> not lists:member(Char,CharList) end,String).

format(Format,Params) ->
    lists:flatten(io_lib:format(Format, Params)).

format(Format,Params,binary) ->
    list_to_binary(io_lib:format(Format, Params)).

%% ===================================================================
%% eunit tests
%% ===================================================================

format_test() ->
    ?assertEqual([116,101,115,116,58,"123"],io_lib:format("test:~B",[123])),
    ?assertEqual("test:123",format("test:~B",[123])),
    ?assertEqual(<<"test:123">>,format("test:~B",[123],binary)).

remove_characters_test() ->
    ?assertEqual("27741234567",remove_characters("cellphone : + 27-74 123 4567"," -+cellphone:")).

is_made_of_characters_test() ->
    ?assertNot(is_made_of_characters("cellphone : + 27-74 123 4567",lists:seq($0, $9))),
    ?assert(is_made_of_characters("27741234567",lists:seq($0, $9))).
