%%------------------------------------------------------------------------------

-module(corel_string).

%% API
-export([
         is_made_of_characters/2,
         remove_characters/2
        ]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

is_made_of_characters(String,CharList) when is_list(String), is_list(CharList) ->
  lists:all(fun(Char)-> lists:member(Char,CharList) end,String).

%%------------------------------------------------------------------------------

remove_characters(String,CharList) when is_list(String), is_list(CharList) ->
  lists:filter(fun(Char)-> not lists:member(Char,CharList) end,String).

%%------------------------------------------------------------------------------
%% type specs
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
