%%------------------------------------------------------------------------------

-module(corel_re).

%% API
-export([
         matches/2
        ]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

matches(Regex,String) when is_list(Regex), is_list(String) ->
  case re:run(String,Regex) of
    {match,_} -> true;
    _ -> false
  end.

%%------------------------------------------------------------------------------
%% type specs
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
