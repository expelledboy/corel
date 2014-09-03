%%------------------------------------------------------------------------------

-module(corel_xmerl).

%% API
-export([
         escape_string/1,
         % get_value/2,
         parse/1
        ]).

-include_lib("xmerl/include/xmerl.hrl").

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

escape_string(String) when is_list(String) ->
  xmerl_lib:export_text(String).

%%------------------------------------------------------------------------------

% ([{'==',name,person},{'==',pos,1}],[contents])
% ([{'==',name,person},{'==',pos,1}],[contents])

% get_value(Xdoc,Condition) ->

%%------------------------------------------------------------------------------

parse(String) when is_list(String) ->
  Table = ets:new(xmerl_rules, [set]),
  try
    {Xdoc,_} = xmerl_scan:string(String, [{space,normalize}, {acc_fun, fun normalize/3}, {rules,Table}]),
    {ok,Xdoc}
  catch C:E -> {error,[{class,C},{reason,E},{raw,String}]}
  after ets:delete(Table)
  end.

normalize(#xmlText{value = " ", pos = P}, Acc, S) -> {Acc, P, S};  % remove spaces
normalize(#xmlComment{pos = P}, Acc, S) -> {Acc, P, S};  % remove comments
normalize(X, Acc, S) ->
  io:format("DEBUG: ~p:~p: ~p = ~p\n",[ ?MODULE, ?LINE, "X", X ]),
  {[X|Acc], S}.

%%------------------------------------------------------------------------------
%% type specs
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
