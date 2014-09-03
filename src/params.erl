%%------------------------------------------------------------------------------

-module(params).

%% API
-export([
         fget/2,
         fget/3,
         fset/2,
         fset/3,
         insert/2,
         insert/3,
         remove/2,
         replace/2,
         replace/3,
         defined/2,
         is_params/1
        ]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

%% @doc get values of params
%% @throws {not_present,[{params,Pairs},{key,Param}]}
fget(Pairs,Param) when is_list(Pairs) and is_atom(Param) ->
  assert_params(Pairs),
  fget_param(Pairs,Param);
fget(Pairs,Params) when is_list(Pairs) and is_list(Params) ->
  assert_params(Pairs),
  lists:map(fun(Param) -> fget_param(Pairs,Param) end,Params).

fget_param(Pairs,Param) ->
  case lists:keyfind(Param,1,Pairs) of
    false -> throw({not_present,[{params,Pairs},{key,Param}]});
    {Param,Value} -> Value
  end.

%%------------------------------------------------------------------------------

%% @doc get values of params using defaults when not present
fget(Pairs,Param,Default) when is_list(Pairs) and is_atom(Param) ->
  assert_params(Pairs),
  fget_param_default(Pairs,Param,Default);
fget(Pairs,Params,Default) when is_list(Pairs) and is_list(Params) ->
  assert_params(Pairs),
  lists:map(fun(Param) -> fget_param_default(Pairs,Param,Default) end,Params).

fget_param_default(Pairs,Param,Default) ->
  case lists:keyfind(Param,1,Pairs) of
    false -> Default;
    {Param,Value} -> Value
  end.

%%------------------------------------------------------------------------------

%% @doc set value of param
%% @throws {not_present,[{params,Pairs},{key,Param}]}
fset(Pairs,Param,Value) when is_list(Pairs) and is_atom(Param) ->
  assert_params(Pairs),
  fset_param(Pairs,Param,Value).

%% @doc using proplist fset values for params
%% @throws {not_present,[{params,Pairs},{key,Param}]}
fset(Pairs,Params) when is_list(Pairs) and is_list(Params) ->
  assert_params(Pairs),
  lists:foldl(fun({Param,Value},Acc) -> fset_param(Acc,Param,Value) end,Pairs,Params).

fset_param(Pairs,Param,Value) ->
  case defined(Pairs,Param) of
    false -> throw({not_present,[{params,Pairs},{key,Param}]});
    true -> replace(Pairs,Param,Value)
  end.

%%------------------------------------------------------------------------------

%% @doc add params
insert(Pairs,Param,Value) when is_list(Pairs) and is_atom(Param) ->
  [{Param,Value}|Pairs].

%% @doc using proplist add params
insert(Pairs,Params) when is_list(Pairs) and is_list(Params) ->
  Pairs ++ Params.

%%------------------------------------------------------------------------------

%% @doc delete params
remove(Pairs,Param) when is_list(Pairs) and is_atom(Param) ->
  lists:keydelete(Param,1,Pairs);
remove(Pairs,Params) when is_list(Pairs) and is_list(Params) ->
  lists:foldl(fun(Param,Acc) -> remove(Acc,Param) end,Pairs,Params).

%%------------------------------------------------------------------------------

%% @doc replace the param with a new value
replace(Pairs,Param,Value) when is_list(Pairs) and is_atom(Param) ->
  Pairs1 = lists:keydelete(Param,1,Pairs),
  [{Param,Value}|Pairs1].

%% @doc replace the params using another proplist
replace(Pairs,Params) when is_list(Pairs) and is_list(Params) ->
  lists:foldl(fun({Param,Value},Acc) -> replace(Acc,Param,Value) end,Pairs,Params).

%%------------------------------------------------------------------------------

%% @doc determine if params are present
defined(Pairs,Param) when is_list(Pairs) and is_atom(Param) ->
  lists:keymember(Param,1,Pairs);
defined(Pairs,Params) when is_list(Pairs) and is_list(Params) ->
  AreDefined = lists:map(fun(Param) -> defined(Pairs,Param) end,Pairs,Params),
  lists:all(fun(X) -> X == true end,AreDefined).

%%------------------------------------------------------------------------------

is_params(BadArg) when not is_list(BadArg) ->
  false;
is_params(List) when is_list(List) ->
  Pairs = [ true || {_,_} <- List ],
  length(List) == length(Pairs).

%%------------------------------------------------------------------------------
%% helpers
%%------------------------------------------------------------------------------

assert_params(Term) ->
  corel:assert(fun() -> is_params(Term) end,{not_params,Term}).

%%------------------------------------------------------------------------------
%% type specs
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
