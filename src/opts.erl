%% @copyright Anthony Jackson <expelledboy> All Rights Reserved
%% This file is subject to the terms and conditions defined in
%% file 'LICENSE.txt', which is part of this source code package.

-module(opts).
-compile({parse_transform, autohelp}).

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
         validate/1
        ]).

%% @type not_present_error() = {'not_present', list( {'options',opts:type()} , {'key',atom()} )}

%% ===================================================================
%% API
%% ===================================================================

%% @doc get values of options
%% @throws not_present_error()
fget(Opts,Option) when is_list(Opts) and is_atom(Option) ->
    corel:assert(validate(Opts),{not_opts,Opts}),
    fget_opt(Opts,Option);
fget(Opts,Options) when is_list(Opts) and is_list(Options) ->
    corel:assert(validate(Opts),{not_opts,Opts}),
    lists:map(fun(Option) -> fget_opt(Opts,Option) end,Options).

fget_opt(Opts,Option) ->
    case lists:keyfind(Option,1,Opts) of
        false -> throw({not_present,[{options,Opts},{key,Option}]});
        {Option,Value} -> Value
    end.

%% ===================================================================

%% @doc get values of options using defaults when not present
fget(Opts,Option,Default) when is_list(Opts) and is_atom(Option) ->
    corel:assert(validate(Opts),{not_opts,Opts}),
    fget_opt_default(Opts,Option,Default);
fget(Opts,Options,Default) when is_list(Opts) and is_list(Options) ->
    corel:assert(validate(Opts),{not_opts,Opts}),
    lists:map(fun(Option) -> fget_opt_default(Opts,Option,Default) end,Options).

fget_opt_default(Opts,Option,Default) ->
    case lists:keyfind(Option,1,Opts) of
        false -> Default;
        {Option,Value} -> Value
    end.

%% ===================================================================

%% @doc set value of option
%% @throws not_present_error()
fset(Opts,Option,Value) when is_list(Opts) and is_atom(Option) ->
    corel:assert(validate(Opts),{not_opts,Opts}),
    fset_opt(Opts,Option,Value).

%% @doc using proplist fset values for options
%% @throws not_present_error()
fset(Opts,Options) when is_list(Opts) and is_list(Options) ->
    corel:assert(validate(Opts),{not_opts,Opts}),
    lists:foldl(fun({Option,Value},Acc) -> fset_opt(Acc,Option,Value) end,Opts,Options).

fset_opt(Opts,Option,Value) ->
    case defined(Opts,Option) of
        false -> throw({not_present,[{options,Opts},{key,Option}]});
        true -> replace(Opts,Option,Value)
    end.

%% ===================================================================

%% @doc add option
insert(Opts,Option,Value) when is_list(Opts) and is_atom(Option) ->
    [{Option,Value}|Opts].

%% @doc using proplist add options
insert(Opts,Options) when is_list(Opts) and is_list(Options) ->
    Opts ++ Options.

%% ===================================================================

%% @doc delete option
remove(Opts,Option) when is_list(Opts) and is_atom(Option) ->
    lists:keydelete(Option,1,Opts);
remove(Opts,Options) when is_list(Opts) and is_list(Options) ->
    lists:foldl(fun(Option,Acc) -> remove(Acc,Option) end,Opts,Options).

%% ===================================================================

%% @doc replace the option with a new value
replace(Opts,Option,Value) when is_list(Opts) and is_atom(Option) ->
    Pairs1 = lists:keydelete(Option,1,Opts),
    [{Option,Value}|Pairs1].

%% @doc replace the options using another proplist
replace(Opts,Options) when is_list(Opts) and is_list(Options) ->
    lists:foldl(fun({Option,Value},Acc) -> replace(Acc,Option,Value) end,Opts,Options).

%% ===================================================================

%% @doc determine if options are present
defined(Opts,Option) when is_list(Opts) and is_atom(Option) ->
    case proplists:lookup(Option,Opts) of
        none -> false;
        _Defined -> true
    end;
defined(Opts,Options) when is_list(Opts) and is_list(Options) ->
    AreDefined = lists:map(fun(Option) -> defined(Opts,Option) end,Options),
    lists:all(fun(X) -> X == true end,AreDefined).

%% ===================================================================

validate(List) when is_list(List)->
    lists:all(fun(X) -> (is_tuple(X) and (size(X) =< 2)) orelse is_atom(X) end, List);
validate(_BadArg) ->
    false.
