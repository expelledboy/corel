-module(cache).
-include_lib("eunit/include/eunit.hrl").

-export([ value/2, reset/1 ]).

value({process,Name,Key},Fun) when is_function(Fun,0) ->
    case get({Name,Key}) of
        undefined ->
            Value = Fun(),
            put({Name,Key},Value),
            Value;
        Value ->
            Value
    end;

value({application,Name,Key},Fun) when is_function(Fun,0) ->
    case application:get_env(Name,Key) of
        undefined ->
            Value = Fun(),
            ok = application:set_env(Name,Key,Value),
            Value;
        {ok,Value} ->
            Value
    end;

value({ets,Name,Key},Fun) when is_function(Fun,0) ->
                                                % XXX going to crash if no ets table
    case ets:lookup(Name,Key) of
        [] ->
            Value = Fun(),
            true = ets:insert(Name,{Key,Value}),
            Value;
        [{Key,Value}] ->
            Value
    end.

reset({application,Name,Key}) ->
    ok = application:unset_env(Name,Key);

reset({process,Name,Key}) ->
    erase({Name,Key}),
    ok;

reset({ets,Name,Key}) ->
    true = ets:delete(Name,Key),
    ok.

%% ===================================================================
%% eunit tests
%% ===================================================================

cache_test_() ->
    {setup,
     fun() -> ets:new(test,[public, named_table]) end,
     fun(_) -> ets:delete(test) end,
     [
      cache_test_(process),
      cache_test_(application),
      cache_test_(ets)
     ]}.

cache_test_(Backend) ->
    Work = fun() -> timer:sleep(40), value end,
    [
     ?_assertEqual(value, value({Backend,test,key},Work)),
     ?_assert(begin {Time,value} = timer:tc(fun value/2,[{Backend,test,key},Work]), Time < 40 end),
     ?_assertEqual(ok, reset({Backend,test,key})),
     ?_assertNot(begin {Time,value} = timer:tc(fun value/2,[{Backend,test,key},Work]), Time < 40 end),
     ?_assertEqual(ok, reset({Backend,test,key}))
    ].
