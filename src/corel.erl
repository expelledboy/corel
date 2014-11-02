-module(corel).

%% API
-export([ assert/1, assert/2 ]).
-export([ timestamp/0, timestamp/1 ]).

assert(Fun) when is_function(Fun) ->
    case Fun() of
        ok -> ok;
        {ok,Result} -> Result;
        {error,Error} -> throw(Error)
    end.

assert(Fun,Exception) when is_function(Fun) ->
    case Fun() of
        true -> ok;
        false -> throw(Exception);
        _ -> error({assert,not_boolean})
    end;

assert(true, _) -> ok;
assert(false,Exception) -> throw(Exception).

timestamp() ->
    timestamp(os:timestamp()).

%% ISO8601
timestamp({_,_,Micros}=DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_local_time(DateTime),
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0B", [Year, Month, Day, Hour, Min, Sec, Micros]));
timestamp({{Year,Month,Day},{Hour,Min,Sec}}) ->
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Min, Sec])).
