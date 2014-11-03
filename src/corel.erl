-module(corel).

%% API
-export([ assert/1, assert/2 ]).
-export([ timestamp/0, timestamp/1 ]).
-export([ trace/0, trace/1, trace/2 ]).

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

dbg(on) ->
    case dbg:tracer() of
        {error,already_started} -> ok;
        {ok,_} ->
            {ok,_} = dbg:p(all,[c,sos,sol]), ok
    end;

dbg(off) -> dbg:stop_clear().

trace() ->
    dbg(off).

trace(M) ->
    ok = dbg(on),
    dbg:tpl(M,[{'_',[],[{message,{return_trace}}]}]).

trace(M,F) ->
    ok = dbg(on),
    dbg:tpl(M,F,[ {'_',[],[{message,{return_trace}}]} ]).
