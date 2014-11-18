%% @copyright Anthony Jackson <expelledboy> All Rights Reserved
%% This file is subject to the terms and conditions defined in
%% file 'LICENSE.txt', which is part of this source code package.

-module(ctest).

-export([ mfa_string/1 ]).
-export([ await_exit/1, ensure_exit/2, after_exit/1, set_env/3 ]).
-export([ setup/0, call/3, cast/2, recv/2, reply/2 ]).

mfa_string(Fun) when is_function(Fun) ->
    {module,M} = erlang:fun_info(Fun, module),
    {name,F} = erlang:fun_info(Fun, name),
    {arity,A} = erlang:fun_info(Fun, arity),
    mfa_string({M,F,A});
mfa_string({M,F,A}) ->
    io_lib:format("~w:~w/~w", [M,F,A]).

%% ===================================================================

await_exit(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, _, _, _} ->
            ok
    end.

ensure_exit(Pid,Reason) ->
    catch exit(Pid,Reason),
    await_exit(Pid).

after_exit(Fun) when is_function(Fun,0)->
    From = self(),
    proc_lib:spawn(
      fun() ->
              try
                  MRef = erlang:monitor(process,From),
                  receive
                      {'DOWN', MRef, process, From, _Info} ->
                          {after_exit,ok} = {after_exit,Fun()}
                  end
              catch
                  error:{badmatch,{after_exit,Res}} ->
                      error_logger:error_msg("** ctest:after_exit(fun ~s) -> ~p~n",[mfa_string(Fun),Res]);
                  Class:Error ->
                      error_logger:error_msg("** ctest:after_exit(fun ~s) -> ~p(~p)~n",[mfa_string(Fun),Class,Error])
              end
      end),
    ok.

set_env(App,Param,Value) ->
    ok = application:set_env(App,Param,Value),
    after_exit(fun() -> application:unset_env(App,Param) end).

%% ===================================================================

setup() ->
    From = self(),
    ok = set_env(?MODULE,test_pid,From).

call(Tag,Msg,Timeout) ->
    try
        {ok,TestPid} = application:get_env(?MODULE,test_pid),
        gen:call(TestPid,Tag,Msg,Timeout)
    catch
        exit:timeout ->
            error_logger:error_msg("** ctest:call(~p,~p,~p) -> throw(timeout)~n",[Tag,Msg,Timeout]),
            error({ctest,timeout})
    end.

cast(Tag,Msg) ->
    case application:get_env(?MODULE,test_pid) of
        {ok,TestPid} when is_pid(TestPid) ->
            TestPid ! {Tag,Msg}
    end.

recv(Tag,Timeout) ->
    receive
        {Tag,Msg}   -> Msg;
        {Tag,From,Msg} -> {From,Msg}
    after
        Timeout ->
            receive
                Msg ->
                    error_logger:error_msg("** ctest:recv(~p,~p) -> ~p % bad_message~n",[Tag,Timeout,Msg]),
                    error({bad_message,Msg})
            after
                0 ->
                    error_logger:error_msg("** ctest:recv(~p,~p) -> throw(timeout)~n",[Tag,Timeout]),
                    error({ctest,timeout})
            end
    end.

reply(From,Msg) ->
    gen:reply(From,Msg).
