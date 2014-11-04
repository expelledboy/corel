%% @copyright Anthony Jackson <expelledboy> All Rights Reserved
%% This file is subject to the terms and conditions defined in
%% file 'LICENSE.txt', which is part of this source code package.

-module(gen_uni_tests).

-include_lib("eunit/include/eunit.hrl").
-define(name, gen_uni_name).
-define(mod, gen_uni_mock).
-define(mock(Name,Action), meck:expect(?mod,Name,Action)).

basic_failover_test() ->
    catch exit(global:whereis_name(?name), kill),
    ?assertEqual(undefined, global:whereis_name(?name)),
    Args = Options = [],
    meck:new(?mod,[non_strict]),

    %% start leader
    ?mock(init,fun([]) -> {ok,{state,leader}} end),
    ?mock(init,fun([]) -> {ok,{state,leader}} end),
    ?mock(recover,fun({state,leader}) -> {commit,state_name,{state,leader}} end),
    ?mock(commit,fun(state_name,{state,leader}) -> ok end),
    {ok,Leader1} = gen_uni:start(?name, ?mod, Args, Options),
    ?assertEqual(Leader1, gen_uni:await(?name)),

    %% start follower
    ?mock(init,fun([]) -> {ok,{state,follower}} end),
    ?mock(challenge,fun({state,follower}) -> {ok,follower} end),
    ?mock(resolve,fun(follower,state_name,{state,leader}) -> false end),
    {ok,Follower} = gen_uni:start(?name, ?mod, Args, Options),
    ?assertEqual(Leader1, gen_uni:await(?name)),

    %% kill leader
    ?mock(recover,fun({state,follower}) -> {ok,state_name,{state,follower}} end),
    ok = ctest:ensure_exit(Leader1, kill),
    ?assertEqual(Follower, gen_uni:await(?name)),

    %% restart leader
    ?mock(init,fun([]) -> {ok,{state,leader}} end),
    ?mock(challenge,fun({state,leader}) -> {ok,leader} end),
    ?mock(resolve,fun(leader,state_name,{state,follower}) -> true end),
    ?mock(terminate,fun(handover,state_name,{state,follower}) -> {commit,state_name,{state,follower}} end),
    ?mock(commit,fun(state_name,{state,follower}) -> ok end),
    ?mock(recover,fun({state,leader}) -> {ok,state_name,{state,leader}} end),
    {ok,Leader2} = gen_uni:start(?name, ?mod, Args, Options),
    ok = ctest:await_exit(Follower),
    ?assertEqual(Leader2, gen_uni:await(?name)),

    ok = ctest:ensure_exit(Leader1, kill),
    ok = ctest:ensure_exit(Leader2, kill),
    ok = ctest:ensure_exit(Follower, kill),
    ok = meck:unload(?mod).
