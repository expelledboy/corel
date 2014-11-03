%% @copyright Anthony Jackson <expelledboy> All Rights Reserved
%% This file is subject to the terms and conditions defined in
%% file 'LICENSE.txt', which is part of this source code package.

-module(gen_uni_tests).

-include_lib("eunit/include/eunit.hrl").

-define(name, gen_uni_name).
-define(mock, gen_uni_mock).

basic_failover_test() ->
    catch exit(global:whereis_name(?name), kill),
    ?assertEqual(undefined, global:whereis_name(?name)),
    Args = Options = [],
    meck:new(?mock,[non_strict]),

    %% start leader
    meck:expect(?mock,context,fun
                                  (init,[]) -> {ok,{context,leader}}; %1
                                  (state_name,leader) -> {ok,{context,leader}} %3
                              end),
    meck:expect(?mock,recover,fun({context,leader}) -> {commit,state_name,leader} end), %2
    meck:expect(?mock,commit,fun({context,leader}) -> ok end), %4
    {ok,Leader1} = gen_uni:start(?name, ?mock, Args, Options),
    ?assertEqual(Leader1, gen_uni:await(?name)),

    %% start follower
    meck:expect(?mock,context,fun (init,[]) -> {ok,{context,follower}} end),
    meck:expect(?mock,handover,fun ({context,follower},state_name,leader) -> false end),
    {ok,Follower} = gen_uni:start(?name, ?mock, Args, Options),
    ?assertEqual(Leader1, gen_uni:await(?name)),

    %% kill leader
    meck:expect(?mock,recover,fun({context,follower}) -> {commit,state_name,follower} end),
    meck:expect(?mock,context,fun (state_name,follower) -> {ok,{context,follower}} end),
    meck:expect(?mock,commit,fun({context,follower}) -> ok end),
    ok = ctest:ensure_exit(Leader1, kill),
    ?assertEqual(Follower, gen_uni:await(?name)),

    %% restart leader
    meck:expect(?mock,context,fun
                                  (init,[]) -> {ok,{context,leader}};
                                  (state_name,follower) -> {ok,{context,from_follower}};
                                  (state_name,leader) -> {ok,{context,leader}}
                              end),
    meck:expect(?mock,handover,fun ({context,leader},state_name,follower) -> true end),
    meck:expect(?mock,terminate,fun (handover,state_name,follower) -> ok end),
    meck:expect(?mock,recover,fun({context,from_follower}) -> {commit,state_name,leader} end),
    meck:expect(?mock,commit,fun({context,leader}) -> ok end),
    {ok,Leader2} = gen_uni:start(?name, ?mock, Args, Options),
    ?assertEqual(Leader2, gen_uni:await(?name)),
    ok = ctest:await_exit(Follower),

    ok = ctest:ensure_exit(Leader1, kill),
    ok = ctest:ensure_exit(Leader2, kill),
    ok = ctest:ensure_exit(Follower, kill),
    ok = meck:unload(?mock).
