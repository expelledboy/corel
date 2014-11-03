%% @copyright Anthony Jackson <expelledboy> All Rights Reserved
%% This file is subject to the terms and conditions defined in
%% file 'LICENSE.txt', which is part of this source code package.

-module(corel_bench).
-compile([export_all]).

new(ID) ->
    {ok, ID}.

run(ping, _KeyGen, _ValueGen, State) ->
    %% _ID = State,
    %% _Key = _KeyGen(),
    %% _Value = _ValueGen(),
    timer:sleep(200),
    {ok, State}.
