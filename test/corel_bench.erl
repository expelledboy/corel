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
