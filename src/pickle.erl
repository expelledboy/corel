%% @headerfile "corel.hrl"
-include_lib("eunit/include/eunit.hrl").
-module(pickle).

%% API
-export([ dump/2, load/1 ]).

%% ===================================================================
%% API
%% ===================================================================

dump(Filename, Term) when not is_binary(Term) ->
    dump(Filename, term_to_binary(Term));
dump(Filename, Binary) when is_list(Filename) ->
    file:write_file(Filename, Binary).

load(Filename) when is_list(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    binary_to_term(Binary).

%% ===================================================================
%% type specs
%% ===================================================================

%% API
-spec dump(string(),term()) -> 'ok' | {'error',atom()}.
-spec load(string()) -> term().

%% ===================================================================
%% eunit tests
%% ===================================================================

pickle_test() ->
    Data = {random, term},
    File = "/tmp/pickle.test",
    file:delete(File),
    ?assertEqual(ok, pickle:dump(File, Data)),
    ?assertEqual(Data, pickle:load(File)),
    ?assertEqual(ok, file:delete(File)).
