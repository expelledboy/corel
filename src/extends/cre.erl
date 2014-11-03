%% @copyright Anthony Jackson <expelledboy> All Rights Reserved
%% This file is subject to the terms and conditions defined in
%% file 'LICENSE.txt', which is part of this source code package.

-module(cre).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([ matches/2 ]).

matches(Regex,String) when is_list(Regex), is_list(String) ->
    case re:run(String,Regex) of
        {match,_} -> true;
        _ -> false
    end.

%% ===================================================================
%% eunit tests
%% ===================================================================

matches_test() ->
    PhoneNumberRegex = "^(\\(?\\+?[0-9]{2,5}\\)?)?([0-9 ]{2,4}-?){1,3}$",
    ?assert(matches(PhoneNumberRegex,"(011)1231234")),
    ?assert(matches(PhoneNumberRegex,"(+2711) 123 1234")),
    ?assertNot(matches(PhoneNumberRegex,"(44+)020-12341234")),
    ?assertNot(matches(PhoneNumberRegex,"(1)(2)(3)")).
