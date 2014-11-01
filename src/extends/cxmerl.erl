-module(cxmerl).
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([
         escape_string/1,
         parse/1,
         extract_fields/2
        ]).
-export([xml/0]).

parse(String) when is_list(String) ->
    Table = ets:new(xmerl_rules, [set]),
    try
        {Xdoc,_} = xmerl_scan:string(String, [{space,normalize}, {acc_fun, fun normalize/3}, {rules,Table}]),
        {ok,Xdoc}
    catch C:E -> {error,[{class,C},{reason,E},{raw,String}]}
    after ets:delete(Table)
    end.


escape_string(String) when is_list(String) ->
    lists:flatten(lists:map(fun escape_character/1, String)).

escape_character(Character) when Character > 127, is_integer(Character) ->
    "&#" ++ integer_to_list(Character) ++ ";";
escape_character(Character) when is_integer(Character) ->
    encode(Character).

%% ([{'==',name,person},{'==',pos,1}],[contents])
%% ([{'==',name,person},{'==',pos,1}],[contents])
%% get_value(Xdoc,Condition) ->

extract_fields(XDoc,Fields) ->
    F = fun(FieldLoc) -> to_text(xmerl_xpath:string(FieldLoc,XDoc)) end,
    lists:map(F, Fields).

%% ===================================================================
%% internal functions
%% ===================================================================

%% @private
encode($\") -> "&quot;";
encode($&)  -> "&amp;";
encode($\') -> "&apos;";
encode($<)  -> "&lt;";
encode($>)  -> "&gt;";
encode($\n) -> "&#10;";
encode(Normal) -> Normal.

%% @private
normalize(#xmlText{value = " ", pos = P}, Acc, S) -> {Acc, P, S};  % remove spaces
normalize(#xmlComment{pos = P}, Acc, S) -> {Acc, P, S};  % remove comments
normalize(X, Acc, S) ->
    {[X|Acc], S}.

%% @private
to_text([]) -> [];
to_text([Item]) -> to_text(Item);
to_text(#xmlElement{ content = Content }) -> to_text(Content);
to_text(#xmlText{ value = Value }) -> Value;
to_text(#xmlAttribute{ value = Value }) -> Value;
to_text(List) when is_list(List) ->
    lists:map(fun to_text/1, List).

%% ===================================================================
%% eunit tests
%% ===================================================================

xml() ->
    "<person>"
        "<!-- comment -->"
        "<name>Anthony</name>"
        "<username>expelledboy</username>"
        "<skills>"
            "<item>programming</item>"
            "<item>being awesome</item>"
        "</skills>"
        "<characters> &lt;'slash &amp; burn'&gt; \n</characters>"
    "</person>".

parse_test() ->
    ?assertMatch({ok,#xmlElement{ name = person }}, parse(xml())).

escape_string_test_() ->
    ASCII = "I remember when, I remember, I remember when I lost my mind!",
    [
     {"ascii is not effected", ?_assertEqual(ASCII, escape_string(ASCII)) },
     {"ascii charaters that are treated differently", ?_assertEqual("&quot; &lt;&apos;slash &amp; burn&apos;&gt; &#10;", escape_string("\" <'slash & burn'> \n")) },
     {"characters outside ascii should be escaped", ?_assertEqual("&#128;"++"&#149;"++"&#167;", escape_string([128,149,167])) }
    ].

extract_fields_test() ->
    {ok,Doc} = parse(xml()),
    ?assertEqual(["Anthony",["programming","being awesome"]], extract_fields(Doc,["//person/name","//person/skills/*"])).
