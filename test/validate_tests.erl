-module(validate_tests).
-include_lib("eunit/include/eunit.hrl").

input_method_test_() ->
    IsTest = fun(Input) -> "test" == Input end,
    [
     {"validate with atom", ?_assertEqual(ok, validate:this("test", string)) },
     {"validate with tuple", ?_assertEqual(ok, validate:this("test", {string})) },
     {"validate from local function", ?_assertEqual(ok, validate:this("test", {is_test,fun is_test/1})) },
     {"validate from anonymous function", ?_assertEqual(ok, validate:this("test", {is_test,IsTest})) },
     {"list of validations of atom, tuple or function", ?_assertEqual(ok, validate:this("test", [{string},no_whitespace,{is_test,IsTest}])) },
     {"pass when given an empty list of validations", ?_assertEqual(ok, validate:this("test", [])) }
    ].

%% helper
is_test("test") -> true;
is_test(_) -> false.

%% ===================================================================

reduction_test_() ->
    {setup,
     fun() -> meck:new(validate,[passthrough]) end,
     fun(_) -> meck:unload(validate) end,
     [
      {"validations can depend on other validations", fun test_no_whitespace_assumes_string/0 },
      {"validations declared ealier are not included later", fun test_dups_not_reincluded/0 }
     ]
    }.

test_no_whitespace_assumes_string() ->
    meck:expect(validate, process,
                fun(_Input,Validations,_Result) ->
                        ?assertMatch([
                                      {{string}, _},
                                      {{no_whitespace},_}
                                     ], Validations),
                        ok
                end),
    ?assertEqual(ok, validate:this("1", [no_whitespace])),
    ?assert(meck:validate(validate)).

test_dups_not_reincluded() ->
    meck:expect(validate, process,
                fun(_Input,Validations,_Result) ->
                        ?assertMatch([
                                      {{string},_},
                                      {{no_whitespace},_},
                                      {{integer_string},_}
                                     ], Validations),
                        ok
                end),
    ?assertEqual(ok, validate:this("1", [integer_string,string,no_whitespace,string])),
    ?assert(meck:validate(validate)).

%% ===================================================================

validations_test_() ->
    Tests =
    [
     {"input is not undefined",
      [
       ?_assertEqual(true, validate:defined("test", [])),
       ?_assertEqual(false, validate:defined(undefined, []))
      ]
     },
     {"input does not equal a value which means nothing",
      [
       ?_assertEqual(true, validate:has_value("test", [])),
       ?_assertEqual(false, validate:has_value([], [])),
       ?_assertEqual(false, validate:has_value(null, [])),
       ?_assertEqual(false, validate:has_value(undefined, []))
      ]
     },
     {"input is of a type",
      [
       ?_assertEqual(true, validate:is_type(12, [integer])),
       ?_assertEqual(true, validate:is_type(12.12, [float])),
       ?_assertEqual(true, validate:is_type("test", [list])),
       ?_assertEqual(true, validate:is_type({test}, [tuple])),
       ?_assertEqual(true, validate:is_type(<<"test">>, [binary])),
       ?_assertEqual(true, validate:is_type(<<2#11110000:6>>,[bitstring])),
       ?_assertEqual(true, validate:is_type(true, [boolean])),
       ?_assertEqual(true, validate:is_type(fun io:format/1, [function])),
       ?_assertEqual(true, validate:is_type(make_ref(), [reference])),
       ?_assertEqual(true, validate:is_type(test, [atom])),
       ?_assertThrow(type_not_atom, validate:is_type(test, ["test"])),
       ?_assertThrow({invalid,{list,type_is_not,atom}}, validate:is_type("test", [atom]))
      ]
     },
     {"input is a string",
      [
       ?_assertEqual(true, validate:string("test", [])),
       ?_assertEqual(false, validate:string(test, []))
      ]
     },
     {"input is a integer in a string",
      [
       ?_assertEqual(true, validate:integer_string("12", [])),
       ?_assertEqual(false, validate:integer_string("test", [])),
       ?_assertEqual(false, validate:integer_string(test, []))
      ]
     },
     {"input is a float in a string",
      [
       ?_assertEqual(true, validate:float_string("12.12", [])),
       ?_assertEqual(false, validate:float_string("test", [])),
       ?_assertEqual(false, validate:float_string(test, [])),
       ?_assertMatch({error,{_,invalid,string}}, validate:this(12.12, float_string))
      ]
     },
     {"input is a numeric in a string",
      [
       ?_assertEqual(true, validate:numeric_string("12", [])),
       ?_assertEqual(true, validate:numeric_string("12.12", [])),
       ?_assertEqual(false, validate:numeric_string("test", [])),
       ?_assertMatch({error,{_,invalid,string}}, validate:this(12.12, numeric_string))
      ]
     },
     {"input is a string with no whitespace",
      [
       ?_assertEqual(true, validate:no_whitespace("test", [])),
       ?_assertEqual(false, validate:no_whitespace("test ", []))
      ]
     },
     {"input is a valid ip address",
      [
       ?_assertEqual(true, validate:ip_address("127.0.0.1", [])),
       ?_assertEqual(false, validate:ip_address("test", [])),
       ?_assertMatch({error,{_,invalid,string}}, validate:this(12.12, ip_address))
      ]
     },
     {"input is a memeber of a collection",
      [
       ?_assertEqual(true, validate:member_of("test", [["test",test]])),
       ?_assertEqual(true, validate:member_of(test, [["test",test]])),
       ?_assertEqual(false, validate:member_of(invalid, [["test",test]])),
       ?_assertEqual(false, validate:member_of("test", [[]]))
      ]
     },
     {"input is of a give length",
      [
       ?_assertEqual(true, validate:len("test", [4])),
       ?_assertEqual(false, validate:len("test", [5])),
       ?_assertThrow(length_not_integer, validate:this(test, {len,"4"})),
       ?_assertThrow(length_not_postive, validate:this(test, {len,-1})),
       ?_assertThrow({not_supported,atom}, validate:this(test, {len,4}))
      ]
     },
     {"input is a pid which is alive",
      fun test_pid_is_alive/0
     },
     {"input is a string which matched a regex pattern",
      [
       ?_assertEqual(true, validate:regex("test", ["^t.*t$"])),
       ?_assertEqual(false, validate:regex("invalid", ["^t.*t$"])),
       ?_assertMatch({error,{_,invalid,string}}, validate:this(12.12, {regex,"^$"})),
       ?_assertThrow(regex_not_string, validate:this("12.12", {regex,'^$'}))
      ]
     },
     {"input is valid'ish email address",
      [
       ?_assertEqual(false, validate:email_address("test", [])),
       ?_assertEqual(true, validate:email_address("test@example.com", [])),
       ?_assertEqual(true, validate:email_address("Test_Email+tag@subdom.example.co.za", [])),
       ?_assertMatch({error,{_,invalid,string}}, validate:this(test, email_address))
      ]
     },
     {"input is a credit card",
      [
       ?_assertEqual(true, validate:credit_card("6011000990139424", [])),
       ?_assertEqual(false, validate:credit_card("123451234512345", [])),
       ?_assertEqual(true, validate:credit_card("4012888888881881", [visa])),
       ?_assertEqual(true, validate:credit_card("5105105105105100", [mastercard])),
       ?_assertEqual(true, validate:credit_card("371449635398431", [american_express])),
       ?_assertEqual(true, validate:credit_card("6011000990139424", [discover])),
       ?_assertMatch({error,{_,invalid,integer_string}}, validate:this("test", credit_card))
      ]
     },
     {"input is a phone number",
      [
       ?_assertEqual(true, validate:phone_number("0111231234", [])),
       ?_assertEqual(true, validate:phone_number("011 123 1234", [])),
       ?_assertEqual(true, validate:phone_number("011-123-1234", [])),
       ?_assertEqual(true, validate:phone_number("+27821231234", [])),
       ?_assertEqual(true, validate:phone_number("27111231234", [])),
       ?_assertEqual(true, validate:phone_number("(011)1231234", [])),
       ?_assertEqual(true, validate:phone_number("(+2711) 123 1234", [])),
       ?_assertEqual(true, validate:phone_number("(011) 123-1234", [])),
       ?_assertEqual(false, validate:phone_number("(44+)020-12341234", [])),
       ?_assertEqual(false, validate:phone_number("12341234(+020)", [])),
       ?_assertEqual(false, validate:phone_number("(1)(2)(3)", [])),
       ?_assertMatch({error,{_,invalid,string}}, validate:this(test, phone_number))
      ]
     }
    ],
    {inparallel,Tests}.

test_pid_is_alive() ->
    PID = spawn(fun()-> receive exit -> ok end end),
    ?assert(validate:pid_alive(PID, [])),
    PID ! exit, timer:sleep(100),
    ?assertNot(validate:pid_alive(PID, [])),
    ?assertMatch({error,{_,invalid,{atom,type_is_not,pid}}}, validate:this(not_a_pid, [pid_alive])).
