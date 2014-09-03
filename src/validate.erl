%%------------------------------------------------------------------------------

-module(validate).

-ifdef(TEST).
-compile(export_all).
-define(process(Input,Validations,_), ?MODULE:process(Input,Validations,ok)).
-else.
-define(process(Input,Validations,_), process(Input,Validations,ok)).
-endif.

%% API
-export([
         this/2
        ]).

%% Validations
-export([
         defined/2,
         has_value/2,
         is_type/2,
         string/2,
         integer_string/1, integer_string/2,
         float_string/1, float_string/2,
         numeric_string/1, numeric_string/2,
         no_whitespace/1, no_whitespace/2,
         ip_address/1, ip_address/2,
         member_of/2,
         len/2,
         pid_alive/1, pid_alive/2,
         regex/1, regex/2,
         email_address/1, email_address/2,
         credit_card/1, credit_card/2,
         phone_number/1, phone_number/2
        ]).

-define(RE_EMAIL, "^[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-zA-Z0-9])?\\.)+(?:[a-zA-Z]{2}|com|org|net|edu|gov|mil|biz|info|mobi|name|aero|asia|jobs|museum)$").
-define(RE_VISA, "^4[0-9]{12}(?:[0-9]{3})?$").
-define(RE_MASTERCARD, "^5[1-5][0-9]{14}$").
-define(RE_AMERICAN_EXPRESS, "^3[47][0-9]{13}$").
-define(RE_DISCOVER, "^6(?:011|5[0-9]{2})[0-9]{12}$").
-define(RE_CREDIT_CARD, "(?:"++?RE_VISA++"|"++?RE_MASTERCARD++"|"++?RE_AMERICAN_EXPRESS++"|"++?RE_DISCOVER++")").
-define(RE_PHONE_NUMBER, "^(\\(?\\+?[0-9]{2,5}\\)?)?([0-9 ]{2,4}-?){1,3}$").

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

this(Input, Validation) when is_atom(Validation);
                             is_tuple(Validation) ->
  this(Input, [Validation]);
this(Input, Validations) when is_list(Validations) ->
  Included = lists:flatten([ includes(V) || V <- Validations ]),
  Reduced = remove_dups(Included),
  Normalized = lists:flatten([ normalize(V) || V <- Reduced ]),
  ?process(Input, Normalized, ok).

%%------------------------------------------------------------------------------

includes({_,Function}=Validation) when is_function(Function) ->
  [Validation];
includes(Validation) ->
  includes([],[Validation]).

includes(Processed, [Validation|Validations]) when is_atom(Validation) ->
  includes(Processed,[{Validation}|Validations]);
includes(Processed, [Validation|Unprocessed]) when is_tuple(Validation) ->
  Callback = element(1,Validation),
  IsDefined = erlang:function_exported(?MODULE, Callback, 2),
  HasIncludes = erlang:function_exported(?MODULE, Callback, 1),
  Validations = case {IsDefined,HasIncludes} of
                  {true,true} ->
                    Includes = apply(?MODULE, Callback, [include]),
                    Includes ++ Unprocessed;
                  {true,false} -> Unprocessed;
                  _ -> error(validation_unkown)
                end,
  includes([Validation|Processed],Validations);
includes(_, [Validation|_]) ->
  error({bad_validation,Validation});
includes(Processed, []) ->
  Processed.

remove_dups([])    -> [];
remove_dups([H|T]) -> [H | [X || X <- remove_dups(T), X /= H]].

normalize({_,Function}=Validation) when is_function(Function) ->
  Validation;
normalize(CallbackSpec) when is_tuple(CallbackSpec) ->
  [Callback|Params] = tuple_to_list(CallbackSpec),
  normalize(CallbackSpec,Callback,Params).

normalize(CallbackSpec,Callback,Params) ->
  Function = fun(Input) -> apply(?MODULE, Callback, [Input,Params]) end,
  {CallbackSpec,Function}.

%%------------------------------------------------------------------------------
%% main
%%------------------------------------------------------------------------------

process(Input, [{Validation,Function}|Rest], ok) when is_function(Function) ->
  Result = execute(Validation,Function,Input),
  process(Input, Rest, Result);
process(_, _, {error,_}=Invalid) -> Invalid;
process(_, [], ok) -> ok.

execute(Validation,Function,Input) ->
  try Function(Input) of
    true -> ok;
    false -> 
      case Validation of
        {Atom} -> {error, {Input, invalid, Atom}};
        _      -> {error, {Input, invalid, Validation}}
      end
  catch
    throw:{invalid,Reason} ->
      {error,{Input, invalid, Reason}}
  end.

%%------------------------------------------------------------------------------
%% validations
%%------------------------------------------------------------------------------

defined(undefined,[]) -> false;
defined(_,_) -> true.

has_value(undefined,[]) -> false;
has_value(null,[]) -> false;
has_value([],[]) -> false;
has_value(_,_) -> true.

is_type(_,[Type]) when not is_atom(Type) ->
  throw(type_not_atom);
is_type(Input,[Type]) ->
  case catch type:which(Input) of
    Type -> true;
    OtherType -> throw({invalid,{OtherType,type_is_not,Type}})
  end.

string(Input,[]) when is_list(Input) ->
  io_lib:printable_list(Input);
string(_,[]) ->
  false.

integer_string(include) -> [no_whitespace].
integer_string(Input,[]) when is_list(Input) ->
  Int = (catch list_to_integer(Input)),
  is_integer(Int);
integer_string(_,_) ->
  false.

float_string(include) -> [no_whitespace].
float_string(Input,[]) when is_list(Input) ->
  Float = (catch list_to_float(Input)),
  is_float(Float);
float_string(_,_) ->
  false.

numeric_string(include) -> [no_whitespace].
numeric_string(Input,[]) ->
  Float = (catch erlang:list_to_float(Input)),
  Int = (catch erlang:list_to_integer(Input)),
  is_number(Float) orelse is_number(Int).

no_whitespace(include) -> [string].
no_whitespace(Input,[]) when is_list(Input) ->
  Input == lists:filter(fun(X) -> not lists:member(X," \f\n\r\t\v") end,Input);
no_whitespace(_,_) ->
  false.

ip_address(include) -> [no_whitespace].
ip_address(String,[]) when is_list(String) ->
  SubElements = string:tokens(String,"."),
  case {length(SubElements), lists:all(fun(Element) -> integer_string(Element,[]) end,SubElements)} of
    {4,true} -> true;
    {_,_} -> false
  end;
ip_address(_,[]) ->
  false.

member_of(Item,[List]) when is_list(List) -> 
  lists:member(Item,List);
member_of(_,[NotList]) ->
  throw({not_list,NotList}).

len(_,[Length]) when not is_integer(Length) ->
  throw(length_not_integer);
len(_,[Length]) when Length < 0 ->
  throw(length_not_postive);
len(String,[Length]) when is_list(String) ->
  case string_length(String) of
    Length -> true;
    _ -> false
  end;
len(Input,_) ->
  throw({not_supported,type_of(Input)}).

pid_alive(include) -> [{is_type,pid}].
pid_alive(PID,[]) when is_pid(PID) ->
  is_process_alive(PID).

regex(include) -> [string].
regex(_,[Regex]) when not is_list(Regex) ->
  throw(regex_not_string);
regex(String,[Regex]) when is_list(String) ->
  match_regex(Regex,String).

email_address(include) -> [string].
email_address(String,[]) when is_list(String) ->
  match_regex(?RE_EMAIL,String).

credit_card(include) -> [integer_string].
credit_card(String,[]) when is_list(String) ->
  match_regex(?RE_CREDIT_CARD,String);
credit_card(String,[visa]) when is_list(String) ->
  match_regex(?RE_VISA,String);
credit_card(String,[mastercard]) when is_list(String) ->
  match_regex(?RE_MASTERCARD,String);
credit_card(String,[american_express]) when is_list(String) ->
  match_regex(?RE_AMERICAN_EXPRESS,String);
credit_card(String,[discover]) when is_list(String) ->
  match_regex(?RE_DISCOVER,String).

phone_number(include) -> [string].
phone_number(String,[]) when is_list(String) ->
  match_regex(?RE_PHONE_NUMBER,String).

%%------------------------------------------------------------------------------
%% helpers
%%------------------------------------------------------------------------------

string_encoding(String) when is_list(String) ->
  ASCII =
  fun
    (Char) when is_integer(Char),Char >= 0, Char =< 255 -> true;
    (Char) when is_integer(Char),Char > 255 -> false
  end,
  case lists:all(ASCII,String) of
    true -> ascii;
    false -> unicode
  end.

string_length(String) when is_list(String) ->
  case string_encoding(String) of
    ascii ->
      length(String);
    unicode ->
      UC = unicode:characters_to_list(list_to_binary(String)),
      length(UC)
  end.

type_of(X) when is_integer(X)   -> integer;
type_of(X) when is_float(X)     -> float;
type_of(X) when is_list(X)      -> list;
type_of(X) when is_tuple(X)     -> tuple;
type_of(X) when is_binary(X)    -> binary;
type_of(X) when is_bitstring(X) -> bitstring;
type_of(X) when is_boolean(X)   -> boolean;
type_of(X) when is_function(X)  -> function;
type_of(X) when is_pid(X)       -> pid;
type_of(X) when is_port(X)      -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X)      -> atom;
type_of(_X)                     -> unknown.

match_regex(Regex,String) when is_list(Regex), is_list(String) ->
  case re:run(String,Regex) of
    {match,_} -> true;
    _ -> false
  end.

%%------------------------------------------------------------------------------
%% type specs
%%------------------------------------------------------------------------------

-type compiled_validation() :: atom() | tuple().
-type runtime_validation() :: {atom() | tuple(),fun((_) -> boolean())}.
-type validation() :: compiled_validation() | runtime_validation().
-type invalid() :: {'error',{_,'invalid',atom() | tuple()}}.
-type validation_result() :: 'ok' | invalid().
-type input() :: any().

%% API
-spec this(input(),validation() | [validation()]) -> validation_result().
-spec includes(validation()) -> [tuple()].
-spec includes([tuple()],[validation()]) -> [tuple()].
-spec remove_dups([any()]) -> [any()].
-spec normalize(tuple()) -> runtime_validation().
-spec normalize(tuple(),atom(),[any()]) -> runtime_validation().

%% main
-spec process(input(),[runtime_validation()],validation_result()) -> validation_result().
-spec execute(tuple(),fun((_) -> any()),input()) -> validation_result().

%% validations
-spec defined(_,_) -> boolean().
-spec has_value(_,_) -> boolean().
-spec is_type(_,_) -> boolean().
-spec string(_,_) -> boolean().
-spec integer_string('include') -> [compiled_validation()].
-spec integer_string(_,_) -> boolean().
-spec float_string('include') -> [compiled_validation()].
-spec float_string(_,_) -> boolean().
-spec numeric_string('include') -> [compiled_validation()].
-spec numeric_string(_,_) -> boolean().
-spec no_whitespace('include') -> [compiled_validation()].
-spec no_whitespace(_,_) -> 'false'.
-spec ip_address('include') -> [compiled_validation()].
-spec ip_address(_,_) -> 'false'.
-spec member_of(_,[[any()]]) -> boolean().
-spec len(string(),[integer()]) -> boolean().
-spec pid_alive('include') -> [compiled_validation()].
-spec pid_alive(pid(),_) -> boolean().
-spec regex('include') -> [compiled_validation()].
-spec regex(string(),[string()]) -> boolean().
-spec email_address('include') -> [compiled_validation()].
-spec email_address(string(),_) -> boolean().
-spec credit_card('include') -> [compiled_validation()].
-spec credit_card(string(),_) -> boolean().
-spec phone_number('include') -> [compiled_validation()].
-spec phone_number(string(),_) -> boolean().

%% helpers
-spec string_encoding(string()) -> 'ascii' | 'unicode'.
-spec string_length(string()) -> non_neg_integer().
-spec type_of(_) -> 'atom' | 'bitstring' | 'boolean' | 'float' | 'function' | 'integer' | 'list' | 'pid' | 'port' | 'reference' | 'tuple' | 'unknown'.
-spec match_regex(string(),string()) -> boolean().

%%------------------------------------------------------------------------------
