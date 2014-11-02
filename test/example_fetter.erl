-module(example_fetter).
-compile({parse_transform, fetter}).

%% -behaviour(perpetuerl).
%% TODO implement perpetuerl ( database abstraction and serialization library )
-export([ spec/1, validate/1 ]).

-record(?MODULE, {
           username,
           password,
           balance=0,
           active,
           active_months=0,
           services=[]
          }).

spec(username) -> [pkey];
spec(password) -> [{access,[user]},encrypted,string];
spec(balance) -> [{access,[user,admin]},integer];
spec(active) -> [boolean];
spec(active_months) -> [integer];
spec(services) -> [{proplist,[pay2me,valueit]}];
spec({services,pay2me}) -> [defined,boolean]; % defined or required
spec({services,valueit}) -> [{valueit,fun valueit/1}];
spec({fetter,?MODULE}) -> [serializable].

valueit(Value) ->
    lists:member(Value,[active,fraud,archived]).

validate(Object) ->
    Username = Object#?MODULE.username,
    Password = Object#?MODULE.password,
    ReversePassword = lists:reverse(Password),
    Pay2MeActive = opts:fget(Object#?MODULE.services,pay2me),
    ValuITDefined = opts:defined(Object#?MODULE.services,valueit),
    if
        Username == Password -> false;
        Username == ReversePassword -> false;
        Pay2MeActive and not ValuITDefined -> false;
        true -> true
    end.

%% TODO simplify example_spec_fsm and move implementation here
