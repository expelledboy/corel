%% API
-module(spec_fsm).

-export([ run/3 ]).

%% entry point of processing
%% based on event decide to change state or execute action
%% should be side effect free
-callback event(StateObject::term(), Event::term()) -> 'ok' | tuple('action'|'change', term()).

%% process task and generate events
%% can also shortcut to state change
-callback action(StateObject::term(), Action::term()) -> 'ok' | tuple('event'|'change', term()).

%% the end of processing
%% only point at which state can be changed
-callback change(StateObject::term(), Change::term()) -> tuple('ok', NewStateObject::term()).

%% ===================================================================
%% API
%% ===================================================================

run(Module,StateObject,Event) when is_atom(Module) ->
    loop(event,Module,StateObject,Event).

loop(Function=event,Module,StateObject,Event) ->
    case Module:event(StateObject,Event) of
        {action,Action} -> loop(action,Module,StateObject,Action);
        {change,Change} -> loop(change,Module,StateObject,Change);
        ok -> {ok,StateObject};
        BadRet -> throw({bad_return,[{module,Module},{function,Function},{return,BadRet}]})
    end;
loop(Function=action,Module,StateObject,Action) ->
    case Module:action(StateObject,Action) of
        {event,Event} -> loop(event,Module,StateObject,Event);
        {change,Change} -> loop(change,Module,StateObject,Change);
        ok -> {ok,StateObject};
        BadRet -> throw({bad_return,[{module,Module},{function,Function},{return,BadRet}]})
    end;
loop(Function=change,Module,StateObject,Change) ->
    case Module:change(StateObject,Change) of
        {ok,NewStateObject} -> {ok,NewStateObject};
        BadRet -> throw({bad_return,[{module,Module},{function,Function},{return,BadRet}]})
    end.
