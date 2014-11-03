-module(gen_uni).
-include_lib("eunit/include/eunit.hrl").
-define(DEBUG, true).

%% API
-export([ start_link/4, start/4 ]).
-export([ start/2, handover/3 ]).
-export([ await/1, await/2 ]).

%% gen callback
-export([ init_it/6 ]).

-define(TTL, 1000*60*5).
-define(NETWORK_TIMEOUT, 10000).
-define(AWAIT_TIMEOUT, 5000).
-define(AWAIT_POLL, 100).

-record(state, { parent,
                 module,
                 name,
                 state_name,
                 state_data,
                 context,
                 message,
                 options }).

-define(parent(S), S#state.parent).
-define(mod(S), S#state.module).
-define(name(S), S#state.name).
-define(act(S), S#state.state_name).
-define(data(S), S#state.state_data).
-define(cxt(S), S#state.context).
-define(msg(S), S#state.message).
-define(opts(S), S#state.options).

-define(opt(S,Key), opts:fget(S#state.options,Key)).

%% ===================================================================
%% behaviour
%% ===================================================================

-type name() :: tuple() | atom().
-type context() :: term().
-type state() :: term().
-type state_name() :: atom().
-type reason() :: term().
-type state_change() :: {'commit', state_name(), state()}.

-callback context(state_name(),state()) -> {'ok', context()}.
-callback recover(context()) -> state_change() | {'error',reason()}.
-callback commit(context()) -> 'ok'.

-callback handover(context(), state_name(), state()) -> boolean().
-callback resolve(name(), pid(), pid()) -> pid().
-callback terminate(reason(), state_name(), state()) -> 'ok'.

call_info(Info,S) -> (?mod(S)):(?act(S))(Info, ?data(S)).
call_context(S) -> (?mod(S)):context(?act(S),?data(S)).
call_recover(S) -> (?mod(S)):recover(?cxt(S)).
call_commit(S) -> (?mod(S)):commit(?cxt(S)).
call_handover(RContext,S) -> (?mod(S)):handover(RContext, ?act(S), ?data(S)).
call_terminate(Reason,S) -> (?mod(S)):terminate(Reason, ?act(S), ?data(S)).

%% ===================================================================
%% API
%% ===================================================================

start_link(Name, Mod, Args, Options) ->
    gen:start(?MODULE, link, Mod, Args, [{name,Name}|Options]).

start(Name, Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Mod, Args, [{name,Name}|Options]).

await(Name) ->
    await(Name,[]).

await(Name,Timeout) when is_integer(Timeout) ->
    await(Name,[{timeout,Timeout}]);

await(Name,Options) ->
    Poll = opts:fget(Options,poll,?AWAIT_POLL),
    Timeout = opts:fget(Options,timeout,?AWAIT_TIMEOUT),
    case Timeout rem Poll of
        0 -> poll_name(Name,Poll,Timeout);
        _ -> throw(poll_not_divisible)
    end.

handover(Pid,RPid,Context) ->
    callback(Pid,{handover,RPid,Context}).

start(Pid,Context) ->
    callback(Pid,{start,Context}).

%% ===================================================================

callback(Pid,Msg) ->
    ?debugFmt("callback ~p ~p",[Pid,Msg]),
    gen:call(Pid,?MODULE,{callback,Msg}).

poll_name(Name,_,T) when T < 0 ->
    throw({await,timeout,Name});
poll_name(Name,P,T) ->
    timer:sleep(P),
    case global:whereis_name(Name) of
        undefined -> poll_name(Name,P,T-P);
        Pid when is_pid(Pid) -> Pid
    end.

%% ===================================================================
%% gen callback
%% ===================================================================

init_it(Starter, Parent, _Self, Mod, Args, Options) ->
    Name = opts:fget(Options,name),
    S = #state{ parent = Parent,
                module = Mod,
                name = Name,
                state_name = init,
                state_data = Args,
                options = Options },
    case catch call_context(S) of
        {ok,Context} ->
            ?debugMsg("process alive"),
            proc_lib:init_ack(Starter, {ok, self()}),
            start(S#state{context=Context});
        {error, Reason} ->
            proc_lib:init_ack(Starter, {error, Reason}),
            exit(Reason);
        {'EXIT', Reason} ->
            proc_lib:init_ack(Starter, {error, Reason}),
            exit(Reason);
        Else ->
            Error = {bad_return_value, Else},
            proc_lib:init_ack(Starter, {error, Error}),
            exit(Error)
    end.

start(S) ->
    case register_name(?name(S), S) of
        true ->
            ?debugMsg("become leader"),
            recover(S);
        {existing,Pid} ->
            ?debugMsg("need to resolve"),
            resolve(Pid,S)
    end.

recover(S) ->
    ?debugMsg("recovering state"),
    case catch call_recover(S) of
        {commit, StateName, StateData} ->
            commit(S#state{state_name=StateName,state_data=StateData});
        Return -> common(Return,S)
    end.

commit(S0) ->
    ?debugMsg("getting context"),
    {ok,Context} = call_context(S0),
    ?debugMsg("commiting context"),
    S = S0#state{context=Context},
    case catch call_commit(S) of
        ok ->
            leader(S);
        Return -> common(Return,S)
    end.

resolve(Pid,S) ->
    case call_remote(Pid,handover,[self(),?cxt(S)]) of
        {ok,true} -> shell(S);
        {ok,false} ->
            Ref = erlang:monitor(process,Pid),
            follower(Ref,S)
    end.

%% ===================================================================

leader(S) ->
    ?debugMsg("leader loop"),
    receive
        {?MODULE,From, {callback,{handover,RPid,RContext}}} = Msg ->
            handover(RPid,RContext, From, S#state{message=Msg});
        Info ->
            info(Info, S#state{message=Info})
    end.

shell(S) ->
    ?debugMsg("shell loop"),
    receive
        {?MODULE, From, {callback,{start,Context}}} = Msg ->
            gen:reply(From,ok),
            start(S#state{context=Context,message=Msg});
        Info ->
            terminate({unexpected_message, Info}, S#state{message=Info})
    after
        ?NETWORK_TIMEOUT ->
            terminate(timeout_during_handover, S)
    end.

follower(Ref,S) ->
    ?debugMsg("follower loop"),
    receive
        {'DOWN',Ref,process,_From,_Reason}=Msg ->
            start(S#state{message=Msg});
        Info ->
            terminate({unexpected_message, Info}, S#state{message=Info})
    end.

%% ===================================================================

info(Info, S) ->
    ?debugVal(Info),
    case catch call_info(Info,S) of
        {ok, StateName, StateData} ->
            leader(S#state{state_name=StateName,state_data=StateData});
        {ok, StateData} ->
            leader(S#state{state_data=StateData});
        Return -> common(Return,S)
    end.

handover(RPid, RContext,From,S) ->
    case catch call_handover(RContext,S) of
        true ->
            ok = global:unregister_name(?name(S)),
            gen:reply(From,true),
            terminate({handover,RPid},S);
        false ->
            gen:reply(From,false),
            leader(S);
        Return ->
            common(Return,S)
    end.

common(Return,S) ->
    ?debugFmt("common ~p",Return),
    case Return of
        {stop, Reason, StateData} -> terminate(Reason, S#state{state_data=StateData});
        {'EXIT', What} -> terminate(What, S);
        _ -> terminate({bad_return_value, Return}, S)
    end.

terminate({handover,Pid}, S) ->
    ?debugMsg("doing handover"),
    case catch call_context(S) of
        {ok, Context} ->
            ok = call_remote(Pid,start,[Context]),
            terminate(handover,S);
        Return -> common(Return,S)
    end;

terminate(Reason, S) ->
    case catch call_terminate(Reason,S) of
        {'EXIT', What} ->
            fatal(What, S),
            exit(What);
        _ ->
            exit(Reason)
    end.

%% ===================================================================
%% private functions
%% ===================================================================

fatal(What, S) ->
    error_logger:format(
      "Singleton ~p terminating reason:~p last_message:~p statename:~p state:~p",
      [self(),What,?msg(S),?act(S),?data(S)]).

register_name(Name, S) ->
    Mod = ?mod(S),
    case global:register_name(Name, self(), fun Mod:resolve/3) of
        yes -> true;
        no ->
            case global:whereis_name(Name) of
                undefined -> register_name(Name, S);
                Pid -> {existing,Pid}
            end
    end.

call_remote(Pid,Fun,Args) ->
    ?debugFmt("call_remote ~p ~p",[Fun,Args]),
    Node = erlang:node(Pid),
    rpc:call(Node,?MODULE,Fun,[Pid|Args],?NETWORK_TIMEOUT).
