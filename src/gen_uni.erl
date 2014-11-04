%% @copyright Anthony Jackson <expelledboy> All Rights Reserved
%% This file is subject to the terms and conditions defined in
%% file 'LICENSE.txt', which is part of this source code package.

-module(gen_uni).
-include_lib("eunit/include/eunit.hrl").
-define(DEBUG, true).

%% API
-export([ start_link/4, start/4 ]).
-export([ resolve/2 ]).
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
                 %% context,
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
-type state() :: term().
-type state_name() :: atom().
-type challenge() :: term().
-type reason() :: term().
-type state_change() :: {'commit', state_name(), state()} | {'ok', state_name(), state()}.

-callback init(term()) -> {'ok', state()}.
-callback recover(state()) -> state_change() | {'error',reason()}.
-callback commit(state_name(),state()) -> 'ok'.

-callback challenge(state()) -> {'ok',challenge()}.
-callback resolve(challenge(), state_name(), state()) -> boolean().
-callback conflict(name(), pid(), pid()) -> pid().
-callback terminate(reason(), state_name(), state()) -> {'commit', state_name(), state()} | 'ok'. 

call_init(S) -> (?mod(S)):init(?data(S)).
call_challenge(S) -> (?mod(S)):challenge(?data(S)).
call_info(Info,S) -> (?mod(S)):(?act(S))(Info, ?data(S)).
call_recover(S) -> (?mod(S)):recover(?data(S)).
call_commit(S) -> (?mod(S)):commit(?act(S),?data(S)).
call_resolve(Challenge,S) -> (?mod(S)):resolve(Challenge, ?act(S), ?data(S)).
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

resolve(Pid,Context) ->
    callback(Pid,{resolve,Context}).

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
    case catch call_init(S) of
        {ok,StateData} ->
            ?debugMsg("process alive"),
            proc_lib:init_ack(Starter, {ok, self()}),
            start(S#state{state_data=StateData});
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
            ?debugMsg("need to challenge"),
            MRef = erlang:monitor(process,Pid),
            challenge(MRef,Pid,S)
    end.

recover(S) ->
    ?debugMsg("recovering state"),
    case catch call_recover(S) of
        {ok, StateName, StateData} ->
            leader(S#state{state_name=StateName,state_data=StateData});
        {commit, StateName, StateData} ->
            commit(S#state{state_name=StateName,state_data=StateData});
        Return -> common(Return,S)
    end.

commit(S) ->
    ?debugMsg("commiting state"),
    case catch call_commit(S) of
        ok ->
            leader(S);
        Return -> common(Return,S)
    end.

challenge(MRef,Pid,S) ->
    Challenge = call_challenge(S),
    case call_remote(Pid,resolve,[Challenge]) of
        {ok,true} -> shell(MRef,Pid,S);
        {ok,false} -> follower(MRef,Pid,S)
    end.

%% ===================================================================

leader(S) ->
    ?debugMsg("leader loop"),
    receive
        {?MODULE,From, {callback,{resolve,Challenge}}} = Msg ->
            resolve(Challenge, From, S#state{message=Msg});
        Info ->
            info(Info, S#state{message=Info})
    end.

shell(MRef,Pid,S) ->
    ?debugMsg("shell loop"),
    receive
        {'DOWN',MRef,process,Pid,handover}=Msg ->
            start(S#state{message=Msg});
        Info ->
            terminate({unexpected_message, Info}, S#state{message=Info})
    after
        ?NETWORK_TIMEOUT ->
            terminate({timeout,handover}, S)
    end.

follower(MRef,Pid,S) ->
    ?debugMsg("follower loop"),
    receive
        {'DOWN',MRef,process,Pid,_Reason}=Msg ->
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

resolve(Challenge,From,S) ->
    case catch call_resolve(Challenge,S) of
        true ->
            gen:reply(From,true),
            terminate(handover,S);
        false ->
            gen:reply(From,false),
            leader(S);
        Return ->
            common(Return,S)
    end.

terminate(Reason, S) ->
    ?debugFmt("terminate ~p",[Reason]),
    case catch call_terminate(Reason,S) of
        {commit, StateName, StateData} ->
            ok = call_commit(S#state{state_name=StateName,state_data=StateData}),
            exit(Reason);
        {'EXIT', What} ->
            fatal(What, S),
            exit(What);
        _ ->
            exit(Reason)
    end.

common(Return,S) ->
    ?debugFmt("common ~p",Return),
    case Return of
        {stop, Reason, StateData} -> terminate(Reason, S#state{state_data=StateData});
        {'EXIT', What} -> terminate(What, S);
        _ -> terminate({bad_return_value, Return}, S)
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
    case global:register_name(Name, self(), fun Mod:conflict/3) of
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
