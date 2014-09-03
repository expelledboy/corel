%%------------------------------------------------------------------------------

-module(example_object).

% example of a fetter and spec_fsm
-compile({parse_transform, fetter}).
-behaviour(spec_fsm).

%% spec_fsm callbacks
-export([
         state/1,
         event_handler/2,
         state_change/3
        ]).

-export([
         basic/2,
         loyal/2,
         active/2,
         inactive/2,
         disabled/2,
         ignore/2
        ]).

-record(?MODULE, {
           username,
           password,
           balance=0,
           active,
           active_months=0,
           services=[]
          }).

-define(FEE, 10.0).
-define(DISCOUNT, 2.0).
-define(LOYAL_FEE, ?FEE-?DISCOUNT).

%%------------------------------------------------------------------------------
%% spec_fsm callbacks
%%------------------------------------------------------------------------------

% determine the state of the object
state(#?MODULE{active=Active,active_months=Months}) ->
  case Active of
    undefined -> new_account;
    disabled -> disabled;
    false -> inactive;
    true when Months < 24 -> {active,basic};
    true -> {active,loyal}
  end.

%%------------------------------------------------------------------------------

% only state active gets new_day
event_handler({active,AccountType},new_day) ->
  AccountType;
event_handler(_StateHandle,new_day) ->
  ignore;

% new_account and inactive go to the inactive event handler
% otherwise its is handled generically
event_handler(new_account,{paid,_}) ->
  inactive;
event_handler(inactive,{paid,_}) ->
  inactive;
event_handler(_StateHandle,{paid,_}) ->
  active;

% dont want to activate already active account
% and all events should go to the active event handler
event_handler({active,_},activate) ->
  ignore;
event_handler({active,_},_Event) ->
  active;

% only disabled accounts can be activated
event_handler(disabled,activate) ->
  disabled;
event_handler(_StateHandle,activate) ->
  ignore;

% appart from active accounts we dont want anyone else getting this event
event_handler(_StateHandle,new_month) ->
  ignore;

event_handler(StateHandle,_Event) ->
  StateHandle.

%%------------------------------------------------------------------------------

state_change(Object,{active,basic},{active,loyal}) ->
  send_message(Object#?MODULE.username,"Well done! You have been granted a discount."),
  Object;

state_change(Object,new_account,activate) ->
  send_message(Object#?MODULE.username,"Welcome! We hope that you will become a loyal customer, because we are awesome and you get discouts."),
  Object;

state_change(Object,{active,_},inactive) ->
  send_message(Object#?MODULE.username,"Your account has been deactivated."),
  Object#?MODULE{active_months=0};

state_change(Object,_Before,_After) ->
  Object.

%%------------------------------------------------------------------------------
%% event handlers
%%------------------------------------------------------------------------------

basic(#?MODULE{balance=Balance}=Object,new_day) when (Balance-?FEE) < 0 ->
  Object#?MODULE{active=false};
basic(#?MODULE{balance=Balance}=Object,new_day) ->
  Object#?MODULE{balance=Balance-?FEE}.

loyal(#?MODULE{balance=Balance}=Object,new_day) when (Balance-?LOYAL_FEE) < 0 ->
  Object#?MODULE{active=false};
loyal(#?MODULE{balance=Balance}=Object,new_day) ->
  Object#?MODULE{balance=Balance-?LOYAL_FEE}.

active(Object,{paid,Value}) ->
  Object#?MODULE{balance=Object#?MODULE.balance+Value};
active(Object,deactivate) ->
  Object#?MODULE{active=disabled};
active(Object,new_month) ->
  ActiveMonths = Object#?MODULE.active_months+1,
  State1 = Object#?MODULE{active_months=ActiveMonths},
  case ActiveMonths rem 6 of
    0 ->
      send_message(Object#?MODULE.username,"Thanks for supporting us. We have given you a month free!"),
      State1#?MODULE{active=true,balance=Object#?MODULE.balance+?FEE};
    _ ->
      State1
  end.

%%------------------------------------------------------------------------------

inactive(Object,{paid,Value}) ->
  NewBalance = Object#?MODULE.balance+Value,
  BalanceFee = NewBalance-?FEE,
  case BalanceFee > 0 of
    true ->
      Object#?MODULE{active=true,balance=BalanceFee};
    false ->
      Object#?MODULE{balance=NewBalance}
  end.

%%------------------------------------------------------------------------------

disabled(#?MODULE{balance=Balance},activate) when (Balance-?FEE) < 0 ->
  throw({insufficient_funds,Balance});
disabled(Object,activate) ->
  Object#?MODULE{active=true,balance=Object#?MODULE.balance-?FEE}.

%%------------------------------------------------------------------------------

ignore(Object,_Event) ->
  Object.

%%------------------------------------------------------------------------------
%% dummy functions
%%------------------------------------------------------------------------------

send_message(Username,Message) ->
  io:format("~p: ~s\n", [Username,Message]).

%%------------------------------------------------------------------------------
