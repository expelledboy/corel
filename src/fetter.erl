%% @copyright Anthony Jackson <expelledboy> All Rights Reserved
%% This file is subject to the terms and conditions defined in
%% file 'LICENSE.txt', which is part of this source code package.

-module(fetter).

%% parse transform 
-export([ parse_transform/2 ]).
%% -compile({parse_transform, fetter}).

%% fetter proxy commands
-export([ new_record_with_proplist/2, set_field/4, set_field_from_proplist/3 ]).

%% utils
-export([ to_xml/1, to_xml/2 ]).

%% ===================================================================
%% parse transform 
%% ===================================================================

parse_transform(Forms, _Options) ->
    Definition = get_record_definition(Forms),
    Record = simplify_record(Definition),

    DefineModule = fun({attribute,_,module,_}) -> true; (_) -> false end,
    WithExports = insert_after(DefineModule,[create_export()],Forms),

    Functions = [ create_new(Record),create_new_proplist(Record),create_fields(Record),
                  create_fget(Record),create_fset(Record),create_fset_proplist(Record) ],
    DefineRecord = fun({attribute,_,record,{_,_}}) -> true; (_) -> false end,
    _WithFunctions = insert_after(DefineRecord,Functions,WithExports).

get_record_definition(Forms) ->
    [Record] = [Rec || {attribute,_,record,_} = Rec <- Forms],
    Record.

%% reduce record definition to {record_name,[fields()]}
simplify_record({attribute,_,record, {Name, Fields}}) ->
    GetFieldName = fun
                       ({record_field,_,{atom,_,FieldName},_Default}) -> FieldName;
                       ({record_field,_,{atom,_,FieldName}}) -> FieldName
                   end,
    {Name, [GetFieldName(Field) || Field <- Fields]}.

%% ===================================================================
%% abstract code
%% ===================================================================

%% -export([new/0,new/1,fields/0,fget/2,fset/2,fset/3]).
create_export() ->
    {attribute, 0, export, [ {new,0}, {new,1}, {fields,0},
                             {fget,2}, {fset,2}, {fset,3} ]}.

%% new() -> #record_name{}.
create_new({RecordName,_}) ->
    Clause = {clause,0,[],[],[{record,0,RecordName,[]}]},
    {function,0,new,0,[Clause]}.


%% new(Fields) when is_list(Fields) ->
%%   fetter:new_record_with_proplist(record_name,Fields).
create_new_proplist({RecordName,_}) ->
    Clause = {clause,0,
              [{var,0,'Fields'}],
              [[{call,0,{atom,0,is_list},[{var,0,'Fields'}]}]],
              [{call,0,
                {remote,0,
                 {atom,0,fetter},
                 {atom,0,new_record_with_proplist}},
                [{atom,0,RecordName},{var,0,'Fields'}]}]},
    {function,0,new,1, [Clause]}.

%% fields() ->
%%   [field_name1, field_name2, field_name3].
create_fields({_RecordName,Fields}) ->
    CreateListItem = fun(FieldName,List) ->
                             {cons,0,{atom,0,FieldName},List}
                     end,
    List = case lists:foldl(CreateListItem, {nil,0}, lists:reverse(Fields)) of
               {nil,0} -> [];
               Items -> [Items]
           end,
    Clause = {clause,0,[],[],List},
    {function, 0, fields, 0, [Clause]}.

%% fget(Record,field_name) when is_record(Record, record_name) ->
%%   Record#record_name.field_name.
create_fget({RecordName,Fields}=Record) ->
    CreateClause = fun(FieldName) ->
                           {clause, 0,
                            [{var, 0, 'Record'},{atom, 0, FieldName}],
                            [[{call, 0, {atom, 0, is_record}, [{var, 0, 'Record'}, {atom, 0, RecordName}]}]],
                            [{record_field, 0, {var, 0, 'Record'}, RecordName, {atom, 0, FieldName}}]}
                   end,
    GetFields = lists:foldl(fun(Field, Acc) -> [CreateClause(Field) | Acc] end, [], Fields),
    Clauses = [ clause_fget_no_field(Record), clause_fget_list(Record) | GetFields],
    {function, 0, fget, 2, lists:reverse(Clauses)}.

%% fget(Record,Fields) when is_record(Record, record_name) and is_list(Fields) ->
%%   [ fget(Record,Field) || Field <- Fields ].
clause_fget_list({RecordName,_Fields}) ->
    {clause,0,
     [{var,0,'Record'},{var,0,'Fields'}],
     [[{op,0,'and',
        {call,0, {atom,0,is_record}, [{var,0,'Record'},{atom,0,RecordName}]},
        {call,0,{atom,0,is_list},[{var,0,'Fields'}]}
       }]],
     [{lc,0,
       {call,0,{atom,0,fget},[{var,0,'Record'},{var,0,'Field'}]},
       [{generate,0,{var,0,'Field'},{var,0,'Fields'}}]
      }]}.

%% fget(Record,_Field) when is_record(Record, record_name) ->
%%   throw({not_present,{record_name,_Field}}).
clause_fget_no_field({RecordName,_Fields}) ->
    {clause,0,
     [{var,0,'Record'},{var,0,'_Field'}],
     [[{call,0,
        {atom,0,is_record},
        [{var,0,'Record'},{atom,0,RecordName}]
       }]],
     [{call,0,
       {atom,0,throw},
       [{tuple,0,
         [{atom,0,not_present},
          {cons,0,
           {tuple,0, [{atom,0,record},{atom,0,RecordName}]},
           {cons,0, {tuple,0,[{atom,0,field},{var,0,'_Field'}]},
            {nil,0}}
          }]}]
      }]}.

%% fset(Record, field_name, Value) when is_record(Record, record_name) ->
%%   fetter:set_field(Record,field_name,#record_name.field_name,Value).
create_fset({RecordName,Fields}) ->
    CreateClause = fun(FieldName) ->
                           {clause,0,
                            [{var,0,'Record'},{atom,0,FieldName},{var,0,'Value'}],
                            [[{call,0,
                               {atom,0,is_record},
                               [{var,0,'Record'},{atom,0,RecordName}]}]],
                            [{call,0,
                              {remote,0,{atom,0,fetter},{atom,0,set_field}},
                              [{var,0,'Record'},
                               {atom,0,field_name},
                               {record_index,0,RecordName,{atom,0,FieldName}},
                               {var,0,'Value'}]}]}
                   end,
    List = lists:foldl(fun(Field, Acc) -> [CreateClause(Field) | Acc] end, [], Fields),
    {function, 0, fset, 3, lists:reverse(List)}.

%% fset(Record, Fields) when is_record(Record, record_name) and is_list(Fields) ->
%%   fetter:set_field_from_proplist(record_name,Record,Fields).
create_fset_proplist({RecordName,_Fields}) ->
    Clause = {clause,0,
              [{var,0,'Record'},{var,0,'Fields'}],
              [[{op,0,'and',
                 {call,0,
                  {atom,0,is_record},
                  [{var,0,'Record'},{atom,0,RecordName}]},
                 {call,0,{atom,0,is_list},[{var,0,'Fields'}]}}]],
              [{call,0,
                {remote,0,
                 {atom,0,fetter},
                 {atom,0,set_field_from_proplist}},
                [{atom,0,RecordName},
                 {var,0,'Record'},
                 {var,0,'Fields'}]}]},
    {function, 0, fset, 2, [Clause]}.

%% ===================================================================
%% fetter proxy commands
%% ===================================================================

new_record_with_proplist(RecordName,Fields) ->
    Record = RecordName:new(),
    lists:foldl(fun({Field,Value},Acc) -> RecordName:fset(Acc, Field, Value) end,Record,Fields).

set_field(Record,_FieldName,FieldIndex,Value) ->
    % TODO validation
    setelement(FieldIndex,Record,Value).

set_field_from_proplist(RecordName,Record,Fields) ->
    lists:foldl(fun({Field,Value},Acc) -> RecordName:fset(Acc, Field, Value) end,Record,Fields).

%% ===================================================================
%% Utils
%% ===================================================================

to_xml(Record) ->
    RecordName = element(1,Record),
    Fields = [ Field || Field <- RecordName:fields(), RecordName:fget(Record,Field) =/= undefined ],
    to_xml(Record,[{fields,Fields}]).

to_xml(Record,Options) when is_tuple(Record) and is_list(Options) ->
    Fields = opts:fget(Options,fields),
    Format = opts:fget(Options,format,simple),
    to_xml(Format,Record,Fields).

to_xml(simple,Record,Fields) when is_list(Fields) ->
    RecordName = element(1,Record),
    Elements = [ {Field,[type:to_list(RecordName:fget(Record,Field))]} || Field <- Fields ],
    Struct = [{RecordName,Elements}],
    lists:flatten(xmerl:export_simple_content(Struct, xmerl_xml)).

%% from_xml(XML)
%%   RecordName = element(1,Record),
%%   Fields = [ Field || Field <- RecordName:fields(), RecordName:fget(Record,Field) =/= undefined ],
%%   to_xml(Record,[{fields,Fields}]).

%% ===================================================================
%% internal functions
%% ===================================================================

%% insert_before(Predicate,Elements,Forms) ->
%%   insert_when(true,Predicate,Elements,[],Forms).

%% @private
insert_after(Predicate,Elements,Forms) ->
    insert_when(false,Predicate,Elements,[],Forms).

%% @private
insert_when(Before,Pred,Insert,Prefix,[Elem|Rest]=Postfix) ->
    case Pred(Elem) of
        true when Before -> lists:reverse(Prefix)++Insert++Postfix;
        true when not Before -> lists:reverse(Prefix)++[Elem|Insert]++Rest;
        false -> insert_when(Before,Pred,Insert,[Elem|Prefix],Rest)
    end.

%% f(Code), f(Tokens),
%% Code = "fset(Record, field_name, Value) when is_record(Record, record_name) -> fetter:set_field(record_name,field_name,#record_name.field_name,Value).\n".
%% {done,{ok,Tokens,_}, _} = erl_scan:tokens([],Code,0),
%% io:format("~p~n", [erl_parse:parse_form(Tokens)]).
