-module(type).

%% API
-export([
         which/1,
         to_atom/1,
         to_list/1,
         to_integer/1,
         % to_tuple/1,
         % to_binary/1,
         % to_boolean/1,
         to_float/1
         % to_pid/1,
         % to_port/1,
         % to_bitstring/1,
         % to_reference/1
        ]).

%% ===================================================================
%% API
%% ===================================================================

%% from most likely
which(Value) when is_boolean(Value)   -> boolean;
which(Value) when is_atom(Value)      -> atom;
which(Value) when is_list(Value)      -> list;
which(Value) when is_integer(Value)   -> integer;
which(Value) when is_tuple(Value)     -> tuple;
which(Value) when is_binary(Value)    -> binary;
which(Value) when is_float(Value)     -> float;
which(Value) when is_pid(Value)       -> pid;
which(Value) when is_function(Value)  -> function;
which(Value) when is_port(Value)      -> port;
which(Value) when is_bitstring(Value) -> bitstring;
which(Value) when is_reference(Value) -> reference.

%% ===================================================================

to_atom(Value) when is_atom(Value)      -> Value;
to_atom(Value) when is_list(Value)      -> list_to_existing_atom(Value);
to_atom(Value) when is_integer(Value)   -> to_atom(integer_to_list(Value));
to_atom(Value) when is_binary(Value)    -> to_atom(binary_to_list(Value));
%% to_atom(Value) when is_float(Value)     -> to_atom(float_to_list(Value));
to_atom(Value) when is_pid(Value)       -> to_atom(pid_to_list(Value));
to_atom(Value) when is_port(Value)      -> to_atom(erlang:port_to_list(Value));
to_atom(Value) when is_bitstring(Value) -> to_atom(bitstring_to_list(Value));
to_atom(Value) when is_port(Value)      -> to_atom(erlang:ref_to_list(Value)).

%% ===================================================================

to_list(Value) when is_list(Value)      -> Value;
to_list(Value) when is_atom(Value)      -> atom_to_list(Value);
to_list(Value) when is_integer(Value)   -> integer_to_list(Value);
to_list(Value) when is_tuple(Value)     -> tuple_to_list(Value);
to_list(Value) when is_binary(Value)    -> binary_to_list(Value);
%% to_list(Value) when is_float(Value)     -> float_to_list(Value);
to_list(Value) when is_pid(Value)       -> pid_to_list(Value);
to_list(Value) when is_port(Value)      -> erlang:port_to_list(Value);
to_list(Value) when is_bitstring(Value) -> bitstring_to_list(Value);
to_list(Value) when is_reference(Value) -> erlang:ref_to_list(Value).

%% ===================================================================

to_integer(Value) when is_integer(Value)   -> Value;
to_integer(Value) when is_list(Value)      -> list_to_integer(Value);
to_integer(Value) when is_atom(Value)      -> to_integer(atom_to_list(Value));
to_integer(Value) when is_binary(Value)    -> to_integer(binary_to_list(Value)).
%% to_integer(Value) when is_float(Value)     -> to_integer(float_to_list(Value));

%% ===================================================================

to_float(Value) when is_list(Value) ->
    case lists:member($., Value) of
        true -> to_float(list_to_float(Value));
        false -> to_float(list_to_integer(Value))
    end;
to_float(Value) when is_atom(Value)     -> to_float(atom_to_list(Value));
to_float(Value) when is_integer(Value)  -> to_float(Value+0.0);
to_float(Value) when is_float(Value)    -> Value.
