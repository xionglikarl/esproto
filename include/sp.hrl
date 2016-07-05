-ifndef(SP_H).
-define(SP_H, true).

-define(SPROTO_SESSION_ETS, sproto_session_ets). 

-record(type, {
		name,				% string
		fields_or_types		% [#field{} | #type{}, ..]
	}).

-record(field, {
		name,		% string
		tag, 		% integer
		buildin, 	% integer::Enum("string"=1 | "integer"=2 | "boolean"=3)
		datatype,	% string
		isarray, 	% boolean
		key			% integer, If key exists, isarray must be true, and it's a map
}).

-record(protocol, {
		name,			% string
		tag,			% integer
		subprotolist	% subproto::{request | response, string() | [#field{}, ..]}
}).

-define(BUILDIN_STRUCT, 0).
-define(BUILDIN_STRING, 1).
-define(BUILDIN_INT, 2).
-define(BUILDIN_BOOL, 3).

-endif.

