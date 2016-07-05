%%%========================================================================
%%% File: sproto.erl
%%% Author: xionglikarl <xiongli.karl@gmail.com>
%%%
%%%-- LICENSE -------------------------------------------------------------
%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2016 xionglikarl
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining
%%% a copy of this software and associated documentation files (the
%%% "Software"), to deal in the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software,
%%% and to permit persons to whom the Software is furnished to do so,
%%% subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%========================================================================
-module(sproto).
-include("sp.hrl").
-include_lib("stdlib/include/assert.hrl").
-export([encode/2, decode2/2, decode/2]).
-export([pack/1, unpack/1]).
-export([rpc_init/0, proto_encode/2, proto_encode/3, proto_encode/4, proto_encode/5,
	proto_decode/1, proto_decode/2]).


%% rpc api
rpc_init() ->
	case ets:info(?SPROTO_SESSION_ETS) of
		undefined ->
			ets:new(?SPROTO_SESSION_ETS, [named_table, set, public, {keypos,2}, {read_concurrency, true}, {write_concurrency,true}]);
		_ -> skip
	end.

%% proto encode
proto_encode(TypeName, DataMap) ->
	proto_encode(TypeName, DataMap, undefined, undefined).
proto_encode(TypeName, DataMap, Session) ->
	proto_encode(TypeName, DataMap, Session, undefined).
proto_encode(TypeName, DataMap, Session, UserData) ->
	proto_encode("BasePackage", TypeName, DataMap, Session, UserData).
proto_encode(BasePackage, TypeName, DataMap, Session, UserData) ->
	#protocol{tag = Tag, subprotolist = _Subprotolist} = spb:get_protocol(TypeName),
	HeaderMap1 = 
	case UserData of 
		undefined -> #{"type" => Tag, "session" => Session};
		_ -> #{"type" => Tag, "session" => Session, "ud" => UserData}
	end,
	HeaderMap = 
	case Session of 
		undefined -> maps:remove("session", HeaderMap1);
		_ -> 
			{RespTypeSchema, _} = get_subproto_type_schema(Tag, "response"),
			ets:insert(?SPROTO_SESSION_ETS, {session, Session, RespTypeSchema}),
			HeaderMap1
	end,
	HeadBin = encode(BasePackage, HeaderMap),
	{TypeSchema, _} = get_subproto_type_schema(Tag, "request"),
	DataBin = encode(TypeSchema, DataMap),
	Bin = <<HeadBin/binary, DataBin/binary>>,
	Bin2 = pack(Bin),
	Bin2.

%% proto decode
proto_decode(BinaryMessage) ->
	proto_decode("BasePackage", BinaryMessage).
proto_decode(BasePackage, BinaryMessage) ->
	Bin = unpack(BinaryMessage),
	{HeadMap, _Len, RestBin} = decode2(BasePackage, Bin),
	UserData = maps:get("ud", HeadMap, undefined),
	case maps:get("type", HeadMap, undefined) of
		undefined -> 
			% response
			case maps:get("session", HeadMap, undefined) of
				undefined -> throw("Unknown session");
				Session -> 
					[SessionR] = ets:lookup(?SPROTO_SESSION_ETS, Session),
					{session, _, RespTypeSchema} = SessionR,
					ets:delete(?SPROTO_SESSION_ETS, Session),
					case RespTypeSchema of 
						undefined -> 
							{"RESPONSE", Session, undefined, UserData};
						_ -> 
							{DataMap, _, _} = decode2(RespTypeSchema, RestBin),
							{"RESPONSE", Session, DataMap, UserData}
					end
			end;
		Type -> 
			% request
			{TypeSchema, Name} = get_subproto_type_schema(Type, "request"),
			{DataMap, _, _} = decode2(TypeSchema, RestBin),
			case maps:get("session", HeadMap, undefined) of
				undefined ->
					{"REQUEST", Name, DataMap, undefined, UserData};
				Session -> 
					{"REQUEST", Name, DataMap, gen_response(Type, Session, BasePackage), UserData}
			end
	end.

gen_response(ProtoTag, Session, BasePackage) ->
	{TypeSchema, _} = get_subproto_type_schema(ProtoTag, "response"),
	Fun = fun(DataMap, UserData) ->
		HeaderMap = 
		case UserData of 
			undefined -> #{"session" => Session};
			_ -> #{"session" => Session, "ud" => UserData}
		end,
		HeadBin = encode(BasePackage, HeaderMap),
		case TypeSchema of 
			undefined -> 
				pack(HeadBin);
			_ -> 
				DataBin = encode(TypeSchema, DataMap),
				Bin = <<HeadBin/binary, DataBin/binary>>,
				pack(Bin)
		end
	end,
	Fun.

%% Type::request | response
get_subproto_type_schema(ProtoTag, Type) ->
	#protocol{name = Name, subprotolist = Subprotolist} = spb:get_protocol(ProtoTag),
	try
		{_, Fields} = _RequestSchema = lists:keyfind(Type, 1, Subprotolist), 
		TypeSchema = 
		case Fields =:= [] orelse is_tuple(hd(Fields)) of
			true -> 	% field list
				#type{fields_or_types = Fields};
			false -> 	% type string
				get_schema(Fields)
		end,	
		{TypeSchema, Name}
	catch
		_Err:_Reason -> {undefined, undefined}
	end.


gen_0bin(Acc, 0) ->
	Acc;
gen_0bin(Acc, Num) ->
	gen_0bin(<<Acc/binary, 0>>, Num - 1).
pad_8byte_multiple(Bin) ->
	Len = byte_size(Bin),
	Rem = Len rem 8,
	case Rem == 0 of 
		true -> Bin;
		false -> 
			Pad = 8 - Rem,
			PadBin = gen_0bin(<<>>, Pad),
			<<Bin/binary, PadBin/binary>>
	end.

pack_8byte(<<>>) ->
	{false, <<>>};
pack_8byte(Bin) ->
	List = binary_to_list(Bin),
	Fun = fun(Ele, {BoolList, Bin2}) ->
	 	case Ele == 0 of 
	 		true -> 
	 			{[false|BoolList], Bin2};
	 		false -> 
	 			{[true|BoolList], <<Bin2/binary, Ele:8>>}
	 	end
	end,
	{BoolList, Bin3} = lists:foldl(Fun, {[], <<>>}, List),
	IsAllNotZero = lists:filter(fun(X) -> X =:= false end, BoolList) == [],
	Fun2 = fun(Bool) -> 
		case Bool of 
			true -> integer_to_list(1);
			false -> integer_to_list(0)
		end
	end,
	BitByte = list_to_integer( lists:append([Fun2(X) || X <- BoolList]), 2), 
	{IsAllNotZero, <<BitByte:8, Bin3/binary>>}.

%% 0-pack
pack(Bin) ->
	pack2(Bin, 0, <<>>).
pack2(<<>>, _Num, AccBin) ->
	AccBin;
pack2(Bin, AccNotZeroGroupNum, AccBin) ->
	PadBin = pad_8byte_multiple(Bin),
	Num = AccNotZeroGroupNum * 8,
	<<DivBin:Num/binary, Bin1:8/binary, Bin2/binary>> = PadBin,
	{IsAllNotZero, Bin3} = pack_8byte(Bin1),
	case IsAllNotZero of 
		true -> 
			pack2(Bin, AccNotZeroGroupNum + 1, AccBin);
		false -> 
			case AccNotZeroGroupNum > 0 of
				true -> 
					Head = <<16#FF, (AccNotZeroGroupNum-1)>>,
					Bin4 = <<Head/binary, DivBin/binary>>,
					pack2(<<Bin1/binary, Bin2/binary>>, 0, <<AccBin/binary, Bin4/binary>>);
				false -> 
					pack2(Bin2, AccNotZeroGroupNum, <<AccBin/binary, Bin3/binary>>)
			end
	end.

bit_to_list(_Bin, 0, List) ->
	List;
bit_to_list(Bin, Num, List) ->
	Num2 = Num - 1,
	<<A:1, B:Num2>> = Bin,
	bit_to_list(<<B:Num2>>, Num - 1, [A|List]).
unpack_8byte(Bin1, Bin2) ->
	List = bit_to_list(Bin1, 8, []),
	Fun = fun(Ele, {AccBin, RestBin}) ->
	 	case Ele of
	 		1 -> 
	 			<<Bin3:1/binary, Bin4/binary>> = RestBin,
	 			{<<AccBin/binary, Bin3/binary>>, Bin4};
	 		0 -> 
	 			{<<AccBin/binary, 0>>, RestBin}
	 	end
	end,
	{AccBin, RestBin} = lists:foldl(Fun, {<<>>, Bin2}, List),
	{AccBin, RestBin}.

%% 0-pack unpack
unpack(Bin) ->
	unpack(Bin, <<>>).
unpack(<<>>, AccBin) ->
	AccBin;
unpack(Bin, AccBin) ->
	<<Bin1:1/binary, Bin2/binary>> = Bin,
	case Bin1 of 
		<<16#FF:8>> -> 
			<<Num:8, Bin4/binary>> = Bin2,
			NotZeroGroupNum = Num + 1,
			ByteNum = NotZeroGroupNum * 8,
			<<Bin5:ByteNum/binary, Bin6/binary>> = Bin4,
			unpack(Bin6, <<AccBin/binary, Bin5/binary>>);
		_ -> 
			{Bin3, RestBin} = unpack_8byte(Bin1, Bin2),
			unpack(RestBin, <<AccBin/binary, Bin3/binary>>)
	end.


%% encodes a erlang map by a type schema, and generates a binary message,
%% call the encode/2 first case must ensure your spb.beam file is loaded
encode(TypeName, Map) when is_list(TypeName)->
	Schema = get_schema(TypeName),
	encode(Schema, Map);
encode(Schema, Map) when is_tuple(Schema)->
	#type{fields_or_types = List} = Schema,
	Fields = lists:filter(fun(X) -> is_record(X, field) end, List),
	SortedFields = lists:sort(fun(#field{tag=Tag1}, #field{tag=Tag2}) -> Tag1 < Tag2 end, Fields),
	Fun = fun(#field{name=Name, tag=Tag}=Field, {FieldNumber, LastTag, FieldPartAcc, DataPartAcc}) -> 
			case maps:is_key(Name, Map) of 
				true -> 
					TagCheck = Tag - LastTag - 1,
					{SkipFieldPart, IsSkipField} = 
					case TagCheck > 0 of
						true -> 	% skip tag
							Val = (TagCheck - 1) * 2 + 1,
							{<<Val:16/little>>, 1};
						false -> {<<>>, 0}
					end,
					{FieldPart, DataPart} = encode_field(Field, maps:get(Name, Map)),
					{FieldNumber + IsSkipField + 1, Tag, <<FieldPartAcc/binary, SkipFieldPart/binary, FieldPart/binary>>, <<DataPartAcc/binary, DataPart/binary>>};
				false -> 
					{FieldNumber, LastTag, FieldPartAcc, DataPartAcc}
			end
	end,
	{FieldNumber, _LastTag, FieldPart, DataPart} = lists:foldl(Fun, {0, -1, <<>>, <<>>}, SortedFields),
	Bin = <<FieldNumber:16/little, FieldPart/binary, DataPart/binary>>,
	Bin.


is_tuple_list([]) ->
	false;
is_tuple_list(List) when is_list(List)->
	is_tuple(hd(List));
is_tuple_list(_) ->
	false.

list_to_map(List) ->
	case is_tuple_list(List) of 
		true -> 
			Fun = fun(Item) ->
			 	case is_list(Item) of 
			 		true -> list_to_map(Item);
			 		false -> Item
			 	end
			end,
			List2 = [{Name, Fun(Val)} || {Name, Val} <- List],
			maps:from_list(List2);
		false -> 
			case List =/= [] andalso is_tuple_list(hd(List)) of 
				true -> [list_to_map(Sub) || Sub <- List];
				false -> List
			end
	end.

%% decodes a binary string generated by sproto:encode/2 with type
%% return map data
decode2(TypeName, BinaryMessage) when is_list(TypeName)->
	Schema = get_schema(TypeName),
	decode2(Schema, BinaryMessage);
decode2(Schema, BinaryMessage) ->
	{Len, List, RestBin} = decode(Schema, BinaryMessage),
	Map = 
	case List == [] of 
		true -> maps:from_list(List);
		false -> list_to_map(List)
	end,
	{Map, Len, RestBin}.

%% decodes a binary string generated by sproto:encode/2 with type
decode(TypeName, BinaryMessage) when is_list(TypeName)->
	Schema = get_schema(TypeName),
	decode(Schema, BinaryMessage);
decode(Schema, BinaryMessage) ->
	<<FieldNumber:16/little, Bin1/binary>> = BinaryMessage,
	?assert(FieldNumber >= 0),
	% decode field part
	Fun = fun(_Index, {LastTag, RestBin, ValueList, TagList}) ->
			<<FieldValue:16/little, Bin2/binary>> = RestBin,
			if
				FieldValue band 1 =:= 1 -> 	% odd
					SkipVal = (FieldValue - 1)/2 + 1,
					Tag = LastTag + SkipVal,
					{Tag, Bin2, ValueList, TagList};
				FieldValue =:= 0 -> 
					Tag = LastTag + 1,
					{Tag, Bin2, ValueList, [Tag|TagList]};
				true -> 					% even
					Tag = LastTag + 1,
					FieldValue2 = FieldValue div 2 - 1,
					{FieldName, FieldValue3} = get_field_part_kv(Tag, Schema, FieldValue2),
					{Tag, Bin2, [{FieldName, FieldValue3}|ValueList], TagList}	
	 		end
	end,
	{_LastTag, Bin2, ValueList1, TagList} = lists:foldl(Fun, {-1, Bin1, [], []}, lists:seq(1, FieldNumber)),
	% decode data part
	Fun2 = fun(Tag, {AccByteLen, ValueList, RestBin}) ->
	 		{Len, NameValuePair, Bin3} = decode_field(Tag, Schema, RestBin),
	 		{AccByteLen + Len, [NameValuePair|ValueList], Bin3}
	end,
	{AccByteLen, ValueList2, Bin3} = lists:foldl(Fun2, {0, [], Bin2}, lists:reverse(TagList)),
	ByteLen = AccByteLen + 2 + FieldNumber*2,
	{ByteLen, lists:reverse(ValueList1 ++ ValueList2), Bin3}.

get_field_part_kv(Tag, Schema, FieldValue) ->
	#field{name=Name, buildin=BuildinType} = get_field(Tag, Schema),
	case BuildinType =:= ?BUILDIN_BOOL of 
		true ->
			Value = 
			case FieldValue =:= 1 of 
				true -> true;
				false -> false
			end,
			{Name, Value};
		false -> {Name, FieldValue}
	end.

decode_field(Tag, Schema, Bin) ->
	#field{buildin=BuildinType, isarray=Isarray} = Field = get_field(Tag, Schema),
	case Isarray of 
		true -> 
			decode_array(Bin, Field);
		false ->
			case BuildinType of 
				?BUILDIN_STRING -> 
					decode_string(Bin, Field);
				?BUILDIN_INT -> 
					decode_integer(Bin, Field);
				?BUILDIN_STRUCT -> 
					decode_struct(Bin, Field)
			end
	end.

decode_array(Bin, #field{name=Name, buildin=BuildinType, datatype=Datatype}) ->
	{Len, Bin2} = unpack_array_bin(Bin),
	{List, Bin4} = 
	case BuildinType of 
		?BUILDIN_STRING -> 
			decode_array2(fun unpack_string/1, Bin2, Len);
		?BUILDIN_INT -> 
			<<Size:8, Bin3/binary>> = Bin2,
			case Size of
				4 -> decode_array2(fun unpack_int32/1, Bin3, Len - 1);
				8 -> decode_array2(fun unpack_int64/1, Bin3, Len - 1)
			end;
		?BUILDIN_BOOL -> 
			decode_array2(fun unpack_bool/1, Bin2, Len);
		?BUILDIN_STRUCT -> 
			decode_array2(fun unpack_struct/2, Bin2, Len, Datatype)
	end,
	{4 + Len, {Name, List}, Bin4}.
decode_array2(UnpackFun, Bin, Len) ->
	decode_array2(UnpackFun, Bin, Len, "").
decode_array2(UnpackFun, Bin, Len, Datatype) ->
	Fun = fun(_Index, {AccLen, List, RestBin}) ->
			case AccLen >= Len of 
				true -> {AccLen, List, RestBin};
				false ->
				 	{Len2, Val, Bin2} = 
				 	case Datatype of 
				 		"" -> UnpackFun(RestBin);
				 		_ -> UnpackFun(RestBin, Datatype)
				 	end,
				 	{AccLen + Len2, [Val|List], Bin2}
			end
	end,
	{_, List, Bin2} = lists:foldl(Fun, {0, [], Bin}, lists:seq(1, Len)),
	{lists:reverse(List), Bin2}.

unpack_array_bin(Bin) ->
	<<Len:32/little, Bin2/binary>> = Bin,
	{Len, Bin2}.

unpack_int32(Bin) ->
	<<Num:32/little-signed, Bin2/binary>> = Bin,
	{4, Num, Bin2}.
unpack_int64(Bin) ->
	<<Num:64/little-signed, Bin2/binary>> = Bin,
	{8, Num, Bin2}.
	
unpack_bool(Bin) ->
	<<Bool:8, Bin2/binary>> = Bin,
	case Bool == 1 of 
		true -> {1, true, Bin2};
		false -> {1, false, Bin2}
	end.

decode_struct(Bin, #field{name=Name, datatype=Datatype}) ->
	{Len, List, Bin2} = unpack_struct(Bin, Datatype),
	{Len + 4, {Name, List}, Bin2}.
unpack_struct(Bin, Datatype) ->
	<<_Len1:32/little, Bin2/binary>> = Bin,
	{Len2, List, Bin3} = decode(Datatype, Bin2),
	{Len2 + 4, List, Bin3}.

decode_integer(Bin, #field{name=Name}) ->
	{Len, Num, Bin2} = unpack_integer(Bin),
	{Len, {Name, Num}, Bin2}.
unpack_integer(Bin) ->
	<<Len:32/little, Bin2/binary>> = Bin,
	<<Num:Len/little-signed-integer-unit:8, Rest/binary>> = Bin2,
	{Len + 4, Num, Rest}.

decode_string(Bin, #field{name=Name}) ->
	{Len, Str, Bin2} = unpack_string(Bin),
	{Len, {Name, Str}, Bin2}.
unpack_string(Bin) ->
	<<Len:32/little, Bin2/binary>> = Bin,
	<<Str:Len/little-binary-unit:8, Rest/binary>> = Bin2,
	{Len + 4, binary_to_list(Str), Rest}.

get_field(Tag, TypeSchema) when is_tuple(TypeSchema)->
	#type{fields_or_types = List} = TypeSchema,
	Field = lists:keyfind(Tag, #field.tag, List),
	Field;
get_field(Tag, TypeName) when is_list(TypeName)->
	#type{fields_or_types = List} = get_schema(TypeName),
	Field = lists:keyfind(Tag, #field.tag, List),
	Field.

get_schema(TypeName) ->
	FullTypeList = spb:get_fully_type(TypeName),
	get_schema(FullTypeList, root).
get_schema([], Schema) ->
	Schema;
get_schema([H|T], root) ->
	Schema = spb:get_type(H),
	get_schema(T, Schema);
get_schema([H|T], Schema) ->
	#type{fields_or_types = List} = Schema,
	TypeSchema = lists:keyfind(H, #type.name, List),
	?assert(is_record(TypeSchema, type)),
	get_schema(T, TypeSchema).

encode_field(#field{buildin=BuildinType, datatype=Datatype, isarray=Isarray}=Field, FieldData) ->
	case Isarray of 
		true -> 
			encode_array(Field, FieldData);
		false ->
			case BuildinType of 
				?BUILDIN_STRING -> 
					encode_string(FieldData);
				?BUILDIN_INT -> 
					encode_integer(FieldData);
				?BUILDIN_BOOL -> 
					encode_bool(FieldData);
				?BUILDIN_STRUCT -> 
					encode_struct(Datatype, FieldData)
			end
	end.

encode_array(#field{buildin=BuildinType, datatype=Datatype}, FieldData) ->
	case BuildinType of 
		?BUILDIN_STRING -> 
			encode_array2(fun pack_string/1, FieldData);
		?BUILDIN_INT -> 
			List = [pack_integer(Item) || Item <- FieldData],
			{Size, BinList} = 
			case lists:filter(fun({X, _}) -> X=:=8 end, List) =:= [] of 
				true -> {4, [Bin1 || {_, Bin1} <- List]};
				false -> 
					{8,
					[begin
						case Size1 of
							4 ->
								<<Num1:32/little-signed>> = Bin1, 
								<<Num1:64/little-signed>>;
							8 -> Bin1
						end
					end|| {Size1, Bin1} <- List]}
			end,
			Bin = <<Size:8/little-signed, (list_to_binary(BinList))/binary>>,
			pack_array_bin(Bin);
		?BUILDIN_BOOL -> 
			encode_array2(fun pack_bool/1, FieldData);
		?BUILDIN_STRUCT -> 
			encode_array2(fun pack_struct/2, {Datatype, FieldData})
	end.
encode_array2(PackFun, {Datatype, FieldData}) ->
	Bin = list_to_binary([PackFun(Datatype, Item) || Item <- FieldData]),
	pack_array_bin(Bin);
encode_array2(PackFun, FieldData) ->
	Bin = list_to_binary([PackFun(Item) || Item <- FieldData]),
	pack_array_bin(Bin).
pack_array_bin(Bin) ->
	Len = byte_size(Bin),
	{<<0:16>>, <<Len:32/little, Bin/binary>>}.

pack_bool(true) ->
	<<1:8>>;
pack_bool(_) ->
	<<0:8>>.

pack_integer(Integer) ->
	Range1 = math:pow(2, 31),
	Range2 = math:pow(2, 63),
	if 
		Integer >= -Range1 andalso Integer =< Range1 - 1 ->		% 4 bytes
			{4, <<Integer:32/little-signed>>};
		Integer >= -Range2 andalso Integer =< Range2 - 1 ->		% 8 bytes
			{8, <<Integer:64/little-signed>>};
		true -> throw("invalid number")
	end.

encode_struct(Datatype, FieldData) ->
	Bin = pack_struct(Datatype, FieldData),
	{<<0:16>>, <<Bin/binary>>}.
pack_struct(Datatype, FieldData) ->
	Bin = encode(Datatype, FieldData),
	Len = byte_size(Bin),
	<<Len:32/little, Bin/binary>>.

encode_string(FieldData) ->
	Bin = pack_string(FieldData),
	{<<0:16>>, <<Bin/binary>>}.
pack_string(FieldData) ->
	Bin = list_to_binary(FieldData),
	Len = byte_size(Bin),
	<<Len:32/little, Bin/binary>>.

encode_bool(true) ->
	encode_integer(1);
encode_bool(_) ->
	encode_integer(0).

encode_integer(Integer) ->
	Range1 = math:pow(2, 31),
	Range2 = math:pow(2, 63),
	if 
		Integer >= 0 andalso Integer < 16#7fff ->	% why is 16#7fff, depends on sproto write protocol
			{<<((Integer+1)*2):16/little>>, <<>>};
		Integer >= -Range1 andalso Integer =< Range1 - 1 ->		% 4 bytes
			{<<0:16>>, <<4:32/little, Integer:32/little-signed>>};
		Integer >= -Range2 andalso Integer =< Range2 - 1 ->		% 8 bytes
			{<<0:16>>, <<8:32/little, Integer:64/little-signed>>};
		true -> throw("invalid number")
	end.