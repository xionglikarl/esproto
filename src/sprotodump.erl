%%%========================================================================
%%% File: sprotodump.erl
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
-module(sprotodump).
-include("sp.hrl").
-export([file/1, dir/1, dir/2, dir/3]).

%% todo.. add sproto check, like type redefined , field tag is continue and ascend etc.

%% parse a sproto file to term
file(File) ->
	sprotoparse:file(File).

%% dump the *.sproto files in the directory Dir and sub-directories to a beam file,
%% the beam module export 2 functions, get_type/1 and get_protocol/1, the parameter is type name or protocol name
dir(Dir) ->
	dir(Dir, "./", false).
dir(Dir, OutDir) ->
	dir(Dir, OutDir, false).
dir(Dir, OutDir, WriteErl) ->
	SprotoListofList = filelib:fold_files(Dir, ".sproto", true, 
		fun(File, Acc) -> 
			io:format("parse sproto file:~s~n", [File]),
			Sproto = sprotoparse:file(File),
			[Sproto|Acc] 
		end, []),
	SprotoList = lists:append(SprotoListofList),
	Fun = fun(Ele, {TypeDataAcc, ProtoDataAcc}) -> 
			case Ele of 
				{type, Name, _} -> 
					Str = "get_type(\"" ++ Name ++ "\")->\n" ++ io_lib:format("~p", [Ele]) ++ ";\n",
					{[Str|TypeDataAcc], ProtoDataAcc};
				{protocol, Name, Tag, _SubList} -> 
					Str1 = "get_protocol(\"" ++ Name ++ "\")->\n" ++ io_lib:format("~p", [Ele]) ++ ";\n",
					Str2 = "get_protocol(" ++ integer_to_list(Tag) ++ ")->\n" ++ io_lib:format("~p", [Ele]) ++ ";\n",
					{TypeDataAcc, [Str1, Str2|ProtoDataAcc]}
			end
	end,
	{TypeData, ProtoData} = lists:foldl(Fun, {[], []}, SprotoList),
	TypeData2 = TypeData ++ ["get_type(_)->undefined.\n"],
	ProtoData2 = ProtoData ++ ["get_protocol(_)->undefined.\n"],
	ModStr = "-module(spb).",
	ExportStr = "-compile(export_all).",
	HeaderForms = [forms:to_abstract(ModStr), forms:to_abstract(ExportStr)],
	TypeDataForm = forms:to_abstract(lists:flatten(TypeData2)),
	ProtoDataForm = forms:to_abstract(lists:flatten(ProtoData2)),
	{FullytypeFunForms, FullytypeFunStr} = fullytype_fun_forms(SprotoList),
	Forms = HeaderForms ++ [TypeDataForm, ProtoDataForm] ++ FullytypeFunForms,
	{ok, Mod, Bin} = compile:forms(Forms),
	Filename = do_write_beam(OutDir, Mod, Bin),
	OutputStr = ModStr ++ ExportStr ++ TypeData2 ++ ProtoData2 ++ FullytypeFunStr,
	do_write_erl(OutDir, Mod, OutputStr, WriteErl),
	io:format("[debug]dump:~n ~s ~n", [OutputStr]),
	io:format("dump to file ~s succ.~n", [Filename]),
	ok.


%% ========================================================================
%%  Local functions
%% ========================================================================
% get_subprototype_str(ProtoR, request) ->
% 	{protocol, Name, Tag, SubList} = ProtoR,
% 	Str = case lists:keyfind(request, 1, SubList) of 
% 		false -> "";
% 		{_, TypestrOrFieldlist} -> 
% 			case TypestrOrFieldlist =:= [] orelse is_tuple(hd(TypestrOrFieldlist)) of
% 				true -> % field list
% 					TypeR = #type{name = Name ++ ".request", fields_or_types = TypestrOrFieldlist}
% 					"get_type(\"" ++ Name ++ "\",request)->\n" ++ io_lib:format("~p", [TypeR]) ++ ";\n";
% 				false -> % type string
% 					"get_type(\"" ++ Name ++ "\",request)->\n" ++ io_lib:format("~p", [TypestrOrFieldlist]) ++ ";\n"
% 			end
% 	end,
% 	Str;
% get_subprototype_str(ProtoR, response) ->
% 	Str = case lists:keyfind(response, 1, SubList) of 
% 		false -> "";
% 		{_, TypestrOrFieldlist} -> 
% 			case TypestrOrFieldlist =:= [] orelse is_tuple(hd(TypestrOrFieldlist)) of
% 				true -> % field list
% 					TypeR = #type{name = Name ++ ".response", fields_or_types = TypestrOrFieldlist}
% 					"get_type(\"" ++ Name ++ "\",response)->\n" ++ io_lib:format("~p", [TypeR]) ++ ";\n";
% 				false -> % type string
% 					"get_type(\"" ++ Name ++ "\",response)->\n" ++ io_lib:format("~p", [TypestrOrFieldlist]) ++ ";\n"
% 			end
% 	end,
% 	Str.

do_write_erl(OutDir, Mod, Str, WriteErl) when is_list(OutDir) ->
	case WriteErl of 
		false -> skip;
		true ->
		    Filename = filename:join([OutDir, atom_to_list(Mod) ++ ".erl"]),
		    ok = file:write_file(Filename, Str)
    end.

do_write_beam(OutDir, Mod, Bin) when is_list(OutDir) ->
    Filename = filename:join([OutDir, atom_to_list(Mod) ++ ".beam"]),
    ok = file:write_file(Filename, Bin),
    Filename.

fullytype_fun_forms(SprotoList) ->
	Str1 = fullytype_fun_str1(SprotoList),
	Str2 = fullytype_fun_str2(),
	Form1 = forms:to_abstract(lists:flatten(Str1)),
	Form2 = forms:to_abstract(lists:flatten(Str2)),
	{[Form1, Form2], Str1 ++ Str2}.
fullytype_fun_str1(SprotoList) ->
	TypeDict = gen_type_dict(SprotoList, "root", []),
	Str = "get_fully_type(TypeName)->\n" ++ io_lib:format("TypeDict = ~p,\n", [TypeDict]) ++ 
		"get_fully_type(TypeName, TypeDict, []).\n",
	Str.
fullytype_fun_str2() ->
	Str = "get_fully_type(TypeName, TypeDict, Acc) ->\n" ++
		"case string:str(TypeName, \".\") > 0 of\n" ++
		"true -> string:tokens(TypeName, \".\");\n" ++ 
		"false ->\n" ++
		"case lists:keyfind(TypeName, 1, TypeDict) of\n" ++
		"false -> throw(\"unknown type\");\n" ++
		"{_, \"root\"} -> [TypeName | Acc];\n" ++
		"{_, ParentType} -> get_fully_type(ParentType, TypeDict, [TypeName | Acc])\n" ++
		"end\n" ++
		"end.\n",
	Str.

% get_fully_type(TypeName) ->
% 	TypeDict = [],
% 	get_fully_type(TypeName, TypeDict, []).

% % pass a type name, return a fully type list, like ["parenttype", "subtype"]
% get_fully_type(TypeName, TypeDict, Acc) ->
% 	case string:str(TypeName, ".") > 0 of
% 		true -> string:tokens(TypeName, ".");
% 		false -> 
% 			case lists:keyfind(TypeName, 1, TypeDict) of 
% 				false -> throw("unknown type");
% 				{_, "root"} -> [TypeName | Acc];
% 				{_, ParentType} -> get_fully_type(ParentType, TypeDict, [TypeName | Acc])
% 			end
% 	end.


% generate type-parenttype dictionary by sproto list, like [{"type", "parenttype"}, ..]
gen_type_dict([], _, Acc) ->
	Acc;
gen_type_dict([H|T], ParentType, Acc) ->
	case is_record(H, type) of 
		true ->
			#type{name=Name, fields_or_types=List} = H,
			AddDict = [{Name, ParentType} | gen_type_dict(List, Name, [])],
			gen_type_dict(T, ParentType, Acc ++ AddDict);
		false ->
			gen_type_dict(T, ParentType, Acc)
	end.