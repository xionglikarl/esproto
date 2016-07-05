-module(test).
-include("sp.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(TEST_DIR, "./_build/test/lib/esproto").
-define(TEST_EBIN_DIR, ?TEST_DIR ++ "/ebin").
-define(TEST_SPROTO_DIR, ?TEST_DIR ++ "/test/extra/sprotos").


test1_test() ->
	Res = sprotodump:file(?TEST_SPROTO_DIR ++ "/example.sproto"),
	io:format("test1_test sprotodump res:~p~n", [Res]),
	ok.

test2_test() ->
	sprotodump:dir(?TEST_SPROTO_DIR, ?TEST_EBIN_DIR).

test3_test() ->
	sprotodump:dir(?TEST_SPROTO_DIR, ?TEST_EBIN_DIR),
	code:purge(spb),
	code:load_file(spb),
	Data = #{
	    "number" => 100000,
    	"bignumber" => -10000000000
		},
	Bin = sproto:encode("Data", Data),
	print_bin(Bin),

	% test decode
	{Len, List, RestBin} = sproto:decode("Data", Bin),
	io:format("Len:~p, RestBin:~p~n", [Len, RestBin]),
	io:format("List:~p~n", [List]),
	ok.

test4_test() ->
	sprotodump:dir(?TEST_SPROTO_DIR, ?TEST_EBIN_DIR),
	code:purge(spb),
	code:load_file(spb),
	AddressBook = #{
	    "person" => [
			#{
				"name" => "Alice",
				"id" => 10000,
				"phone" => [
					#{ "number" => #{"aa" => "123456789"} , "type" => 1 },
					#{ "number" => #{"aa" => "87654321"} , "type" => 2 }
				]
			},
			#{
				"name" => "Bob",
				"id" => 20000,
				"phone" => [
					#{ "number" => #{"aa" => "01234567890"} , "type" => 3 }
				]
			}
		],
		"others" => [
			#{
				"name" => "Carol",
				"id" => 30000,
				"phone" => [
					#{ "number" => #{"aa" => "9876543210"} }
				]
			}
		]
		},
	Bin = sproto:encode("AddressBook", AddressBook),
	print_bin(Bin),

	{Len, List, RestBin} = sproto:decode("AddressBook", Bin),
	io:format("Len:~p, RestBin:~p~n", [Len, RestBin]),
	io:format("List:~p~n", [List]),
	{Map, _, _} = sproto:decode2("AddressBook", Bin),
	Map.

test5_test() ->
	sprotodump:dir(?TEST_SPROTO_DIR, ?TEST_EBIN_DIR),
	code:purge(spb),
	code:load_file(spb),
	% expampe 1
	Person = #{"name" => "Alice" ,  "age" => 13, "marital" => false},
	Bin = sproto:encode("Person1", Person),
	print_bin(Bin),
	{Map, _, _} = sproto:decode2("Person1", Bin),
	io:format("Map:~p~n", [Map]),
	% expampe 2
	Person2 = #{"name" => "Bob" ,  "age" => 40, 
	"children" => [#{"name" => "Alice" ,  "age" => 13}, #{"name" => "Carol" ,  "age" => 5}]},
	Bin2 = sproto:encode("Person1", Person2),
	print_bin(Bin2),
	{Map2, _, _} = sproto:decode2("Person1", Bin2),
	io:format("Map:~p~n", [Map2]),
	% expampe 3
	Data = #{"numbers" => [1,2,3,4,5] },
	Bin3 = sproto:encode("Data", Data),
	print_bin(Bin3),
	{Map3, _, _} = sproto:decode2("Data", Bin3),
	io:format("Map:~p~n", [Map3]),
	% expampe 4
	Data2 = #{"numbers" => [(1 bsl 32)+1, (1 bsl 32)+2, (1 bsl 32)+3] },
	Bin4 = sproto:encode("Data", Data2),
	print_bin(Bin4),
	{Map4, _, _} = sproto:decode2("Data", Bin4),
	io:format("Map:~p~n", [Map4]),
	% expampe 5
	Data3 = #{"bools" => [false, true, false] },
	Bin5 = sproto:encode("Data", Data3),
	print_bin(Bin5),
	{Map5, _, _} = sproto:decode2("Data", Bin5),
	io:format("Map:~p~n", [Map5]),
	% expampe 6
	Data4 = #{"number" => 100000, "bignumber" => -10000000000},
	Bin6 = sproto:encode("Data", Data4),
	print_bin(Bin6),
	{Map6, _, _} = sproto:decode2("Data", Bin6),
	io:format("Map:~p~n", [Map6]),
	ok.

test6_test() ->
	sprotodump:dir(?TEST_SPROTO_DIR, ?TEST_EBIN_DIR),
	code:purge(spb),
	code:load_file(spb),
	AddressBook = #{
	    "person" => [
			#{
				"name" => "Alice",
				"id" => 10000,
				"phone" => [
					#{ "number" => #{"aa" => "123456789"} , "type" => 1 },
					#{ "number" => #{"aa" => "87654321"} , "type" => 2 }
				]
			},
			#{
				"name" => "Bob",
				"id" => 20000,
				"phone" => [
					#{ "number" => #{"aa" => "01234567890"} , "type" => 3 }
				]
			}
		],
		"others" => [
			#{
				"name" => "Carol",
				"id" => 30000,
				"phone" => [
					#{ "number" => #{"aa" => "9876543210"} }
				]
			}
		]
		},
	Bin = sproto:encode("AddressBook", AddressBook),
	print_bin(Bin),
	PackedBin = sproto:pack(Bin),
	io:format("Packed Bin:~n", []),
	print_bin(PackedBin),
	UnPackedBin = sproto:unpack(PackedBin),
	io:format("UnPackedBin:~n", []),
	print_bin(UnPackedBin),
	ok.

test7_test() ->
	sprotodump:dir(?TEST_SPROTO_DIR, ?TEST_EBIN_DIR, true),
	code:purge(spb),
	code:load_file(spb),
	Bin = sproto:encode("Foo", #{}),
	print_bin(Bin),
	{Map, _, _} = sproto:decode2("Foo", Bin),
	io:format("Map:~p~n", [Map]),
	ok.

%% rpc test1
test8_test() ->
	sprotodump:dir(?TEST_SPROTO_DIR, ?TEST_EBIN_DIR, true),
	code:purge(spb),
	code:load_file(spb),
	% client request
	sproto:rpc_init(),
	TypeName = "get_dreamland_info",
	DataMap = #{"id" => 1},
	Session = 1,
	Bin1 = sproto:proto_encode(TypeName, DataMap, Session),
	print_bin(Bin1),
	% server receive and decode
	DecodeInfo = sproto:proto_decode(Bin1),
	io:format("DecodeInfo:~p~n", [DecodeInfo]),
	{_, _, _DecodeMap, ResponseFun, _} = DecodeInfo,
	% server response
	ResponseDataMap = #{"count" => 1, "infos" => [#{"bid" => 1,
		"score" => 99, "time" => 1, "first" => true
	}]},
	ResponseBin = ResponseFun(ResponseDataMap, undefined),
	print_bin(ResponseBin),
	% client receive response
	DecodeInfo2 = sproto:proto_decode(ResponseBin),
	io:format("DecodeInfo2:~p~n", [DecodeInfo2]),
	% check client session
	[] = ets:lookup(?SPROTO_SESSION_ETS, Session),
	ok.

%% rpc test2
test9_test() ->
	sprotodump:dir(?TEST_SPROTO_DIR, ?TEST_EBIN_DIR, true),
	code:purge(spb),
	code:load_file(spb),
	% client request
	sproto:rpc_init(),
	TypeName = "get_dreamland_info2",
	DataMap = #{},
	Session = 2,
	Bin1 = sproto:proto_encode(TypeName, DataMap, Session),
	print_bin(Bin1),
	% server receive and decode
	DecodeInfo = sproto:proto_decode(Bin1),
	io:format("DecodeInfo:~p~n", [DecodeInfo]),
	{_, _, _DecodeMap, ResponseFun, _} = DecodeInfo,
	% server response
	ResponseDataMap = #{"count" => 1, "infos" => [#{"bid" => 1,
		"score" => 99, "time" => 1, "first" => true
	}]},
	ResponseBin = ResponseFun(ResponseDataMap, undefined),
	print_bin(ResponseBin),
	% client receive response
	DecodeInfo2 = sproto:proto_decode(ResponseBin),
	io:format("DecodeInfo2:~p~n", [DecodeInfo2]),
	% check client session
	[] = ets:lookup(?SPROTO_SESSION_ETS, Session),
	ok.

%% rpc test3
test10_test() ->
	sprotodump:dir(?TEST_SPROTO_DIR, ?TEST_EBIN_DIR, true),
	code:purge(spb),
	code:load_file(spb),
	% server notify
	sproto:rpc_init(),
	TypeName = "notify",
	DataMap = #{"info" => 1},
	Session = undefined,
	Bin1 = sproto:proto_encode(TypeName, DataMap, Session),
	print_bin(Bin1),
	% client receive and decode
	DecodeInfo = sproto:proto_decode(Bin1),
	io:format("DecodeInfo:~p~n", [DecodeInfo]),
	{_, _, _DecodeMap, undefined, _} = DecodeInfo,
	ok.

%% rpc test4, user data
test11_test() ->
	sprotodump:dir(?TEST_SPROTO_DIR, ?TEST_EBIN_DIR, true),
	code:purge(spb),
	code:load_file(spb),
	% client request
	sproto:rpc_init(),
	TypeName = "get_dreamland_info",
	DataMap = #{"id" => 1},
	Session = 3,
	UserData = #{"status" => 1, "msg" => "hello"},
	Bin1 = sproto:proto_encode(TypeName, DataMap, Session, UserData),
	print_bin(Bin1),
	% server receive and decode
	DecodeInfo = sproto:proto_decode(Bin1),
	io:format("DecodeInfo:~p~n", [DecodeInfo]),
	{_, _, _DecodeMap, ResponseFun, _} = DecodeInfo,
	% server response
	ResponseDataMap = #{"count" => 1, "infos" => [#{"bid" => 1,
		"score" => 99, "time" => 1, "first" => true
	}]},
	ResponseBin = ResponseFun(ResponseDataMap, UserData),
	print_bin(ResponseBin),
	% client receive response
	DecodeInfo2 = sproto:proto_decode(ResponseBin),
	io:format("DecodeInfo2:~p~n", [DecodeInfo2]),
	% check client session
	[] = ets:lookup(?SPROTO_SESSION_ETS, Session),
	ok.



print_bin(Bin) ->
	ByteList = lists:reverse(bin_to_hex(Bin, [])),
	Fun = fun(Byte, Acc) ->
	 	io:format("~2.16.0b ", [Byte]),
	 	case Acc rem 8  =:= 0 of
	 		true -> io:format("~n", []);
	 		false -> ok
	 	end,
	 	Acc + 1 
	end,
	lists:foldl(Fun, 1, ByteList),
	io:format("bytes:~p~n", [byte_size(Bin)]).

bin_to_hex(<<>>, Acc) ->
	Acc;
bin_to_hex(Bin, Acc) ->
	<<A:8, Bin2/binary>> = Bin,
	bin_to_hex(Bin2, [A|Acc]).