-module(sprotoparse).
-export([parse/1,file/1]).
-define(p_anything,true).
-define(p_charclass,true).
-define(p_choose,true).
-define(p_label,true).
-define(p_not,true).
-define(p_one_or_more,true).
-define(p_optional,true).
-define(p_scan,true).
-define(p_seq,true).
-define(p_string,true).
-define(p_zero_or_more,true).



-spec file(file:name()) -> any().
file(Filename) -> case file:read_file(Filename) of {ok,Bin} -> parse(Bin); Err -> Err end.

-spec parse(binary() | list()) -> any().
parse(List) when is_list(List) -> parse(unicode:characters_to_binary(List));
parse(Input) when is_binary(Input) ->
  _ = setup_memo(),
  Result = case 'all'(Input,{{line,1},{column,1}}) of
             {AST, <<>>, _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

-spec 'all'(input(), index()) -> parse_result().
'all'(Input, Index) ->
  p(Input, Index, 'all', fun(I,D) -> (p_seq([fun 'blank0'/2, p_zero_or_more(p_seq([p_choose([fun 'type'/2, fun 'protocol'/2]), fun 'blank0'/2]))]))(I,D) end, fun(Node, _Idx) ->
[_|[T]] = Node,
DataList = [H || [H|_] <- T],

DataList
 end).

-spec 'protocol'(input(), index()) -> parse_result().
'protocol'(Input, Index) ->
  p(Input, Index, 'protocol', fun(I,D) -> (p_seq([p_label('name', fun 'name'/2), fun 'blanks'/2, p_label('tag', fun 'tag'/2), fun 'blank0'/2, p_string(<<"{">>), fun 'blank0'/2, p_label('sub', p_zero_or_more(p_seq([fun 'subproto'/2, fun 'blank0'/2]))), p_string(<<"}">>)]))(I,D) end, fun(Node, _Idx) ->
Name = binary_to_list(iolist_to_binary(proplists:get_value(name, Node))),
Tag = list_to_integer(binary_to_list(iolist_to_binary(proplists:get_value(tag, Node)))),
SubList = proplists:get_value(sub, Node),
SubProtoList = [Head || [Head, _] <- SubList],
{protocol, Name, Tag, SubProtoList}
 end).

-spec 'subproto'(input(), index()) -> parse_result().
'subproto'(Input, Index) ->
  p(Input, Index, 'subproto', fun(I,D) -> (p_seq([p_label('parta', p_choose([p_string(<<"request">>), p_string(<<"response">>)])), fun 'blanks'/2, p_label('partb', p_choose([fun 'typename'/2, fun 'struct'/2]))]))(I,D) end, fun(Node, _Idx) ->
Parta = binary_to_list(iolist_to_binary(proplists:get_value(parta, Node))),
Partb = proplists:get_value(partb, Node),
Partb2 = 
	case is_tuple(Partb) of
		false -> Partb;
		true -> element(1, Partb)
	end,
{Parta, Partb2}
 end).

-spec 'type'(input(), index()) -> parse_result().
'type'(Input, Index) ->
  p(Input, Index, 'type', fun(I,D) -> (p_seq([p_string(<<".">>), p_label('name', fun 'name'/2), fun 'blank0'/2, p_label('struct', fun 'struct'/2)]))(I,D) end, fun(Node, _Idx) ->
Name = binary_to_list(iolist_to_binary(proplists:get_value(name, Node))),
Struct = proplists:get_value(struct, Node),
{type, Name, Struct}
 end).

-spec 'struct'(input(), index()) -> parse_result().
'struct'(Input, Index) ->
  p(Input, Index, 'struct', fun(I,D) -> (p_seq([p_string(<<"{">>), fun 'blank0'/2, p_zero_or_more(p_seq([p_choose([fun 'field'/2, fun 'type'/2]), fun 'blank0'/2])), p_string(<<"}">>)]))(I,D) end, fun(Node, _Idx) ->
[_, _, List, _] = Node,
[H || [H|_] <- List]
 end).

-spec 'field'(input(), index()) -> parse_result().
'field'(Input, Index) ->
  p(Input, Index, 'field', fun(I,D) -> (p_seq([p_label('name', fun 'name'/2), fun 'blanks'/2, p_label('tag', fun 'tag'/2), fun 'blank0'/2, p_string(<<":">>), fun 'blank0'/2, p_label('isarray', p_zero_or_more(p_string(<<"*">>))), p_label('datatype', fun 'typename'/2), p_label('key', p_zero_or_more(fun 'mainkey'/2))]))(I,D) end, fun(Node, _Idx) ->
Name = binary_to_list(iolist_to_binary(proplists:get_value(name, Node))),
Tag = list_to_integer(binary_to_list(iolist_to_binary(proplists:get_value(tag, Node)))),
Isarray = 
	case proplists:get_value(isarray, Node) =:= [<<"*">>] of
		true -> true;
		false -> false
	end,
{Buildintype, Datatype} = 
	case proplists:get_value(datatype, Node) of
		{"string", _} -> {1, undefined};
		{"integer", _} -> {2, undefined};
		{"boolean", _} -> {3, undefined};
		{Other, SubName} -> {0, Other ++ SubName}
	end,
Key = binary_to_list(iolist_to_binary(proplists:get_value(key, Node))),
{field, Name, Tag, Buildintype, Datatype, Isarray, Key}
 end).

-spec 'eof'(input(), index()) -> parse_result().
'eof'(Input, Index) ->
  p(Input, Index, 'eof', fun(I,D) -> (p_not(p_string(<<".">>)))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'newline'(input(), index()) -> parse_result().
'newline'(Input, Index) ->
  p(Input, Index, 'newline', fun(I,D) -> (p_seq([p_optional(p_charclass(<<"[\r]">>)), p_charclass(<<"[\n]">>)]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'line_comment'(input(), index()) -> parse_result().
'line_comment'(Input, Index) ->
  p(Input, Index, 'line_comment', fun(I,D) -> (p_seq([p_string(<<"#">>), p_zero_or_more(p_seq([p_not(fun 'newline'/2), p_anything()])), p_choose([fun 'newline'/2, fun 'eof'/2])]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'blank'(input(), index()) -> parse_result().
'blank'(Input, Index) ->
  p(Input, Index, 'blank', fun(I,D) -> (p_choose([p_charclass(<<"[\s\t]">>), fun 'newline'/2, fun 'line_comment'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'blank0'(input(), index()) -> parse_result().
'blank0'(Input, Index) ->
  p(Input, Index, 'blank0', fun(I,D) -> (p_zero_or_more(fun 'blank'/2))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'blanks'(input(), index()) -> parse_result().
'blanks'(Input, Index) ->
  p(Input, Index, 'blanks', fun(I,D) -> (p_one_or_more(fun 'blank'/2))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'alpha'(input(), index()) -> parse_result().
'alpha'(Input, Index) ->
  p(Input, Index, 'alpha', fun(I,D) -> (p_choose([p_charclass(<<"[a-z]">>), p_charclass(<<"[A-Z]">>), p_string(<<"_">>)]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'alnum'(input(), index()) -> parse_result().
'alnum'(Input, Index) ->
  p(Input, Index, 'alnum', fun(I,D) -> (p_choose([fun 'alpha'/2, p_charclass(<<"[0-9]">>)]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'word'(input(), index()) -> parse_result().
'word'(Input, Index) ->
  p(Input, Index, 'word', fun(I,D) -> (p_seq([fun 'alpha'/2, p_zero_or_more(fun 'alnum'/2)]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'name'(input(), index()) -> parse_result().
'name'(Input, Index) ->
  p(Input, Index, 'name', fun(I,D) -> (fun 'word'/2)(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'typename'(input(), index()) -> parse_result().
'typename'(Input, Index) ->
  p(Input, Index, 'typename', fun(I,D) -> (p_seq([fun 'word'/2, p_zero_or_more(p_seq([p_string(<<".">>), fun 'word'/2]))]))(I,D) end, fun(Node, _Idx) ->
[Head, List] = Node,
SubName = 
  case List of
    [] -> "";
    _ -> 
      	List2 = [[Dot, iolist_to_binary(Word)] || [Dot, Word] <- List],
     	binary_to_list( list_to_binary(lists:append(List2)) )
  end,
{binary_to_list(iolist_to_binary(Head)), SubName}
 end).

-spec 'tag'(input(), index()) -> parse_result().
'tag'(Input, Index) ->
  p(Input, Index, 'tag', fun(I,D) -> (p_one_or_more(p_charclass(<<"[0-9]">>)))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'mainkey'(input(), index()) -> parse_result().
'mainkey'(Input, Index) ->
  p(Input, Index, 'mainkey', fun(I,D) -> (p_seq([p_string(<<"(">>), fun 'blank0'/2, p_label('name', fun 'name'/2), fun 'blank0'/2, p_string(<<")">>)]))(I,D) end, fun(Node, _Idx) ->
proplists:get_value(name, Node)
 end).



-file("peg_includes.hrl", 1).
-type index() :: {{line, pos_integer()}, {column, pos_integer()}}.
-type input() :: binary().
-type parse_failure() :: {fail, term()}.
-type parse_success() :: {term(), input(), index()}.
-type parse_result() :: parse_failure() | parse_success().
-type parse_fun() :: fun((input(), index()) -> parse_result()).
-type xform_fun() :: fun((input(), index()) -> term()).

-spec p(input(), index(), atom(), parse_fun(), xform_fun()) -> parse_result().
p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  case get_memo(StartIndex, Name) of      % See if the current reduction is memoized
    {ok, Memo} -> %Memo;                     % If it is, return the stored result
      Memo;
    _ ->                                        % If not, attempt to parse
      Result = case ParseFun(Inp, StartIndex) of
        {fail,_} = Failure ->                       % If it fails, memoize the failure
          Failure;
        {Match, InpRem, NewIndex} ->               % If it passes, transform and memoize the result.
          Transformed = TransformFun(Match, StartIndex),
          {Transformed, InpRem, NewIndex}
      end,
      memoize(StartIndex, Name, Result),
      Result
  end.

-spec setup_memo() -> ets:tid().
setup_memo() ->
  put({parse_memo_table, ?MODULE}, ets:new(?MODULE, [set])).

-spec release_memo() -> true.
release_memo() ->
  ets:delete(memo_table_name()).

-spec memoize(index(), atom(), parse_result()) -> true.
memoize(Index, Name, Result) ->
  Memo = case ets:lookup(memo_table_name(), Index) of
              [] -> [];
              [{Index, Plist}] -> Plist
         end,
  ets:insert(memo_table_name(), {Index, [{Name, Result}|Memo]}).

-spec get_memo(index(), atom()) -> {ok, term()} | {error, not_found}.
get_memo(Index, Name) ->
  case ets:lookup(memo_table_name(), Index) of
    [] -> {error, not_found};
    [{Index, Plist}] ->
      case proplists:lookup(Name, Plist) of
        {Name, Result}  -> {ok, Result};
        _  -> {error, not_found}
      end
    end.

-spec memo_table_name() -> ets:tid().
memo_table_name() ->
    get({parse_memo_table, ?MODULE}).

-ifdef(p_eof).
-spec p_eof() -> parse_fun().
p_eof() ->
  fun(<<>>, Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.
-endif.

-ifdef(p_optional).
-spec p_optional(parse_fun()) -> parse_fun().
p_optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.
-endif.

-ifdef(p_not).
-spec p_not(parse_fun()) -> parse_fun().
p_not(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.
-endif.

-ifdef(p_assert).
-spec p_assert(parse_fun()) -> parse_fun().
p_assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.
-endif.

-ifdef(p_seq).
-spec p_seq([parse_fun()]) -> parse_fun().
p_seq(P) ->
  fun(Input, Index) ->
      p_all(P, Input, Index, [])
  end.

-spec p_all([parse_fun()], input(), index(), [term()]) -> parse_result().
p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> p_all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.
-endif.

-ifdef(p_choose).
-spec p_choose([parse_fun()]) -> parse_fun().
p_choose(Parsers) ->
  fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
  end.

-spec p_attempt([parse_fun()], input(), index(), none | parse_failure()) -> parse_result().
p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P|Parsers], Input, Index, FirstFailure)->
  case P(Input, Index) of
    {fail, _} = Failure ->
      case FirstFailure of
        none -> p_attempt(Parsers, Input, Index, Failure);
        _ -> p_attempt(Parsers, Input, Index, FirstFailure)
      end;
    Result -> Result
  end.
-endif.

-ifdef(p_zero_or_more).
-spec p_zero_or_more(parse_fun()) -> parse_fun().
p_zero_or_more(P) ->
  fun(Input, Index) ->
      p_scan(P, Input, Index, [])
  end.
-endif.

-ifdef(p_one_or_more).
-spec p_one_or_more(parse_fun()) -> parse_fun().
p_one_or_more(P) ->
  fun(Input, Index)->
      Result = p_scan(P, Input, Index, []),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {fail, {expected, Failure, _}} = P(Input,Index),
          {fail, {expected, {at_least_one, Failure}, Index}}
      end
  end.
-endif.

-ifdef(p_label).
-spec p_label(atom(), parse_fun()) -> parse_fun().
p_label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.
-endif.

-ifdef(p_scan).
-spec p_scan(parse_fun(), input(), index(), [term()]) -> {[term()], input(), index()}.
p_scan(_, <<>>, Index, Accum) -> {lists:reverse(Accum), <<>>, Index};
p_scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> p_scan(P, InpRem, NewIndex, [Result | Accum])
  end.
-endif.

-ifdef(p_string).
-spec p_string(binary()) -> parse_fun().
p_string(S) ->
    Length = erlang:byte_size(S),
    fun(Input, Index) ->
      try
          <<S:Length/binary, Rest/binary>> = Input,
          {S, Rest, p_advance_index(S, Index)}
      catch
          error:{badmatch,_} -> {fail, {expected, {string, S}, Index}}
      end
    end.
-endif.

-ifdef(p_anything).
-spec p_anything() -> parse_fun().
p_anything() ->
  fun(<<>>, Index) -> {fail, {expected, any_character, Index}};
     (Input, Index) when is_binary(Input) ->
          <<C/utf8, Rest/binary>> = Input,
          {<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
  end.
-endif.

-ifdef(p_charclass).
-spec p_charclass(string() | binary()) -> parse_fun().
p_charclass(Class) ->
    {ok, RE} = re:compile(Class, [unicode, dotall]),
    fun(Inp, Index) ->
            case re:run(Inp, RE, [anchored]) of
                {match, [{0, Length}|_]} ->
                    {Head, Tail} = erlang:split_binary(Inp, Length),
                    {Head, Tail, p_advance_index(Head, Index)};
                _ -> {fail, {expected, {character_class, binary_to_list(Class)}, Index}}
            end
    end.
-endif.

-ifdef(p_regexp).
-spec p_regexp(binary()) -> parse_fun().
p_regexp(Regexp) ->
    {ok, RE} = re:compile(Regexp, [unicode, dotall, anchored]),
    fun(Inp, Index) ->
        case re:run(Inp, RE) of
            {match, [{0, Length}|_]} ->
                {Head, Tail} = erlang:split_binary(Inp, Length),
                {Head, Tail, p_advance_index(Head, Index)};
            _ -> {fail, {expected, {regexp, binary_to_list(Regexp)}, Index}}
        end
    end.
-endif.

-ifdef(line).
-spec line(index() | term()) -> pos_integer() | undefined.
line({{line,L},_}) -> L;
line(_) -> undefined.
-endif.

-ifdef(column).
-spec column(index() | term()) -> pos_integer() | undefined.
column({_,{column,C}}) -> C;
column(_) -> undefined.
-endif.

-spec p_advance_index(input() | unicode:charlist() | pos_integer(), index()) -> index().
p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput)-> % strings
  lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
