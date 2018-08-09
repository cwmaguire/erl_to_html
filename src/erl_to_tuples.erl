%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
-module(erl_to_tuples).

-include("colours.hrl").

-export([write_tuples/1]).
-export([parse_transform/2]).


write_tuples(Filename) ->
    io:format(user, "Compiling ~p~n", [Filename]),
    Result = compile:file(Filename, [report_errors,
                                     return_errors,
                                     {parse_transform, ?MODULE},
                                     {d, filename, Filename}]),
    io:format(user, "Compile result:~n~p~n", [Result]).

parse_transform(Forms, Options) ->
    io:format(user, "Scanning ~p for indentation~n", [Filename]),
    Indents = get_indents:indents(Filename),
    io:format(user, "Scanning ~p for comments~n", [Filename]),
    Comments = get_comments:comments(Filename),

    io:format(user, "Parse transforming forms: ~n"
                    "\t~p~n"
                    "\t with Options:~n"
                    "\t~p~n",
              [Forms, Options]),

    Tuples = parse(lines(tuples(Forms), Indents, Comments)),
    erl_to_png:render(Tuples),
    Forms.


%% walk the tree and wrap lines in spans
lines(Tree, Indents, Comments) ->
    {Tree2, _} = lines([], Tree, 0, Indents, Comments),
    StartLine = [{_Line = 0, _LineNumber = <<"0">>, <<"93a1a1">>}]
                 %html_spaces(maps:get(1, Indents, 0)),
    [StartLine, Tree2].

lines(Tree, [], Line, _Indents, _Comments)  ->
    {lists:reverse(Tree), Line};
lines(Tree, [Head | Rest], Line, Indents, Comments) when is_list(Head) ->
    {SubTree, CurrLine} = lines([], Head, Line, Indents, Comments),
    lines([SubTree | Tree], Rest, CurrLine, Indents, Comments);
lines(Tree, [{line, Line} | Rest], Line, Indents, Comments) ->
    % same line
    lines(Tree, Rest, Line, Indents, Comments);
lines(Tree, [{line, NewLine} | Rest], Line, Indents, Comments) ->
    EndOfLine =
        case maps:get(Line, Comments, undefined) of
            undefined ->
                [];
            Comment ->
                parse_comment(Line, Comment)
        end,
    StartLine = EndOfLine ++ ,
                 {Line, i2b(Line + 1), ?LINE_COLOUR},
                 html_spaces(maps:get(NewLine, Indents, 0))],
    lines([StartLine | Tree],
          [{line, NewLine} | Rest],
          Line + 1,
          Indents,
          Comments);
lines(Tree, [Head | Rest], Line, Indents, Comments) ->
    lines([Head | Tree], Rest, Line, Indents, Comments).

html_spaces(NumSpaces) ->
    {Line, [$  || _ <- lists:seq(1, NumSpaces)], }.

parse_comment(Comment, Line) ->
    parse_comment(_Text = "", _Parsed = [], Comment, Line).

parse_comment(RemainingText, Parsed, _Comment = [], Line) ->
    TextTuple = {Line, RemainingText, ?COMMENT_COLOUR},
    lists:reverse([TextTuple | Parsed]);
parse_comment(Text, Parsed, "TODO" ++ Rest, Line) ->
    TextTuple = {Line, Text, ?COMMENT_COLOUR},
    KeywordTuple = {Line, "TODO", ?COMMENT_KEYWORD_COLOUR},
    parse_comment([KeywordTuple, TextTuple | Parsed], Rest, Line);
parse_comment(Line, Text, Parsed, "NOTE" ++ Rest, Line) ->
    TextTuple = {Line, Text, ?COMMENT_COLOUR},
    KeywordTuple = {Line, "NOTE", ?COMMENT_KEYWORD_COLOUR},
    parse_comment([KeywordTuple, TextTuple | Parsed], Rest, Line);
parse_comment(Text, Parsed, "FIXME" ++ Rest) ->
    TextTuple = {Line, Text, ?COMMENT_COLOUR},
    KeywordTuple = {Line, "FIXME", ?COMMENT_KEYWORD_COLOUR},
    parse_comment("", [KeywordTuple, TextTuple | Parsed], Rest, Line);
parse_comment(Text, Parsed, [Head | Rest], Line) ->
    parse_comment(Text ++ [Head], Parsed, Rest, Line).

%% No raw numbers should show up in the HTML
parse(String = [I | _]) when is_number(I) ->
    String;
parse(Binary) when is_binary(Binary) ->
    Binary;
parse(List) when is_list(List) ->
    lists:map(fun parse/1, List);
parse(Term) when is_atom(Term); is_tuple(Term) ->
    parse(parse_symbol(Term));
parse(Other) ->
    io:format(user, "Parsing other = ~p~n", [Other]),
    Other.

escape(String0) ->
    % TODO might need to "no_parse" these
    Lt = <<"<">>,
    HtmlLt = <<"&lt;">>,
    Gt = <<">">>,
    HtmlGt = <<"&gt;">>,
    % TODO This is showing up without the backslash
    Quote = <<"\"">>,
    HtmlQuote = <<"&quot;">>,
    Replacements = [{Gt, HtmlGt},
                    {Lt, HtmlLt},
                    {Quote, HtmlQuote}],

    lists:foldl(fun({Old, New}, String) ->
                    string:replace(String, Old, New, all)
                end, String0, Replacements).

tuples(Forms) when is_list(Forms) ->
    [tuple(Form) || Form <- Forms];

%% -type form(Form) -> Form.
%%  Here we show every known form and valid internal structure. We do not
%%  that the ordering is correct!

%% First the various attributes.
tuple({attribute,Line,module,Mod}) ->
    [line(Line),
     parse_symbol(Line, '-'),
     parse_symbol(Line, module),
     parse_symbol(Line, '('),
     module(Line, Mod),
     parse_symbol(Line, ')'),
     parse_symbol(Line, '.'),
tuple({attribute,_AttributeLine,file,{File,_FileLine}}) ->	%This is valid anywhere.
    %% Manually set this to 0 to come before any source lines
    [line(0),
     parse_symbol(0, '%'),
     parse_symbol(0, '-'),
     parse_symbol(0, 'file'),
     parse_symbol(0, '('),
     file_literal((0, File),
     parse_symbol(0, ')'),
     parse_symbol(0, '.')];
tuple({attribute,Line,export,Es0}) ->
    [line(Line),
     parse_symbol(Line, '-'),
     span("export"), parse_symbol(Line, '['),
     farity_list(Es0), parse_symbol(Line, ']'),
     '.'];
     tuple({attribute,Line,import,{Mod,Is0}}) ->
    [line(Line),
     parse_symbol(Line, '-'),
'import', ',', module(Mod), parse_symbol(Line, '('),
     parse_symbol(Line, '['),
     farity_list(Is0), parse_symbol(Line, ']'),
     parse_symbol(Line, ')'),
     '.'];
tuple({attribute,Line,compile,C}) ->
    [line(Line),
     parse_symbol(Line, '-'),
'compile', parse_symbol(Line, '('),
     span(<<"compile-option">>, atom_to_list(C)), parse_symbol(Line, ')'),
     '.'];
tuple({attribute,Line,record,{Name,Defs}}) ->
    [line(Line),
     parse_symbol(Line, '-'),
'record', parse_symbol(Line, '('),
     span("atom", atom_to_list(Name)), ',', '{',
     record_defs(Defs),
     '}', parse_symbol(Line, ')'),
     '.'];
tuple({attribute,Line,asm,{function,_N,_A,_Code}}) ->
    line(Line);
tuple({attribute,Line,_Attr,_Val}) ->		%The general attribute.
    line(Line);
tuple({function,Line,Name,_Arity,Clauses}) ->
    [line(Line),
     separate(';', lists:map(fun(Clause) -> clause(Name, Clause) end, Clauses)), '.'];
% TODO figure out how to reproduce these
tuple({error,E}) ->
    {error,E};
tuple({warning,W}) ->
    {warning,W};
tuple({eof,Line}) ->
    [line(Line), span(<<"eof">>)].

farity_list(FunArities) ->
    separate(lists:map(fun farity/1, FunArities)).

farity({Name, Arity}) ->
    [function(Name), parse_symbol(Line, '/'),
     arity(Arity)].

%% -type record_defs([RecDef]) -> [RecDef].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *parser*!

record_defs(Defs) ->
    separate(lists:map(fun record_def/1, Defs)).

%% I think integer and atom are the only other values
%% for the type field:
%% {atom, Line, foo}
%% {integer, Line, 1}
%% {type, Line, Type, ListOfSubTypes}
%%
%% fun((...) -> any()) is wierd because the (...) is
%% the any type except without the ListOfSubTypes
%% e.g. {type, Line, any} instead of {type, Line, any, []}

record_def({typed_record_field, RecordDef, Type}) ->
    [record_def(RecordDef),
     record_type_line(RecordDef, Type),
     type(Type)];
record_def({record_field,Line,{atom,_La,A},Val}) ->
    [line(Line),
     span("record_field", a2b(A)), '=', expr(Val)];
record_def({record_field,Line,{atom,_La,A}}) ->
    [line(Line),
     span("record_field", a2b(A))].

record_type_line(RecordDef, RecordType) ->
    RecordLine = element(2, RecordDef),
    TypeLine = element(2, RecordType),
    Line = case TypeLine - RecordLine of
        X when X > 1 ->
            RecordLine + 1;
        _ ->
            RecordLine
    end,
    [line(Line), '::'].

catch_clause({clause, Line, Exception, GuardGroups, Body}) ->
    [{tuple, _Line, [Class, ExceptionPattern, _Wild]}] = Exception,
    [line(Line),
     expr(Class), parse_symbol(Line, ':'),
     expr(ExceptionPattern),
     separate(';', lists:map(fun guard_group/1, GuardGroups)),
     '->',
     separate(lists:map(fun expr/1, Body))].

clause({clause, Line, Head, GuardGroups, Body}) ->
    clause('', {clause, Line, Head, GuardGroups, Body}).

clause(Name, {clause,Line,Head,GuardGroups,Body}) ->
    [line(Line),
     span(<<"function">>, atom_to_list(Name)),
     head(Head),
     separate(';', lists:map(fun guard_group/1, GuardGroups)),
     '->',
     separate(lists:map(fun expr/1, Body))].

case_clause({clause, Line, [Head], GuardGroups, Body}) ->
    [line(Line),
     expr(Head),
     case GuardGroups of
         [] ->
             [];
         _ ->
             F = fun guard_group/1,
             [{Line, <<"when">>, ?TEXT_COLOUR},
              separate({Line, ';', ?TEXT_COLOUR},
                       lists:map(F, GuardGroups))]
     end,
     '->',
     separate({Line, <<",">>, ?TEXT_COLOUR},
              lists:map(fun expr/1, Body))].

head(Expressions) ->
    [parse_symbol(Line, '('),
     separate({Line, <<",">>, ?TEXT_COLOUR},
              lists:map(fun expr/1, Expressions)), ')'].

%% -type pattern(Pattern) -> Pattern.
%%  N.B. Only valid patterns are included here.
expr({lc,Line,Result,Quals}) ->
    E1 = expr(Result),
    [line(Line),
     parse_symbol(Line, '['),
     E1, '||', map_separate(fun lc_bc_qual/1, Quals), ']'];
expr({bc,Line,E0,Quals}) ->
    E1 = expr(E0),
    [line(Line),
     parse_symbol(Line, '<<'),
     E1, '||', map_separate(fun lc_bc_qual/1, Quals), '>>'];
expr({block,Line,Expressions}) ->
    [line(Line),
     'begin', map_separate(fun expr/1, Expressions), 'end'];
expr({'if',Line,Clauses}) ->
    [line(Line),
     'if',
     map_separate(fun clause/1, Clauses),
     'end'];
expr({'case',Line,Expression,Clauses}) ->
    [line(Line),
     'case', expr(Expression), 'of',
     map_separate(';', fun case_clause/1, Clauses),
     'end'];
expr({'receive',Line,Clauses}) ->
    [line(Line),
     'receive', map_separate(fun clause/1, Clauses),
     'end'];
expr({'receive',Line,Clauses,AfterWait,AfterExpressions}) ->
    [line(Line),
     'receive', map_separate(fun clause/1, Clauses),
     'after', expr(AfterWait), '->',
     map_separate(fun expr/1, AfterExpressions),
     'end'];
expr({'try',Line,Expressions,_WhatIsThis,CatchClauses,AfterExpressions}) ->
    [line(Line),
     'try', map_separate(fun expr/1, Expressions),
     'catch', map_separate(';', fun catch_clause/1, CatchClauses),
     'after', map_separate(fun expr/1, AfterExpressions),
     'end'];
expr({'fun',Line,Body}) ->
    case Body of
        {clauses,Clauses} ->
            [line(Line),
             'fun',
             map_separate(fun(Clause) -> clause('', Clause) end, Clauses)];
        {function,Fun,Arity} ->
            [line(Line),
             function(Fun), parse_symbol(Line, '/'),
             arity(Arity)];
        {function,M,F,A} when is_atom(M), is_atom(F), is_integer(A) ->
            [line(Line),
             span(<<"module">>, a2b(M)), parse_symbol(Line, ':'),
             function(F), parse_symbol(Line, '/'),
             arity(A)];
        {function,M0,F0,A0} ->
            %% R15: fun M:F/A with variables.
            M = expr(M0),
            F = expr(F0),
            A = expr(A0),
            [line(Line),
             {Line, a2b(M), ?MODULE_COLOUR},
             parse_symbol(Line, ':'),
             {Line, a2b(F), ?TEXT_COLOUR},
             parse_symbol(Line, '/'),
             {Line, "arity", A}]
    end;
expr({call,Line,Fun,Args}) ->
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
     %io:format(user, "calling expr(~p)~n", [Fun]),
    [line(Line),
     %fun_name(Fun), '(', map_separate(fun expr/1, Args), ')'];
     fun_name(Fun), parse_symbol(Line, '('),
     map_separate(fun expr/1, Args), ')'];
expr({'catch',Line,Expression}) ->
    %% No new variables added.
    [line(Line),
     'catch', expr(Expression)];
expr({match,Line,Expr1,Expr2}) ->
    [line(Line),
     expr(Expr1), '=', expr(Expr2)];
expr({bin,Line,BinElements}) ->
    [line(Line),
     parse_symbol(Line, '<<'),
     map_separate(fun bin/1, BinElements), '>>'];
expr({op,Line,Op,A}) ->
    [line(Line),
     {Line, a2b(Op), ?TEXT_COLOUR},
     expr(A)];
expr({op,Line,'==',L,R}) ->
    [line(Line),
     expr(L),
     {Line, <<"==">>, ?DOUBLE_EQUALS_COLOUR},
     expr(R)];
expr({op,Line,Op,L,R}) ->
    [line(Line),
     expr(L),
     {Line, a2b(Op), ?ATOM_COLOUR},
     expr(R)];
expr({remote, Line, {atom, _MLine, Module}, {atom, _FLine, Function}}) ->
    [line(Line),
     {Line, a2b(Module), ?MODULE_COLOUR},
     parse_symbol(Line, ':'),
     function(Line, Function)];
expr({nil, Line}) ->
    [line(Line), parse_symbol(Line, '['),
     ']'];
expr({var,Line,V}) ->
    [line(Line),
     var(Line, V)];
expr({integer,Line,I}) ->
    [line(Line),
     integer(Line, I)];
expr({char,Line,C}) ->
    [line(Line),
     {Line, <<$$, (list_to_binary(C))/binary>>, ?CHAR_COLOUR}];
expr({float,Line,F}) ->
    [line(Line),
     {Line, list_to_binary(hd(io_lib:format("~w", [F]))), ?FLOAT_COLOUR}];
expr({atom,Line,A}) when A == ':';
                         A == '<<';
                         A == '>>';
                         A == '_';
                         A == '-';
                         A == '*';
                         A == '#';
                         A == '...';
                         A == '->';
                         A == '.';
                         A == ',';
                         A == '[';
                         A == ']';
                         A == '{';
                         A == '}';
                         A == '(';
                         A == '(';
                         A == '"' ->
    io:format(user, "{atom, Line, ~p}", [A]),
    [line(Line),
     {Line, <<$', (a2b(A))/binary, $'>>, ?ATOM_COLOUR}];
expr({atom,Line,A}) ->
    [line(Line),
     {Line, a2b(A), ?ATOM_COLOUR}];
expr({string,Line,S}) ->
    [line(Line),
     '"',
     %{no_parse, <<"--><!-- before parse_control_sequences(S) --><!--\n">>},
     parse_control_sequences(Line, S),
     %{no_parse, <<"--><!-- after parse_control_sequences(S) --><!--\n">>},
     '"'];
expr({tuple,Line,Exprs}) ->
    [line(Line),
     ['{', map_separate(fun expr/1, Exprs), '}']];
%% There's a special case for all cons's after the first: {tail, _}
%% so this is a list of one item.
expr({cons,Line,Head,{nil, _}}) ->
    [line(Line),
     parse_symbol(Line, '['),
     expr(Head), ']'];
expr({cons,Line,Head,{var, _Line2, '_'}}) ->
    [line(Line),
     parse_symbol(Line, '['),
     expr(Head), '|', parse_symbol(Line, '_'),
     ']'];
expr(_Cons = {cons,Line,Head,Tail}) ->
    %io:format(user, "Cons -> Tail = ~p~n", [Cons]),
    [line(Line),
     parse_symbol(Line, '['),
     [expr(Head), ',' | expr({tail, Tail})], ']'];
expr(_Tail = {tail, {cons, Line, Head, {nil, _}}}) ->
    %io:format(user, "Tail 1 = ~p~n", [Tail]),

    [line(Line),
     [expr(Head)]];
expr(_Tail_ = {tail, {cons, Line, Head, Tail}}) ->
    %io:format(user, "Tail 2 = ~p~n", [Tail_]),
    [line(Line),
     [expr(Head), ',' | expr({tail, Tail})]];
expr({tail, {var, Line, Expr}}) ->
    [line(Line),
     var(Line, Expr)];
expr({tail, Call = {call, Line, _Fun, _Args}}) ->
    [line(Line),
     expr(Call)];
expr({tail, Unknown}) ->
    io:format(user, "Unknown tail ~p~n", [Unknown]);
expr({record,Line,Name,ExprFields}) ->
    [line(Line),
     [parse_symbol(Line, '#'),
      {Line, a2b(Name), ?RECORD_NAME_COLOUR},
      parse_symbol(Line, '{'),
      map_separate(fun expr_field/1, ExprFields),
      parse_symbol(Line, '}')]];
expr({record_index,Line,Name,Field}) ->
    [line(Line),
     [parse_symbol(Line, '#'),
      {Line, a2b(Name), ?RECORD_NAME_COLOUR},
      parse_symbol(Line, '.'),
      expr(Field)]];
expr({record_field,Line,Expression,RecName,Field}) ->
    [line(Line),
     [expr(Expression),
      parse_symbol(Line, '#'),
      {Line, atom_to_list(RecName), ?RECORD_NAME_COLOUR},
      parse_symbol(Line, '.'),
      expr(Field)]];
% How does this happen? (Foo).bar ?
%expr({record_field,Line,Rec0,Field0}) ->
    %Rec1 = expr(Rec0),
    %Field1 = expr(Field0);
expr(UnknownExpr) ->
    io:format(user, "Unknown expr:~n~p~n", [UnknownExpr]),
    {-1, <<"error">>, ?RED}.


bit_type(Line, Atom) when is_atom(Atom) ->
    {Line, Atom, ?ATOM_COLOUR};
bit_type(Line, {Atom, Integer}) when is_atom(Atom), is_integer(Integer) ->
    [{Line, Atom, ?ATOM_COLOUR},
     parse_symbol(Line, ':'),
     {Line, Integer, ?INTEGER_COLOUR}].

expr_field({record_field,Lf,{atom,_La,F},Expr}) ->
    [line(Lf),
     span(F), '=', expr(Expr)];
expr_field({record_field,Lf,{var,_La,'_'},Expr}) ->
    [line(Lf),
     parse_symbol(Lf, '_'),
     '=', expr(Expr)].

guard_group(GuardGroup) ->
    separate(lists:map(fun expr/1, GuardGroup)).

fun_name({atom, Line, Name}) ->
    [line(Line),
     {Line, Name, ?TEXT_COLOUR}];
fun_name(_A = {remote, LineR, {atom, _LineM, Module}, {atom, LineF, Function}}) ->
     %io:format(user, "fun_name(~p)~n", [A]),
    [line(LineR),
     %line(LineM),
     {LineR, a2b(Module), ?TEXT_COLOUR},
     parse_symbol(LineR, ':'),
     %line(LineF),
     {LineF, a2b(Function), ?TEXT_COLOUR}];
fun_name(Any) ->
    io:format(user, "Unknown fun_name: ~p~n", [Any]),
    [].

parse_control_sequences(Line, String) when is_list(String) ->
    CtrlSeqs = [<<"~n">>,
                <<"~p">>,
                <<"~b">>,
                <<"~w">>],
    Parsed = lists:foldl(fun pcs/2, [list_to_binary(String)], CtrlSeqs),
    lists:map(fun(Bin) -> ctrl_seq_bin(Line, Bin) end, Parsed).

pcs(_, []) ->
    [];
% Where would a tuple come from?
pcs(CtrlSeq, [Tuple | Rest]) when is_tuple(Tuple) ->
    [Tuple | pcs(CtrlSeq, Rest)];
pcs(CtrlSeq, [Bin | Rest]) ->
    case string:split(Bin, CtrlSeq) of
        [_] ->
            [Bin | pcs(CtrlSeq, Rest)];
        [Before, After] ->
            lists:flatten([[Before, CtrlSeq | pcs(CtrlSeq, [After])] | pcs(CtrlSeq, Rest)])
    end.

ctrl_seq_bin(Line, CtrlSeq = <<$~, _>>) ->
    {Line, CtrlSeq, ?CONTROL_SEQUENCE_COLOUR};
ctrl_seq_bin(Line, String) ->
    {Line, String, ?TEXT_COLOUR}.

%% -type lc_bc_qual([Qualifier]) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

% No idea what ann_type is
type({ann_type,Line,[{var,Lv,V},T]}) ->
    T1 = type(T),
    {ann_type,Line,[{var,Lv,V},T1]};
type({atom,Line,A}) ->
    [line(Line), atom(Line, A)];
type({integer,Line,I}) ->
    [line(Line), integer(Line, I)];
type(Op = {op, _, _, _}) ->
    expr(Op);
type(Op = {op,_, _, _, _}) ->
    expr(Op);
type({type,Line,binary,[{_, _, 0},{_, _, 0}]}) ->
    [line(Line),
     parse_symbol(Line, '<<'),
     parse_symbol(Line, '>>')];
type({type,Line,binary,[{_, _, M}, {_, _, 0}]}) ->
    [line(Line),
     parse_symbol(Line, '<<'),
     parse_symbol(Line, '_'),
     parse_symbol(Line, ':'),
     {Line, i2b(M), ?INTEGER_COLOUR},
     parse_symbol(Line, '>>')];
type({type,Line,binary,[{_, _, 0},{_, _, N}]}) ->
    [line(Line),
     parse_symbol(Line, '<<'),
     parse_symbol(Line, '_'),
     parse_symbol(Line, ':'),
     parse_symbol(Line, '_'),
     parse_symbol(Line, '*'),
     {Line, i2b(N), ?INTEGER_COLOUR},
     parse_symbol(Line, '>>')];
type({type,Line,binary,[{_, _, M},{_, _, N}]}) ->
    [line(Line),
     parse_symbol(Line, '<<'),
     parse_symbol(Line, '_'),
     parse_symbol(Line, ':'),
     {Line, i2b(M), ?INTEGER_COLOUR},
     parse_symbol(Line, '_'),
     parse_symbol(Line, ':'),
     parse_symbol(Line, '_'),
     parse_symbol(Line, '*'),
     {Line, i2b(N), ?INTEGER_COLOUR},
     parse_symbol(Line, '>>')];
type({type,Line,'fun',[]}) ->
    [line(Line),
     parse_symbol(Line, 'fun'),
     parse_symbol(Line, '('),
     parse_symbol(Line, ')')];
type({type,Line,'fun',[{type,TypeLine,any},B]}) ->
    [line(Line),
     parse_symbol(Line, 'fun'),
     parse_symbol(Line, '('),
     line(TypeLine),
     parse_symbol(Line, '('),
     parse_symbol(Line, '...'),
     parse_symbol(Line, ')'),
     parse_symbol(Line, '->'),
     type(B), ')'];
type({type,Line,'fun',[{type,Lt,product, ArgTypes},TypeResult]}) ->
    [line(Line),
     parse_symbol(Line, 'fun'),
     parse_symbol(Line, '('),
     line(Lt), parse_symbol(Line, '('),
     lists:join(',', [type(T) || T <- ArgTypes]),
     parse_symbol(Line, ')'),
     parse_symbol(Line, '->'),
     type(TypeResult),
     ')'];
type({type,Line,nil,[]}) ->
    [line(Line), parse_symbol(Line, '['),
     ']'];
type({type,Line,range,[L,H]}) ->
    [line(Line),
     type(L),
     parse_symbol(Line, '..'),
     type(H)];
type({type,Line,map,any}) ->
    [line(Line),
     parse_symbol(Line, "#"),
     parse_symbol(Line, '{'),
     parse_symbol(Line, '}')];
type({type,Line,map,Ps}) ->
    Ps1 = map_pair_types(Ps),
    [line(Line),
     parse_symbol(Line, "#"),
     parse_symbol(Line, '{'),
     Ps1,
     parse_symbol(Line, '}')];
type({type,Line,record,[{atom,_La,N}|Fs]}) ->
    Fs1 = field_types(Fs),
    [line(Line),
     {Line, <<"#">>, ?RECORD_HASH},
     {Line, a2b(N), ?RECORD_NAME},
     parse_symbol(Line, '{'),
     Fs1,
     parse_symbol(Line, '}')];

%type({remote_type,Line,[{atom,Lm,M},{atom,Ln,N},As]}) ->
    %As1 = type_list(As),
    %{remote_type,Line,[{atom,Lm,M},{atom,Ln,N},As1]};
type({type,Line,tuple,any}) ->
    %{type,Line,tuple,any};
    [line(Line), 'tuple', parse_symbol(Line, '('),
     ')'];
type({type,Line,tuple,Ts}) ->
    [line(Line), '{', type_list(Ts), '}'];
type({type,Line,union,Ts}) ->
    [line(Line), lists:join('|', type_list(Ts))];
type({type,Line,list,Ts}) when is_list(Ts) ->
    [line(Line), parse_symbol(Line, '['),
     lists:join(',', type_list(Ts)), ']'];
type({type,Line,List,Ts}) when is_list(Ts) ->
    [line(Line),
     {Line, a2b(List), ?TEXT_COLOUR},
     parse_symbol(Line, '('),
     lists:join(parse_symbol(Line, ','), type_list(Ts)),
     parse_symbol(Line, ')')];
% TODO Figure out what V could be; I've only seen '_'
type({var, Line, _V}) ->
    [line(Line), parse_symbol(Line, '_')];
type({type,Line,Atom,[]}) when is_atom(Atom) ->
    [line(Line),
     {Line, Atom, ?TEXT_COLOUR},
     parse_symbol(Line, '('),
     parse_symbol(Line, ')')];
% TODO A record would fit this pattern, what else?
type(UnknownType = {type,_Line,_N,_As}) ->
    %As1 = type_list(As),
    %{type,Line,N,As1};
    io:format(user, "Unknown type: ~p~n", [UnknownType]);
type(UnknownType) ->
    io:format(user, "Unknown type: ~p~n", [UnknownType]).

map_pair_types(PairTypes) ->
    lists:join(',', [pair_type(PT) || PT <- PairTypes]).

pair_type({type, Line, map_field_assoc, [K, V]}) ->
    [line(Line), type(K), parse_symbol(Line, '=>'), type(V)];
pair_type({type, Line, map_field_exact, [K, V]}) ->
    [line(Line), type(K), ':=', type(V)].

field_types([{type,Line,field_type,[{atom,La,A},T]}|Fs]) ->
    L = line(Line),
    Atom = {La, a2b(A), ?ATOM_COLOUR},
    Symbol = parse_symbol(Line, '::'),
    T = type(T),
    [L, Atom, Symbol, T | field_types(Fs)];
field_types([]) -> [].

type_list([T|Ts]) ->
    T1 = type(T),
    [T1|type_list(Ts)];
type_list([]) -> [].

%% This is a list of generators _or_ filters
%% which are simply expressions
%% A generator is a target and a source
lc_bc_qual({generate,Line,Target,Source}) ->
    [parse_symbol(Line, '['),
     expr(Target),
     parse_symbol(Line, '<-'),
     expr(Source),
     parse_symbol(Line, ']')];
lc_bc_qual({b_generate,_Line,Target,Source}) ->
    [expr(Target), '<=', expr(Source)];
lc_bc_qual(FilterExpression) ->
    expr(FilterExpression).

bin({bin_element,Line,Var,Size,MaybeTypes}) ->
    [line(Line),
     expr(Var),
     case Size of
         default ->
             "";
         Int ->
             [parse_symbol(Line, ':'),
              {Line, i2b(Int), ?TEXT_COLOUR}]
     end,
     case MaybeTypes of
         default ->
             "";
         _ ->
             [parse_symbol(Line, '/'),
             Fun = fun(MaybeType) ->
                       bit_type(Line, MaybeType)
                   end,
             map_separate('-', Fun, MaybeTypes)]
     end].

%bin_element({bin_element,Line,Expression,Size1,Type1}) ->
    %Size2 = case Size1 of
		 %default ->
		 %"";
		 %_ ->
		 %[':', expr(Size1)]
	 %end,
    %Type2 = case Type1 of
		 %default ->
		 %"";
		 %_ ->
		 %['/', separate('-', lists:map(fun bit_type/1, Type1))]
	 %end,
    %[line(Line),
     %expr(Expression), Size2, Type2].

var(Line, Atom) ->
    {Line, a2b(Atom), ?VARIABLE_COLOUR}.

atom(Line, Atom) ->
    {Line, a2b(Atom), ?ATOM_COLOUR}.

integer(Line, Int) ->
    {Line, i2b(Int), ?INTEGER_COLOUR}.

module(Line, Mod) ->
    {Line, a2b(Mod), ?MODULE_LITERAL_COLOUR}.

function(Line, Fun) ->
    {Line, a2b(Fun), ?TEXT_COLOUR}.

arity(Line, Arity) ->
    {Line, i2b(Arity), ?ARITY_COLOUR}.

i2b(I) ->
    list_to_binary(integer_to_list(I)).

a2b(A) ->
    list_to_binary(atom_to_list(A)).

parse_symbol(Line, '(') ->
    {Line, <<"(">>, ?TEXT_COLOUR};
parse_symbol(Line, ')') ->
    {Line, <<")">>, ?TEXT_COLOUR};
parse_symbol(Line, '{') ->
    {Line, <<"{">>, ?BRACE_COLOUR};
parse_symbol(Line, '}') ->
    {Line, <<"}">>, ?BRACE_COLOUR};
parse_symbol(Line, '[') ->
    {Line, <<"[">>, ?BRACKET_COLOUR};
parse_symbol(Line, ']') ->
    {Line, <<"]">>, ?BRACKET_COLOUR};
parse_symbol(Line, '.') ->
    {Line, <<".">>, ?TEXT_COLOUR};
parse_symbol(Line, ',') ->
    {Line, <<",">>, ?TEXT_COLOUR};
parse_symbol(Line, ':') ->
    {Line, <<":">>, ?TEXT_COLOUR};
parse_symbol(Line, ';') ->
    {Line, <<";">>, ?TEXT_COLOUR};
parse_symbol(Line, '/') ->
    {Line, <<"/">>, ?SLASH_COLOUR};
parse_symbol(Line, '-') ->
    {Line, <<"-">>, ?DASH_COLOUR};
parse_symbol(Line, '=') ->
    {Line, <<"=">>, ?DASH_COLOUR};
parse_symbol(Line, '>') ->
    {Line, <<">">>, ?TEXT_COLOUR};
parse_symbol(Line, '<') ->
    {Line, <<"<">>, ?TEXT_COLOUR};
parse_symbol(Line, '==') ->
    {Line, <<"==">>, ?TEXT_COLOUR};
parse_symbol(Line, '+') ->
    {Line, <<"+">>, ?TEXT_COLOUR};
parse_symbol(Line, '*') ->
    {Line, <<"*">>, ?STAR_COLOUR};
parse_symbol(Line, '->') ->
    {Line, <<"->">>, ?CLAUSE_ARROW_COLOUR};
parse_symbol(Line, '<-') ->
    {Line, <<"<-">>, ?GENERATOR_ARROW_COLOUR};
parse_symbol(Line, '"') ->
    {Line, <<"\"">>, ?DOUBLE_QUOTE_COLOUR};
parse_symbol(Line, '%') ->
    {Line, <<"%">>, ?COMMENT_COLOUR};
parse_symbol(Line, '::') ->
    {Line, <<"::">>, ?TEXT_COLOUR};
parse_symbol(Line, '<<') ->
    {Line, <<"<<">>, ?BINARY_OPEN_COLOUR};
parse_symbol(Line, '>>') ->
    {Line, <<">>">>, ?BINARY_CLOSE_COLOUR};
parse_symbol(Line, '...') ->
    {Line, <<"...">>, ?TEXT_COLOUR};
parse_symbol(Line, '..') ->
    {Line, <<"..">>, ?TEXT_COLOUR};
parse_symbol(Line, '_') ->
    {Line, <<"_">>, ?TEXT_COLOUR};
parse_symbol(Line, '#') ->
    {Line, <<"#">>, ?TEXT_COLOUR};
parse_symbol(Line, '|') ->
    {Line, <<"|">>, ?CONS_COLOUR};
parse_symbol(Line, '=>') ->
    {Line, <<"=>">>, ?MAP_FIELD_ASSOC_COLOUR};
parse_symbol(Line, ':=') ->
    {Line, <<":=">>, ?MAP_FIELD_EXTRACT_COLOUR};
parse_symbol(Line, Line = {line, _}) ->
    {Line, i2b(Line), ?LINE_COLOUR};
parse_symbol(Line, eof) ->
    {Line, <<"eof">>, ?EOF_COLOUR};
parse_symbol(Line, module) ->
    {Line, <<"module">>, ?MODULE_COLOUR};
parse_symbol(Line, 'fun') ->
    {Line, <<"fun">>, ?FUN_COLOUR};

parse_symbol(Line, Atom) ->
    {Line, a2b(Atom), ?ATOM_COLOUR}.

line(Line) ->
    {Line, i2b(Line), ?LINE_COLOUR}.

separate(List) when is_list(List) ->
    separate(',', List).

separate(Separator, List) ->
    lists:join(Separator, List).

map_separate(Fun, List) ->
    map_separate(',', Fun, List).

map_separate(Separator, Fun, List) ->
    separate(Separator, lists:map(Fun, List)).