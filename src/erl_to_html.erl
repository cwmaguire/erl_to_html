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
-module(erl_to_html).

-export([write_html/1]).
-export([parse_transform/2]).
-export([escape/1]).
-export([parse_control_sequences/1]).
-export([pcs/2]).

write_html(Filename) ->
    io:format(user, "Compiling ~p~n", [Filename]),
    Result = compile:file(Filename, [report_errors,
                                     return_errors,
                                     {parse_transform, ?MODULE},
                                     {d, filename, Filename}]),
    io:format(user, "Compile result:~n~p~n", [Result]).

parse_transform(Forms, Options) ->
    Filename = filename(Options),
    io:format(user, "Scanning ~p for indentation~n", [Filename]),
    Indents = get_indents:indents(Filename),
    io:format(user, "Scanning ~p for comments~n", [Filename]),
    Comments = get_comments:comments(Filename),

    io:format(user, "Parse transforming forms: ~n"
                    "\t~p~n"
                    "\t with Options:~n"
                    "\t~p~n",
              [Forms, Options]),

    HtmlFilename = html_filename(Filename),
    io:format(user, "Filename: ~p~n", [HtmlFilename]),
    HTML = parse(lines(html(Forms), Indents, Comments)),
    %io:format(user, "HTML = ~p~n", [HTML]),
    {ok, HtmlFile} = file:open(HtmlFilename, [write]),
    case file:write(HtmlFile, [HTML, <<"\n">>]) of
        ok ->
            io:format(user, "Write successful~n", []);
        Error ->
            io:format(user, "Write failed: ~p~n", [Error])
    end,
    Forms.

filename(Options) ->
    case [Filename || {d, filename, Filename} <- Options] of
        [] ->
            "out.html";
        [Filename | _] ->
            Filename % test comment
    end.

html_filename(Filename) ->
    filename:rootname(Filename) ++ ".html".

%% walk the tree and wrap lines in spans
lines(Tree, Indents, Comments) ->
    {Tree2, _} = lines([], Tree, 0, Indents, Comments),
    StartLine = [{no_parse, <<"<span class=\"line\">">>},
                 {no_parse, <<"<span class=\"line_no\">">>},
                 {no_parse, <<"0">>},
                 html_spaces(maps:get(1, Indents, 0)),
                 {no_parse, <<"</span><!--\n">>}],
    [StartLine, Tree2, {no_parse, <<"</span>">>}].

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
                <<>>;
            Comment ->
                [{no_parse, <<"--><span class=\"comment\">">>},
                 parse_comment(Comment),
                 {no_parse, <<"</span><!--\n">>}]
        end,
    StartLine = [EndOfLine,
                 _EndPrevLine = {no_parse, <<"--></span>">>},
                 {no_parse, <<"<span class=\"line\">">>},
                 {no_parse, <<"<span class=\"line_no\">">>},
                 i2b(Line + 1),
                 {no_parse, <<"</span>">>},
                 {no_parse, <<"<!--\n">>},
                 html_spaces(maps:get(NewLine, Indents, 0))],
    lines([StartLine | Tree],
          [{line, NewLine} | Rest],
          Line + 1,
          Indents,
          Comments);
lines(Tree, [Head | Rest], Line, Indents, Comments) ->
    lines([Head | Tree], Rest, Line, Indents, Comments).

html_spaces(NumSpaces) ->
    [span({no_parse, <<"space">>}, {no_parse, <<"&nbsp;">>}) || _ <- lists:seq(1, NumSpaces)].

parse_comment(Comment) ->
    parse_comment([], Comment).

parse_comment(Comment, []) ->
    lists:reverse(Comment);
parse_comment(Comment, "TODO" ++ Rest) ->
    parse_comment([comment_keyword("TODO") | Comment], Rest);
parse_comment(Comment, "NOTE" ++ Rest) ->
    parse_comment([comment_keyword("NOTE") | Comment], Rest);
parse_comment(Comment, "FIXME" ++ Rest) ->
    parse_comment([comment_keyword("FIXME") | Comment], Rest);
parse_comment(Comment,  [Head | Rest]) ->
    parse_comment([[Head] | Comment], Rest).

comment_keyword(Keyword) ->
    [{no_parse,
      list_to_binary(["<span class=\"",
                      string:to_lower(Keyword),
                      "\">"])},
     Keyword,
     {no_parse, <<"</span>">>}].


%% No raw numbers should show up in the HTML
parse(String = [I | _]) when is_number(I) ->
    escape(String);
parse(Binary) when is_binary(Binary) ->
    escape(Binary);
parse(List) when is_list(List) ->
    lists:map(fun parse/1, List);
parse({no_parse, Binary}) when is_binary(Binary) ->
    Binary;
parse({no_parse, NotBinary}) ->
    NotBinary;
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

html(Forms) when is_list(Forms) ->
    [html(Form) || Form <- Forms];

%% -type form(Form) -> Form.
%%  Here we show every known form and valid internal structure. We do not
%%  that the ordering is correct!

%% First the various attributes.
html({attribute,Line,module,Mod}) ->
    [line(Line),
     '-', 'module', '(', module(Mod), ')', '.'];
html({attribute,_AttributeLine,file,{File,_FileLine}}) ->	%This is valid anywhere.
    %% Manually set this to 0 to come before any source lines
    [lit_line(0),
     '%', '-', 'file', '(', span(<<"file-literal">>, File), ')', '.'];
html({attribute,Line,export,Es0}) ->
    [line(Line),
     '-', span("export"), '[', farity_list(Es0), ']', '.'];
html({attribute,Line,import,{Mod,Is0}}) ->
    [line(Line),
     '-', 'import', ',', module(Mod), '(', '[', farity_list(Is0), ']', ')', '.'];
html({attribute,Line,compile,C}) ->
    [line(Line),
     '-', 'compile', '(', span(<<"compile-option">>, atom_to_list(C)), ')', '.'];
html({attribute,Line,record,{Name,Defs}}) ->
    [line(Line),
     '-', 'record', '(', span("atom", atom_to_list(Name)), ',', '{',
     record_defs(Defs),
     '}', ')', '.'];
html({attribute,Line,asm,{function,_N,_A,_Code}}) ->
    line(Line);
html({attribute,Line,_Attr,_Val}) ->		%The general attribute.
    line(Line);
html({function,Line,Name,_Arity,Clauses}) ->
    [line(Line),
     separate(';', lists:map(fun(Clause) -> clause(Name, Clause) end, Clauses)), '.'];
% TODO figure out how to reproduce these
html({error,E}) ->
    {error,E};
html({warning,W}) ->
    {warning,W};
html({eof,Line}) ->
    [get_location_line(Line), span(<<"eof">>)].

farity_list(FunArities) ->
    separate(lists:map(fun farity/1, FunArities)).

farity({Name, Arity}) ->
    [function(Name), '/', arity(Arity)].

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
    RecordLine = get_line(element(2, RecordDef)),
    TypeLine = get_line(element(2, RecordType)),
    Line = case TypeLine - RecordLine of
        X when X > 1 ->
            RecordLine + 1;
        _ ->
            RecordLine
    end,
    [lit_line(Line), '::'].

catch_clause({clause, Anno, Exception, GuardGroups, Body}) ->
    [{tuple, _Anno, [Class, ExceptionPattern, _Wild]}] = Exception,
    [line(Anno),
     expr(Class), ':', expr(ExceptionPattern),
     separate(';', lists:map(fun guard_group/1, GuardGroups)),
     '->',
     separate(lists:map(fun expr/1, Body))].

clause({clause, Anno, Head, GuardGroups, Body}) ->
    clause('', {clause, Anno, Head, GuardGroups, Body}).

clause(Name, {clause,Anno,Head,GuardGroups,Body}) ->
    [line(Anno),
     span(<<"function">>, atom_to_list(Name)),
     head(Head),
     separate(';', lists:map(fun guard_group/1, GuardGroups)),
     '->',
     separate(lists:map(fun expr/1, Body))].

case_clause({clause, Anno, [Head], GuardGroups, Body}) ->
    [line(Anno),
     expr(Head),
     case GuardGroups of
         [] ->
             [];
         _ ->
             F = fun guard_group/1,
             ['when',
              separate(';', lists:map(F, GuardGroups))]
     end,
     '->',
     separate(lists:map(fun expr/1, Body))].

head(Expressions) ->
    ['(', separate(lists:map(fun expr/1, Expressions)), ')'].

%% -type pattern(Pattern) -> Pattern.
%%  N.B. Only valid patterns are included here.
expr({lc,Anno,Result,Quals}) ->
    E1 = expr(Result),
    [line(Anno),
     '[', E1, '||', map_separate(fun lc_bc_qual/1, Quals), ']'];
expr({bc,Anno,E0,Quals}) ->
    E1 = expr(E0),
    [line(Anno),
     '<<', E1, '||', map_separate(fun lc_bc_qual/1, Quals), '>>'];
expr({block,Anno,Expressions}) ->
    [line(Anno),
     'begin', map_separate(fun expr/1, Expressions), 'end'];
expr({'if',Anno,Clauses}) ->
    [line(Anno),
     'if',
     map_separate(fun clause/1, Clauses),
     'end'];
expr({'case',Anno,Expression,Clauses}) ->
    [line(Anno),
     'case', expr(Expression), 'of',
     map_separate(';', fun case_clause/1, Clauses),
     'end'];
expr({'receive',Anno,Clauses}) ->
    [line(Anno),
     'receive', map_separate(fun clause/1, Clauses),
     'end'];
expr({'receive',Anno,Clauses,AfterWait,AfterExpressions}) ->
    [line(Anno),
     'receive', map_separate(fun clause/1, Clauses),
     'after', expr(AfterWait), '->',
     map_separate(fun expr/1, AfterExpressions),
     'end'];
expr({'try',Anno,Expressions,_WhatIsThis,CatchClauses,AfterExpressions}) ->
    [line(Anno),
     'try', map_separate(fun expr/1, Expressions),
     'catch', map_separate(';', fun catch_clause/1, CatchClauses),
     'after', map_separate(fun expr/1, AfterExpressions),
     'end'];
expr({'fun',Anno,Body}) ->
    case Body of
        {clauses,Clauses} ->
            [line(Anno),
             'fun',
             map_separate(fun(Clause) -> clause('', Clause) end, Clauses)];
        {function,Fun,Arity} ->
            [line(Anno),
             function(Fun), '/', arity(Arity)];
        {function,M,F,A} when is_atom(M), is_atom(F), is_integer(A) ->
            [line(Anno),
             span(<<"module">>, a2b(M)), ':', function(F), '/', arity(A)];
        {function,M0,F0,A0} ->
            %% R15: fun M:F/A with variables.
            M = expr(M0),
            F = expr(F0),
            A = expr(A0),
            [line(Anno),
             span(<<"module">>, a2b(M)),
             ':', span("function", F),
             '/', span("arity", A)]
    end;
expr({call,Anno,Fun,Args}) ->
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
     %io:format(user, "calling expr(~p)~n", [Fun]),
    [line(Anno),
     %fun_name(Fun), '(', map_separate(fun expr/1, Args), ')'];
     fun_name(Fun), '(', map_separate(fun expr/1, Args), ')'];
expr({'catch',Anno,Expression}) ->
    %% No new variables added.
    [line(Anno),
     'catch', expr(Expression)];
expr({match,Anno,Expr1,Expr2}) ->
    [line(Anno),
     expr(Expr1), '=', expr(Expr2)];
expr({bin,Anno,BinElements}) ->
    [line(Anno),
     '<<', map_separate(fun bin/1, BinElements), '>>'];
expr({op,Anno,Op,A}) ->
    [line(Anno),
     span("operator", atom_to_list(Op)), expr(A)];
expr({op,Anno,'==',L,R}) ->
    [line(Anno),
     expr(L), span(<<"double_equals">>, "=="), expr(R)];
expr({op,Anno,Op,L,R}) ->
    [line(Anno),
     expr(L), span("operator", atom_to_list(Op)), expr(R)];
expr({remote, Anno, {atom, _MAnno, Module}, {atom, _FAnno, Function}}) ->
    [line(Anno),
     span(<<"module">>, a2b(Module)), ':', function(Function)];
expr({nil, Anno}) ->
    [line(Anno), '[', ']'];
expr({var,Anno,V}) ->
    [line(Anno),
     var(V)];
expr({integer,Anno,I}) ->
    [line(Anno),
     integer(I)];
expr({char,Anno,C}) ->
    [line(Anno),
     span("char", [$$, C])];
expr({float,Anno,F}) ->
    [line(Anno),
     span("float", io_lib:format("~w", [F]))];
expr({atom,Anno,A}) when A == ':';
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
    io:format(user, "{atom, Anno, ~p}", [A]),
    Str = [$', atom_to_list(A), $'],
    [line(Anno),
     span("atom", list_to_binary(Str))];
expr({atom,Anno,A}) ->
[line(Anno),
     span("atom", atom_to_list(A))];
expr({string,Anno,S}) ->
    [line(Anno),
     '"',
     %{no_parse, <<"--><!-- before parse_control_sequences(S) --><!--\n">>},
     parse_control_sequences(S),
     %{no_parse, <<"--><!-- after parse_control_sequences(S) --><!--\n">>},
     '"'];
expr({tuple,Anno,Exprs}) ->
    [line(Anno),
     ['{', map_separate(fun expr/1, Exprs), '}']];
%% There's a special case for all cons's after the first: {tail, _}
%% so this is a list of one item.
expr({cons,Anno,Head,{nil, _}}) ->
    [line(Anno),
     '[', expr(Head), ']'];
expr({cons,Anno,Head,{var, _Anno2, '_'}}) ->
    [line(Anno),
     '[', expr(Head), '|', '_', ']'];
expr(_Cons = {cons,Anno,Head,Tail}) ->
    %io:format(user, "Cons -> Tail = ~p~n", [Cons]),
    [line(Anno),
     '[', [expr(Head), ',' | expr({tail, Tail})], ']'];
expr(_Tail = {tail, {cons, Anno, Head, {nil, _}}}) ->
    %io:format(user, "Tail 1 = ~p~n", [Tail]),

    [line(Anno),
     [expr(Head)]];
expr(_Tail_ = {tail, {cons, Anno, Head, Tail}}) ->
    %io:format(user, "Tail 2 = ~p~n", [Tail_]),
    [line(Anno),
     [expr(Head), ',' | expr({tail, Tail})]];
expr({tail, {var, Anno, Expr}}) ->
    [line(Anno),
     var(Expr)];
expr({tail, Call = {call, Anno, _Fun, _Args}}) ->
    [line(Anno),
     expr(Call)];
expr({tail, Unknown}) ->
    io:format(user, "Unknown tail ~p~n", [Unknown]);
expr({record,Anno,Name,ExprFields}) ->
    [line(Anno),
     [span("record_hash", {no_parse, <<"#">>}), span("record_name", atom_to_list(Name)), '{',
      map_separate(fun expr_field/1, ExprFields),
      '}']];
expr({record_index,Anno,Name,Field}) ->
    [line(Anno),
     [span("record", ['#', atom_to_list(Name)]), '.', expr(Field)]];
expr({record_field,Anno,Expression,RecName,Field}) ->
    [line(Anno),
     [expr(Expression), '#', span("record", atom_to_list(RecName)), '.', expr(Field)]];
% How does this happen? (Foo).bar ?
%expr({record_field,Anno,Rec0,Field0}) ->
    %Rec1 = expr(Rec0),
    %Field1 = expr(Field0);
expr(UnknownExpr) ->
    io:format(user, "Unknown expr:~n~p~n", [UnknownExpr]),
    span("error").


bit_type(Atom) when is_atom(Atom) ->
    span(Atom);
bit_type({Atom, Integer}) when is_atom(Atom), is_integer(Integer) ->
    [atom(Atom), ':', integer(Integer)].

expr_field({record_field,Lf,{atom,_La,F},Expr}) ->
    [line(Lf),
     span(F), '=', expr(Expr)];
expr_field({record_field,Lf,{var,_La,'_'},Expr}) ->
    [line(Lf),
     '_', '=', expr(Expr)].

guard_group(GuardGroup) ->
    separate(lists:map(fun expr/1, GuardGroup)).

fun_name({atom, _Anno, Name}) ->
    [%line(Anno),
     span(<<"fun_call">>, Name)];
fun_name(_A = {remote, _AnnoR, {atom, _AnnoM, Module}, {atom, _AnnoF, Function}}) ->
     %io:format(user, "fun_name(~p)~n", [A]),
    [%line(AnnoR),
     %line(AnnoM),
     span(<<"remote_fun_module">>, a2b(Module)),
     ':',
     %line(AnnoF),
     span(<<"fun_call">>, a2b(Function))];
fun_name(Any) ->
    io:format(user, "Unknown fun_name: ~p~n", [Any]),
    [].

parse_control_sequences(String) when is_list(String) ->
    CtrlSeqs = [<<"~n">>,
                <<"~p">>,
                <<"~b">>,
                <<"~w">>],
    Parsed = lists:foldl(fun pcs/2, [list_to_binary(String)], CtrlSeqs),
    lists:map(fun span_bins/1, Parsed).

% TODO: how would the 2nd argument every have a tuple in it?
pcs(_, []) ->
    [];
pcs(CtrlSeq, [Tuple | Rest]) when is_tuple(Tuple) ->
    [Tuple | pcs(CtrlSeq, Rest)];
pcs(CtrlSeq, [Bin | Rest]) ->
    case string:split(Bin, CtrlSeq) of
        [_] ->
            [Bin | pcs(CtrlSeq, Rest)];
        [Before, After] ->
            lists:flatten([[Before, CtrlSeq | pcs(CtrlSeq, [After])] | pcs(CtrlSeq, Rest)])
    end.

span_bins(CtrlSeq = <<$~, _>>) ->
    span(<<"control_sequence">>, CtrlSeq);
span_bins(String) ->
    span(<<"string">>, String).

%% -type lc_bc_qual([Qualifier]) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

% No idea what ann_type is
type({ann_type,Anno,[{var,Lv,V},T]}) ->
    T1 = type(T),
    {ann_type,Anno,[{var,Lv,V},T1]};
type({atom,Anno,A}) ->
    [line(Anno), atom(A)];
type({integer,Anno,I}) ->
    [line(Anno), integer(I)];
type(Op = {op, _, _, _}) ->
    expr(Op);
type(Op = {op,_, _, _, _}) ->
    expr(Op);
type({type,Anno,binary,[{_, _, 0},{_, _, 0}]}) ->
    [line(Anno), '<<', '>>'];
type({type,Anno,binary,[{_, _, M}, {_, _, 0}]}) ->
    [line(Anno),
     '<<', '_', ':', span(<<"integer">>, i2b(M)), '>>'];
type({type,Anno,binary,[{_, _, 0},{_, _, N}]}) ->
    [line(Anno),
     '<<', '_', ':', '_', '*', span(<<"integer">>, i2b(N)), '>>'];
type({type,Anno,binary,[{_, _, M},{_, _, N}]}) ->
    [line(Anno),
     '<<', '_', ':', span(<<"integer">>, i2b(M)), ',',
     '_', ':', '_', '*', span(<<"integer">>, i2b(N)), '>>'];
type({type,Anno,'fun',[]}) ->
    [line(Anno), span(<<"fun">>), '(', ')'];
type({type,Anno,'fun',[{type,Lt,any},B]}) ->
    [line(Anno), span(<<"fun">>), '(',
     line(Lt), '(', '...', ')', '->', type(B), ')'];
type({type,Anno,'fun',[{type,Lt,product, ArgTypes},TypeResult]}) ->
    [line(Anno),
     span("fun"),
     '(',
     line(Lt), '(',
     lists:join(',', [type(T) || T <- ArgTypes]),
     ')', '->', type(TypeResult),
     ')'];
type({type,Anno,nil,[]}) ->
    [line(Anno), '[', ']'];
type({type,Anno,range,[L,H]}) ->
    [line(Anno), type(L), '..', type(H)];
type({type,Anno,map,any}) ->
    [line(Anno), span(<<"map_hash">>, "#"), '{', '}'];
type({type,Anno,map,Ps}) ->
    Ps1 = map_pair_types(Ps),
    [line(Anno), span(<<"map_hash">>, "#"), '{', Ps1, '}'];
type({type,Anno,record,[{atom,_La,N}|Fs]}) ->
    Fs1 = field_types(Fs),
    [line(Anno),
     span(<<"record_hash">>, <<"#">>), span(<<"record_name">>, a2b(N)), '{', Fs1, '}'];

%type({remote_type,Anno,[{atom,Lm,M},{atom,Ln,N},As]}) ->
    %As1 = type_list(As),
    %{remote_type,Anno,[{atom,Lm,M},{atom,Ln,N},As1]};
type({type,Anno,tuple,any}) ->
    %{type,Anno,tuple,any};
    [line(Anno), 'tuple', '(', ')'];
type({type,Anno,tuple,Ts}) ->
    [line(Anno), '{', type_list(Ts), '}'];
type({type,Anno,union,Ts}) ->
    [line(Anno), lists:join('|', type_list(Ts))];
type({type,Anno,list,Ts}) when is_list(Ts) ->
    [line(Anno), '[', lists:join(',', type_list(Ts)), ']'];
type({type,Anno,List,Ts}) when is_list(Ts) ->
    [line(Anno), span(a2b(List)), '(', lists:join(',', type_list(Ts)), ')'];
% TODO Figure out what V could be; I've only seen '_'
type({var, Anno, _V}) ->
    [line(Anno), '_'];
%type({user_type,Anno,N,As}) ->
    %As1 = type_list(As),
    %{user_type,Anno,N,As1};
type({type,Anno,Atom,[]}) when is_atom(Atom) ->
    [line(Anno), Atom, '(', ')'];
% TODO A record would fit this pattern, what else?
type(UnknownType = {type,_Anno,_N,_As}) ->
    %As1 = type_list(As),
    %{type,Anno,N,As1};
    io:format(user, "Unknown type: ~p~n", [UnknownType]);
type(UnknownType) ->
    io:format(user, "Unknown type: ~p~n", [UnknownType]).

map_pair_types(PairTypes) ->
    lists:join(',', [pair_type(PT) || PT <- PairTypes]).

pair_type({type, Anno, map_field_assoc, [K, V]}) ->
    [line(Anno), type(K), '=>', type(V)];
pair_type({type, Anno, map_field_exact, [K, V]}) ->
    [line(Anno), type(K), ':=', type(V)].

field_types([{type,Anno,field_type,[{atom,_La,A},T]}|Fs]) ->
    [line(Anno), A, '::', type(T) | field_types(Fs)];
field_types([]) -> [].

type_list([T|Ts]) ->
    T1 = type(T),
    [T1|type_list(Ts)];
type_list([]) -> [].

%% This is a list of generators _or_ filters
%% which are simply expressions
%% A generator is a target and a source
lc_bc_qual({generate,_Anno,Target,Source}) ->
    ['[', expr(Target), '<-', expr(Source), ']'];
lc_bc_qual({b_generate,_Anno,Target,Source}) ->
    [expr(Target), '<=', expr(Source)];
lc_bc_qual(FilterExpression) ->
    expr(FilterExpression).

bin({bin_element,Anno,Var,Size,MaybeTypes}) ->
    [line(Anno),
     expr(Var),
     case Size of
         default ->
             "";
         {integer, _, Int} ->
             [':', integer(Int)]
     end,
     case MaybeTypes of
         default ->
             "";
         _ ->
             ['/', map_separate('-', fun bit_type/1, MaybeTypes)]
     end].

%bin_element({bin_element,Anno,Expression,Size1,Type1}) ->
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
    %[line(Anno),
     %expr(Expression), Size2, Type2].

var(Atom) ->
    span(<<"variable">>, a2b(Atom)).

atom(Atom) ->
    span(<<"atom">>, a2b(Atom)).

integer(Int) ->
    span(<<"integer">>, i2b(Int)).

module(Mod) ->
    span(<<"module-literal">>, a2b(Mod)).

function(Fun) ->
    span(<<"function">>, a2b(Fun)).

arity(Arity) ->
    span(<<"arity">>, i2b(Arity)).

i2b(I) ->
    list_to_binary(integer_to_list(I)).

a2b(A) ->
    list_to_binary(atom_to_list(A)).

span(Keyword) ->
    span(Keyword, Keyword).

span(Class, Atom) when is_atom(Atom) ->
    span(Class, a2b(Atom));
span(_Class = <<"line">>, Line) ->
    {line, Line};
span(Class = <<"tuple">>, Text) ->
     [{no_parse, <<"--><span class=\"">>},
      Class,
      {no_parse, <<"\"><!--\n">>},
      Text,
      {no_parse, <<"--></span><!--\n">>}];
span(Class, Text) ->
     [{no_parse, <<"--><span class=\"">>},
      Class,
      {no_parse, <<"\">">>},
      Text,
      {no_parse, <<"</span><!--\n">>}].

%span_open(Class) ->
    %["<span class=\"",
     %Class,
     %"</span>"].

parse_symbol('(') ->
    span(<<"paren">>, <<"(">>);
parse_symbol(')') ->
    span(<<"paren">>, <<")">>);
parse_symbol('{') ->
    span(<<"brace">>, <<"{">>);
parse_symbol('}') ->
    span(<<"brace">>, <<"}">>);
parse_symbol('[') ->
    span(<<"bracket">>, <<"[">>);
parse_symbol(']') ->
    span(<<"bracket">>, <<"]">>);
parse_symbol('.') ->
    span(<<"period">>, <<".">>);
parse_symbol(',') ->
    span(<<"comma">>, <<",">>);
parse_symbol(':') ->
    span(<<"colon">>, <<":">>);
parse_symbol(';') ->
    span(<<"semicolon">>, ";");
parse_symbol('/') ->
    span(<<"slash">>, <<"/">>);
parse_symbol('-') ->
    span(<<"dash">>, <<"-">>);
parse_symbol('=') ->
    span(<<"equals">>, <<"=">>);
parse_symbol('>') ->
    span(<<"greater_than">>, <<"&gt;">>);
parse_symbol('<') ->
    span(<<"less_than">>, <<"&lt;">>);
parse_symbol('==') ->
    span(<<"double_equals">>, <<"==">>);
parse_symbol('+') ->
    span(<<"plus">>, <<"+">>);
parse_symbol('*') ->
    span(<<"star">>, <<"*">>);
parse_symbol('->') ->
    span(<<"clause_arrow">>, <<"-&gt;">>);
parse_symbol('<-') ->
    span(<<"generator_arrow">>, <<"&lt;-">>);
parse_symbol('"') ->
    span(<<"double_quote">>, <<"&quot;">>);
parse_symbol('%') ->
    span(<<"comment">>, <<"%">>);
parse_symbol('::') ->
    span(<<"record_type_op">>, <<"::">>);
parse_symbol('<<') ->
    span(<<"binary_open">>, <<"&lt;&lt;">>);
parse_symbol('>>') ->
    span(<<"binary_close">>, <<"&gt;&gt;">>);
parse_symbol('...') ->
    span(<<"any_elipsis">>, <<"...">>);
parse_symbol('..') ->
    span(<<"range">>, <<"..">>);
parse_symbol('_') ->
    span(<<"var">>, <<"_">>);
parse_symbol('#') ->
    span(<<"hash">>, <<"#">>);
parse_symbol('|') ->
    span(<<"cons">>, <<"|">>);
parse_symbol('=>') ->
    span(<<"map_field_assoc">>, <<"=&gt;">>);
parse_symbol(':=') ->
    span(<<"map_field_exact">>, <<":=">>);
parse_symbol(Line = {line, _}) ->
    Line;
parse_symbol(eof) ->
    span(<<"eof">>);
parse_symbol(Atom) ->
    span(atom_to_list(Atom)).

line(Anno) ->
    Line = get_line(Anno),
    lit_line(Line).

lit_line(Line) ->
    {line, Line}.

get_line(Anno) ->
    erl_anno:line(Anno).

get_location_line(Line) when is_integer(Line) ->
    Line;
get_location_line({Line, Column}) when is_integer(Line), is_integer(Column) ->
    Line.

separate(List) when is_list(List) ->
    separate(',', List).

separate(Separator, List) ->
    lists:join(Separator, List).

map_separate(Fun, List) ->
    map_separate(',', Fun, List).

map_separate(Separator, Fun, List) ->
    separate(Separator, lists:map(Fun, List)).
