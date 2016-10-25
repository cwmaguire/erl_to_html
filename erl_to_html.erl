%% ``The contents of this file are subject to the Erlang Public License,
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
%%     $Id$
%%
-module(erl_to_html).

%% A identity transformer of Erlang abstract syntax.

%% This module only traverses legal Erlang code. This is most noticeable
%% in guards where only a limited number of expressions are allowed.
%% N.B. if this module is to be used as a basis for tranforms then
%% all the error cases must be handled otherwise this module just crashes!

%% I'd like to try creating the HTML without having to thread the file and separators
%% through the whole file.
%%
%% I think I can just pass back the origin Forms as-is.
%% This means that I don't need any of the functions to return the
%% origin {foo, Bar, Baz} form, they can simply return HTML io_lists.

-export([write_html/1]).
-export([parse_transform/2]).

-define(PAREN_OPEN, span("paren", ")")).
-define(PAREN_CLOSE, span("paren", ")")).
-define(BRACE_OPEN, span("brace", "{")).
-define(BRACE_CLOSE, span("brace", "}")).
-define(BRACKET_OPEN, span("bracket", "[")).
-define(BRACKET_CLOSE, span("bracket", "]")).
-define(PERIOD, span("period", ",")).
-define(SEMICOLON, span("semicolon", ";")).
-define(COMMA, span("comma", ",")).
-define(SLASH, span("slash", "/")).
-define(DASH, span("dash", "-")).
-define(EQUALS, span("dash", "-")).
-define(LINE_SPAN, span("line", integer_to_list(Line))).
-define(SPAN_CLOSE, "</span>").
-define(NO_SEPARATOR, "").

write_html(Filename) ->
    io:format("Compiling ~p~n", [Filename]),
    compile:file(Filename, [{parse_transform, ?MODULE}, {d, filename, Filename}]).

parse_transform(Forms, Options) ->
    io:format("Parse transforming forms: ~n\t~p~n\t with Options:~n\t~p~n",
              [Forms, Options]),
    Filename = filename(Options),
    io:format("Filename: ~p~n", [Filename]),
    HTML = parse(html(Forms)),
io:format(user, "HTML = ~p~n", [HTML]),
    {ok, HtmlFile} = file:open(Filename, [write]),
    file:write(HtmlFile, HTML),
    Forms.

filename(Options) ->
    case [Filename || {d, filename, Filename} <- Options] of
        [] ->
            "out.html";
        [Filename | _] ->
            HtmlFileName = filename:rootname(Filename) ++ ".html",
            io:format("HTML file: ~p~n", [HtmlFileName]),
            HtmlFileName
    end.

%% No raw numbers should show up in the HTML
parse(String = [I | _]) when is_number(I) ->
    String;
parse(List) when is_list(List) ->
    lists:map(fun parse/1, List);
parse(Term) when is_atom(Term); is_tuple(Term) ->
    parse_symbol(Term);
parse(Other) ->
    Other.


%% forms(Fs) -> lists:map(fun (F) -> form(F) end, Fs).

html(Forms) when is_list(Forms) ->
    [html(Form) || Form <- Forms];

%% -type form(Form) -> Form.
%%  Here we show every known form and valid internal structure. We do not
%%  that the ordering is correct!

%% First the various attributes.
html({attribute,Line,module,Mod}) ->
    [line(Line),
     '-', 'module', '(', span("module", atom_to_list(Mod)), ')', '.'];
html({attribute,Line,file,{File,Line}}) ->	%This is valid anywhere.
    [line(Line),
     '-', 'file', '(', span("file", File), ')', '.'];
html({attribute,Line,export,Es0}) ->
    [line(Line),
     '-', 'export', '[', farity_list(Es0), ']'];
html({attribute,Line,import,{Mod,Is0}}) ->
    ModuleSpan = span("module", atom_to_list(Mod)),
    [line(Line),
     '-', 'import', ',', ModuleSpan, '(', '[', farity_list(Is0), ']', ')', '.'];
html({attribute,Line,compile,C}) ->
    [line(Line),
     '-', 'compile', '(', span("compile_option", atom_to_list(C)), ')', '.'];
html({attribute,Line,record,{Name,Defs}}) ->
    [line(Line),
     '-', 'record', '(', span("record_name", atom_to_list(Name)), ',', '{', 
     record_defs(Defs),
     '}', ')', '.'];
html({attribute,Line,asm,{function,_N,_A,_Code}}) ->
    line(Line);
html({attribute,Line,_Attr,_Val}) ->		%The general attribute.
    line(Line);
html({function,Line,Name,_Arity,Clauses}) ->
    [line(Line),
     separate(';', lists:map(fun(Clause) -> clause(Name, Clause) end, Clauses)), '.'];
% Mnemosyne, ignore...
html({rule,Line,Name,Arity,Body}) ->
    {rule,Line,Name,Arity,Body}; % Dont dig into this
%% Extra forms from the parser.
html({error,E}) ->
    {error,E};
html({warning,W}) ->
    {warning,W};
html({eof,Line}) ->
    {eof,Line}.

%% -type farity_list([Farity]) -> [Farity] when Farity <= {atom(),integer()}.

farity_list(FunArities) ->
    separate(lists:map(fun farity/1, FunArities)).

farity({Name, Arity}) ->
    [function(Name), '/', arity(Arity)].

%% -type record_defs([RecDef]) -> [RecDef].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *parser*!

record_defs(Defs) ->
    separate(lists:map(fun record_def/1, Defs)).

record_def({record_field,Line,{atom,_La,A},Val}) ->
    [line(Line),
     span("record_field", atom_to_list(A)), '=', expr(Val)];
record_def({record_field,Line,{atom,_La,A}}) ->
    [line(Line),
     span("record_field", atom_to_list(A))].

clause({clause, Line, Head, GuardGroups, Body}) ->
    clause("", {clause, Line, Head, GuardGroups, Body}).

clause(Name, {clause,Line,Head,GuardGroups,Body}) ->
    [line(Line),
     span("function", atom_to_list(Name)),
     head(Head),
     separate(';', lists:map(fun guard_group/1, GuardGroups)),
     '->',
     separate(lists:map(fun expr/1, Body)),
     '.'].

%% -type head([Pattern]) -> [Pattern].

head(Patterns) ->
    ['(', separate(lists:map(fun pattern/1, Patterns)), ')'].

%% -type pattern(Pattern) -> Pattern.
%%  N.B. Only valid patterns are included here.

pattern({var,Line,V}) ->
    [line(Line),
     atom_to_list(V)];
pattern({match,Line,Left,Right}) ->
    [line(Line),
     span("match", [pattern(Left), '=', pattern(Right)])];
pattern({integer,Line,I}) ->
    [line(Line),
     integer(I)];
pattern({char,Line,C}) ->
    [line(Line),
     span("char", [C])];
pattern({float,Line,F}) ->
    [line(Line),
     span("float", io_lib:format("~w", [F]))];
pattern({atom,Line,A}) ->
    [line(Line),
     span("atom", atom_to_list(A))];
pattern({string,Line,S}) ->
    [line(Line),
     span("string", S)];
pattern({nil,Line}) ->
    [line(Line),
     span("list", ['[', ']'])];
pattern({tuple,Line,Patterns}) ->
    [line(Line),
     span("tuple", ['{', map_separate(fun pattern/1, Patterns), '}'])];
%% There's a special case for all cons's after the first: {tail, _}
%% so this is a list of one item.
pattern({cons,Line,Head,{nil, _}}) ->
    [line(Line),
     '[', pattern(Head), ']'];
pattern({cons,Line,Head,Tail}) ->
    [line(Line),
     '[', separate([pattern(Head) | pattern({tail, Tail})]), ']'];
pattern({tail, {cons, Line, Head, {nil, _}}}) ->
    [line(Line),
     [pattern(Head)]];
pattern({tail, {cons, Line, Head, Tail}}) ->
    [line(Line),
     [pattern(Head) | pattern({tail, Tail})]];
pattern({record,Line,Name,PatternFields}) ->
    [line(Line),
     ['#', span("atom", atom_to_list(Name)), '{',
      map_separate(fun pattern_field/1, PatternFields),
      '}']];
pattern({record_index,Line,Name,Field}) ->
    [line(Line),
     [span("record", ['#', atom_to_list(Name)]), '.', pattern(Field)]];
pattern({record_field,Line,Expression,RecName,Field}) ->
    [line(Line),
     [expr(Expression), '#', span("record", atom_to_list(RecName)), '.', expr(Field)]];
% How does this happen? (Foo).bar ?
%pattern({record_field,Line,Rec0,Field0}) ->
    %Rec1 = expr(Rec0),
    %Field1 = expr(Field0);
pattern({bin,Line,BinElems}) ->
    [line(Line),
     ['<<', separate(lists:map(fun bin_element/1, BinElems)), '>>']];
pattern({op,Line,Op,A}) ->
    [line(Line),
     span("operator", atom_to_list(Op)), pattern(A)];
pattern({op,Line,Op,L,R}) ->
    [line(Line),
     pattern(L), span("operator", atom_to_list(Op)), pattern(R)].

bin_element({bin_element,Line,Expression,Size1,Type1}) ->
    Size2 = case Size1 of
	     default ->
		 "";
	     _ ->
		 [':', expr(Size1)]
	 end,
    Type2 = case Type1 of
	     default ->
		 "";
	     _ ->
		 ['/', separate('-', lists:map(fun bit_type/1, Type1))]
	 end,
    [line(Line),
     expr(Expression), Size2, Type2].

bit_type(Atom) when is_atom(Atom) ->
    span(Atom);
bit_type({Atom, Integer}) when is_atom(Atom), is_integer(Integer) ->
    [atom(Atom), ':', integer(Integer)].

%% -type pattern_fields([Field]) -> [Field].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

pattern_field({record_field,Lf,{atom,_La,F},Pattern}) ->
    [line(Lf),
     span(F), '=', pattern(Pattern)];
pattern_field({record_field,Lf,{var,_La,'_'},Pattern}) ->
    [line(Lf),
     '_', '=', pattern(Pattern)].

guard_group(GuardGroup) ->
    separate(lists:map(fun expr/1, GuardGroup)).

%% -type exprs([Expression]) -> [Expression].
%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

%% -type expr(Expression) -> Expression.

expr({lc,Line,Result,Quals}) ->
    E1 = expr(Result),
    [line(Line),
     '[', E1, '||', map_separate(fun lc_bc_qual/1, Quals), ']'];
expr({bc,Line,E0,Quals}) ->
    E1 = expr(E0),
    [line(Line),
     '<<', E1, '||', map_separate(fun lc_bc_qual/1, Quals), '>>'];
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
     map_separate(fun clause/1, Clauses),
     'end'];
expr({'receive',Line,Clauses}) ->
    [line(Line),
     'receive', map_separate(fun clause/1, Clauses),
     'end'];
expr({'receive',Line,Clauses,AfterWait,AfterExpressions}) ->
    [line(Line),
     'receive', map_separate(fun clause/1, Clauses),
     'after', integer(AfterWait), '->',
     map_separate(fun expr/1, AfterExpressions),
     'end'];
expr({'try',Line,Expressions,_WhatIsThis,CatchClauses,AfterExpressions}) ->
    [line(Line),
     'try', map_separate(fun expr/1, Expressions),
     'catch', map_separate(';', fun clause/1, CatchClauses),
     'after', map_separate(fun expr/1, AfterExpressions),
     'end'];
expr({'fun',Line,Body}) ->
    case Body of
	{clauses,Clauses} ->
        [line(Line),
         'fun',
         map_separate(fun(Clause) -> clause("", Clause) end, Clauses)];
	{function,Fun,Arity} ->
        [line(Line),
         function(Fun), '/', arity(Arity)];
	{function,M,F,A} when is_atom(M), is_atom(F), is_integer(A) ->
        [line(Line),
         module(M), ':', function(F), '/', arity(A)];
	{function,M0,F0,A0} ->
	    %% R15: fun M:F/A with variables.
	    M = expr(M0),
	    F = expr(F0),
	    A = expr(A0),
        [line(Line),
         span("module", M),
         ':', span("function", F),
         '/', span("arity", A)]
    end;
expr({call,Line,Fun,Args}) ->
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    [line(Line),
     expr(Fun), '(', map_separate(fun expr/1, Args), ')'];
expr({'catch',Line,Expression}) ->
    %% No new variables added.
    [line(Line),
     'catch', expr(Expression)];
expr({match,Line,Pattern,Expression}) ->
    [line(Line),
     pattern(Pattern), '=', expr(Expression)];
expr({bin,Line,BinElements}) ->
    [line(Line),
     '<<', map_separate(fun bin/1, BinElements), '>>'];
expr({op,Line,Op,UnaryArg}) ->
    [line(Line),
     Op, expr(UnaryArg)];
expr({op,Line,Op,LeftArg,RightArg}) ->
    [line(Line),
     expr(LeftArg), Op, expr(RightArg)];
expr({remote, Line, {atom, _MLine, Module}, {atom, _FLine, Function}}) ->
    [line(Line),
     module(Module), ':', function(Function)];
expr(X) ->
    pattern(X).

%% -type lc_bc_qual([Qualifier]) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

%% This is a list of generators _or_ filters
%% which are simply expressions
%% A generator is a target and a source
lc_bc_qual({generate,_Line,Target,Source}) ->
    ['[', pattern(Target), '<-', expr(Source), ']'];
lc_bc_qual({b_generate,_Line,Target,Source}) ->
    [pattern(Target), '<=', expr(Source)];
lc_bc_qual(FilterExpression) ->
    expr(FilterExpression).

bin({bin_element,Size,Var,Size,MaybeTypes}) ->
    [var(Var),
    case Size of
        default ->
            "";
        {integer, _Line, Int} ->
            [':', integer(Int)]
    end,
    case MaybeTypes of
        default ->
            "";
        _ ->
            ['/', map_separate('-', fun bit_type/1, MaybeTypes)]
    end].

var(Atom) ->
    span("var", atom_to_list(Atom)).

atom(Atom) ->
    span("atom", atom_to_list(Atom)).

integer(Int) ->
    span("integer", integer_to_list(Int)).

module(Mod) ->
    span("module", atom_to_list(Mod)).

function(Fun) ->
    span("function", atom_to_list(Fun)).

arity(Arity) ->
    span("arity", integer_to_list(Arity)).

span(Keyword) ->
    span(Keyword, Keyword).

span(Class, Text) ->
     ["<span class=\"",
      Class,
      "\">",
      Text,
      "</span>"].

%span_open(Class) ->
    %["<span class=\"",
     %Class,
     %"</span>"].

parse_symbol('(') ->
    span("paren", "(");
parse_symbol(')') ->
    span("paren", ")");
parse_symbol('{') ->
    span("brace", "{");
parse_symbol('}') ->
    span("brace", "}");
parse_symbol('[') ->
    span("bracket", "[");
parse_symbol(']') ->
    span("bracket", "]");
parse_symbol('.') ->
    span("period", ".");
parse_symbol(',') ->
    span("comma", ",");
parse_symbol(';') ->
    span("semicolon", ";");
parse_symbol('/') ->
    span("slash", "/");
parse_symbol('-') ->
    span("dash", "-");
parse_symbol('=') ->
    span("equals", "=");
parse_symbol('->') ->
    span("clause_arrow", "->");
parse_symbol('<-') ->
    span("generator_arrow", "<-");
parse_symbol({line, Line}) ->
    span("line", integer_to_list(Line));
parse_symbol({eof, Line}) ->
    span("eof", integer_to_list(Line));
parse_symbol(Atom) ->
    span(atom_to_list(Atom)).

line(Line) ->
    {line, Line}.
    %span("line", integer_to_list(Line)).

separate(List) when is_list(List) ->
    separate(',', List).

separate(_, []) ->
    [];
separate(Separator, [Hd | Tl]) ->
    [Hd | [[Separator,E] || E <- Tl]].

map_separate(Fun, List) ->
    map_separate(',', Fun, List).

map_separate(Separator, Fun, List) ->
    separate(Separator, lists:map(Fun, List)).
