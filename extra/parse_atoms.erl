%% I was trying out having functions named after Erlang symbols but that ended up being too clever.
%% It makes more sense to have a parse function that pattern matches on symbols and produces HTML.
%% For now I'm going to leave atoms like '(', '-', ';', etc in the code and then parse them out
%% after the first pass.
-module(parse_atoms).

-export([parse/1]).

-export(['('/0]).

parse(List) when is_list(List) ->
    lists:map(fun parse/1, List);
parse(Atom) when is_atom(Atom) ->
    io:format("Calling function ~p~n", [Atom]),

    ?MODULE:Atom(); %% <- remote calls are resolved at run time

    %(fun Atom/0)() %% <- local calls are resolved at compile-time
                    %%    so using a variable doesn't work.
parse(Other) ->
    Other.

'('() ->
    "paren".
