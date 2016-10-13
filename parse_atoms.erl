-module(parse_atoms).

-export([parse/1]).

-export(['('/0]).

parse(List) when is_list(List) ->
    lists:map(fun parse/1, List);
parse(Atom) when is_atom(Atom) ->
    io:format("Calling function ~p~n", [Atom]),
    %?MODULE:Atom(); %% <- local calls are resolved at run-time
    (fun Atom/0)();
parse(Other) ->
    Other.

'('() ->
    "paren".
