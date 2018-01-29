-module(get_indents).

-export([indents/1]).

indents(Filename) ->
    {ok, Io} = file:open(Filename, [read]),
    indents(Io, 1, #{}, file:read_line(Io)).

indents(Io, Line, Lines, {ok, Data}) ->
    indents(Io, Line + 1, add_line(Line, Lines, Data), file:read_line(Io));
indents(_, _Line, Lines, eof) ->
    Lines.

add_line(_EmptyLine, Lines, _Data = "\n") ->
    Lines;
add_line(Line, Lines, Data) ->
    {Taken, Rest} = string:take(string:replace(Data, "\t", "    "), " "),
    %io:format(user, "~p: Taken = ~p, Rest = ~p~n", [Line, Taken, Rest]),
    case {Taken, Rest} of
        {_, []} ->
            Lines;
        {"", _} ->
            Lines;
        _ ->
            %io:format(user, "Length of Taken: ~p~n", [length(Taken)]),
            Lines#{Line => length(Taken)}
    end.
