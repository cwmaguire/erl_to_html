-module(get_comments).

-export([comments/1]).

-define(SPACE, $ ).

comments(Filename) ->
    {ok, Io} = file:open(Filename, [read]),
    comments(Io, 1, #{}, file:read_line(Io)).

comments(Io, Line, Lines, {ok, Data}) ->
    comments(Io, Line + 1, add_line(Line, Lines, Data), file:read_line(Io));
comments(_, _Line, Lines, eof) ->
    Lines.

add_line(_EmptyLine, Lines, _Data = "\n") ->
    Lines;
add_line(Line, Lines, Data) ->
    case parse(Data) of
        {comment, Comment} ->
            Lines#{Line => Comment};
        _ ->
            Lines
    end.

parse(Text) ->
    parse(text, "", Text).

parse(_, _, []) ->
    undefined;
parse(whitespace, Whitespace, Rest = [$% | _]) ->
    [_ | NoNewline] = lists:reverse(Rest),
    {comment, Whitespace ++ lists:reverse(NoNewline)};
parse(text, _, Rest = [$% | _]) ->
    [_ | NoNewline] = lists:reverse(Rest),
    {comment, lists:reverse(NoNewline)};
parse(open_dquote, _, [$" | Rest]) ->
    parse(text, undefined, Rest);
parse(open_dquote, _, [_ | Rest]) ->
    parse(open_dquote, undefined, Rest);
parse(open_quote, _, [$' | Rest]) ->
    parse(text, undefined, Rest);
parse(open_quote, _, [_ | Rest]) ->
    parse(open_quote, undefined, Rest);
parse(text, _, [$" | Rest]) ->
    parse(open_dquote, undefined, Rest);
parse(_, _, [$' | Rest]) ->
    parse(open_quote, undefined, Rest);
parse(text, _, [?SPACE | Rest]) ->
    parse(whitespace, "&nbsp;", Rest);
parse(whitespace, Whitespace, [?SPACE | Rest]) ->
    parse(whitespace, [$&, $n, $b, $s, $p, $; | Whitespace], Rest);
parse(whitespace, _, [_ | Rest]) ->
    parse(text, undefined, Rest);
parse(text, _, [_ | Rest]) ->
    parse(text, undefined, Rest).
