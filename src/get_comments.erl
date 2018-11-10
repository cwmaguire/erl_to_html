-module(get_comments).

-export([comments/1]).
-export([comments/2]).

-define(SPACE, $ ).

comments(Filename) ->
    comments(Filename, "&nbsp;").

comments(Filename, Whitespace) ->
    {ok, Io} = file:open(Filename, [read]),
    comments(Io, 1, Whitespace, #{}, file:read_line(Io)).

comments(Io, Line, WS, Lines, {ok, Data}) ->
    comments(Io,
             Line + 1,
             WS,
             add_line(Line, WS, Lines, Data),
             file:read_line(Io));
comments(_, _Line, _WS, Lines, eof) ->
    Lines.

add_line(_EmptyLine, _WS, Lines, _Data = "\n") ->
    Lines;
add_line(Line, WS, Lines, Data) ->
    case parse(Data, WS) of
        {comment, Comment} ->
            Lines#{Line => Comment};
        _ ->
            Lines
    end.

parse(Text, WS) ->
    parse(text, WS, "", Text).

parse(_, _, _, []) ->
    undefined;
parse(whitespace, _WS, Whitespace, Rest = [$% | _]) ->
    [_ | NoNewline] = lists:reverse(Rest),
    {comment, Whitespace ++ lists:reverse(NoNewline)};
parse(text, _, _, Rest = [$% | _]) ->
    [_ | NoNewline] = lists:reverse(Rest),
    {comment, lists:reverse(NoNewline)};
parse(open_dquote, WS, _, [$" | Rest]) ->
    parse(text, WS, undefined, Rest);
parse(open_dquote, WS, _, [_ | Rest]) ->
    parse(open_dquote, WS, undefined, Rest);
parse(open_quote, WS, _, [$' | Rest]) ->
    parse(text, WS, undefined, Rest);
parse(open_quote, WS, _, [_ | Rest]) ->
    parse(open_quote, WS, undefined, Rest);
parse(text, WS, _, [$" | Rest]) ->
    parse(open_dquote, WS, undefined, Rest);
parse(_, WS, _, [$' | Rest]) ->
    parse(open_quote, WS, undefined, Rest);
parse(text, WS, _, [?SPACE | Rest]) ->
    parse(whitespace, WS, WS, Rest);
parse(whitespace, WS, Whitespace, [?SPACE | Rest]) ->
    parse(whitespace, WS, WS ++ Whitespace, Rest);
parse(whitespace, WS, _, [_ | Rest]) ->
    parse(text, WS, undefined, Rest);
parse(text, WS, _, [_ | Rest]) ->
    parse(text, WS, undefined, Rest).
