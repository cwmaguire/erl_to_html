-module(extract_char).

-include("../deps/erl_png/include/png.hrl").

-export([extract/2]).

% Each char is 11 x 18
%
% 3 pixels down to start row
%
% 5 pixels left to first character
%
% 2 pixels between characters sideways
%
% 8 pixels between rows

-define(CHARS_OLD, [$0,$1,$2,$3,$4,
                $5,$6,$7,$8,$9,
                $a,$b,$c,$d,$e,
                $f,$g,$h,$i,$j,
                $k,$l,$m,$n,$o,
                $p,$q,$r,$s,$t,
                $u,$v,$w,$x,$y,$z,
                $A,$B,$C,$D,$E,
                $F,$G,$H,$I,$J,
                $K,$L,$M,$N,$O,
                $P,$Q,$R,$S,$T,
                $U,$V,$W,$X,$Y,$Z,
                $!,$@,$#,$$,$%,
                $^,$&,$*,$(,$),
                $-,$=,$_,$+,$`,
                $~,$,,$.,$<,$>,
                $/,$?,$[,$],${,
                $},$\\,$|,$]]).
-define(CHARS,
        [#{$0 => {}},
         #{$1 => {}},
         #{$2 => {}},
         #{$3 => {}},
         #{$4 => {}},
         #{$5 => {}},
         #{$6 => {}},
         #{$7 => {}},
         #{$8 => {}},
         #{$9 => {}},
         #{$a => {}}]).

-define(LEFT_PAD, 5).
-define(TOP_PAD, 3).
-define(CHAR_SPACING, 2).
-define(ROW_SPACING, 8).
-define(LINE_CHAR_WIDTH, 11).
-define(LINE_CHAR_HEIGHT, 18).
-define(CHAR_WIDTH, 13).
-define(CHAR_HEIGHT, 20).

extract(Row, Char) ->
    Idx = index(Char, ?CHARS),
    io:format(user, "Idx = ~p~n", [Idx]),
    {X, Y, W, H} =
        case Row of
            0 ->
                X_ = ?LEFT_PAD + (Idx * ?LINE_CHAR_WIDTH) + (Idx * ?CHAR_SPACING),
                W_ = ?LINE_CHAR_WIDTH,
                Y_ = ?TOP_PAD,
                H_ = ?LINE_CHAR_HEIGHT,
                {X_, Y_, W_, H_};
            _ ->
                X_ = ?LEFT_PAD + (Idx * ?CHAR_WIDTH) + (Idx * ?CHAR_SPACING),
                W_ = ?CHAR_WIDTH,
                Y_ = ?TOP_PAD + ?LINE_CHAR_HEIGHT + ((Row - 1) * ?CHAR_HEIGHT) + (Row * ?ROW_SPACING),
                H_ = ?CHAR_HEIGHT,
                {X_, Y_, W_, H_}
        end,
    io:format(user, "Coords = ~p~n", [{X, Y, W, H}]),
    #png{pixels = Pixels} = png:read("char_map.png"),
    %io:format("Row ~p:~n~p~n", [Y, lists:nth(Y, Pixels)]),
    %io:format("Column ~p:~n~p~n", [X, [lists:nth(X, Row_) || Row_ <- Pixels]]),

    Lines = lists:sublist(Pixels, Y, H),
    io:format(user, "length(Lines) = ~p~n", [length(Lines)]),
    CharLines = lists:map(fun(Line) -> lists:sublist(Line, X, W) end, Lines),
    %io:format("CharLines: ~p", [CharLines]),
    write_png(CharLines).

index(Char, Chars) ->
    index(Char, Chars, 0).

index(Char, [Char | _], Index) ->
    Index;
index(Char, [_ | Rest], Index) ->
    index(Char, Rest, Index + 1).

write_png(Lines) ->
    Header = #header{width = ?CHAR_WIDTH,
                     height = ?CHAR_HEIGHT,
                     bit_depth = 8,
                     color_type = 6,
                     compression = 0,
                     filter = 0,
                     interlace = 0},
    Png = #png{header = Header,
               background = <<255,0,0>>,
               physical = {?CHAR_WIDTH, ?CHAR_HEIGHT, 0},
               srgb = 0, % rendering intent
               text = [],
               data = <<>>,
               pixels = Lines,
               other = []},
    png:write(Png, "z.png").
