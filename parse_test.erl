%% This is for use with parse_print.erl to find out what
%% different code will look like as parse_transform forms.
-module(parse_test).

-export([a/0]).
-export([b/2, c/1]).
-export([d/1]).
-export([e/0]).
-export([f/1]).

-record(a, {b = 0 :: integer,
            c = [],
            d = [1, b, "c", <<"d">>, [2, {}]],
            e :: list(integer()),
            f}).

a() ->
    #a{}.

b(A = 1, 2 = B) ->
    {A, B}.

%% the Line for match is the line the equals sign is on
c(
  A
  =
  1
 )
->
    -1,
    $c, 9.99, [], [a, b],
    [c
    ],
    [
    ], %% This doesn't get a line, the nil gets line 30
    A
    +
    2
    .

d(1) ->
    a;
d(2) ->
    b.

e() ->
    A = #a{b = 1},
    B = A#a.b,
    {(hd([A]))#a.b, B}.

f(<<A:1/binary, B/binary>>) ->
    <<A, "abc", B>>.
