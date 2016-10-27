-module(example_lc).

-export([a/0]).
-export([b/0]).
-export([c/0]).

a() ->
    [A || {A, _} <- [{1, a}, {2, b}, c]].

b() ->
    One = [{1, a}, {2, b}, c],
    Two = [1, 2, 3, 4, 5],
    [{A, B} || {A, _} <- One, B <- Two].

c() ->
    One = [{1, a}, {2, b}, c],
    [A || A <- One, A > 3].

