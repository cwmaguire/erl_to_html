-module(example_logic).

-export([a/0]).

a() ->
    1 > 0,
    1 < 0,
    1 == 1,
    1 =:= 1.0,
    1 > 2 andalso true,
    1 < 2 orelse false.
