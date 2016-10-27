-module(example_list).

-export([a/0]).

a() ->
    _ = [],
    _ = [a, b],
    _ = [
        ],
    _ = [c
        ],
    _ = [e, [f, []]].
