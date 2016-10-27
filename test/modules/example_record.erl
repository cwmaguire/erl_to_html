-module(example_record).

-export([a/0]).

-record(a, {b = 0 :: integer,
            c = [],
            d = [1, b, "c", <<"d">>, [2, {}]],
            e :: list(integer()),
            f}).

a() ->
    _ = #a{},
    A = #a{b = 1},
    B = A#a.b,
    {(hd([A]))#a.b, B}.
