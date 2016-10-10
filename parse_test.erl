-module(parse_test).

-export([a/0]).

-record(a, {b = 0 :: integer,
            c = [],
            d = [1, b, "c", <<"d">>, [2, {}]],
            e :: list(integer()),
            f}).

a() ->
    #a{}.
