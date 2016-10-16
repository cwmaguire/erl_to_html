-module(parse_test_receive).

-export([a/0]).

a() ->
    receive
        a ->
            ok;
        b ->
            ok
    after 1000 ->
        c
    end.
