-module(example_receive).

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
