-module(parse_test_catch).

-export([a/0]).

a() ->
    catch 1.
