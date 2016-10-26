-module(parse_test_remote_fun).

-export([a/0]).

a() ->
    erlang:integer_to_list(1).
