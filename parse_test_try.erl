-module(parse_test_try).

-export([a/0]).
-export([b/0]).

a() ->
    try
       integer_to_list(1)
    catch
       A:B ->
           {A, B}
    after
       'after'
    end.

b() ->
    try
       integer_to_list(1)
    catch
       A:B when B == 1 ->
           C = A + 1,
           {C, B}
    after
       _1 = 'after clause 1',
       _2 = "after clause 2"
    end.
