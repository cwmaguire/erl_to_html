-module(example_binary).

-export([a/0]).
-export([a/1]).
-export([b/0]).
-export([c/0]).

a() ->
    << <<B/binary, A/binary>> || <<A:1/binary, B:1/binary>> <= <<"abcd">> >>.

a(<<A:1/binary, B/binary>>) ->
    <<A, "abc", B>>.

b() ->
    << <<B, A/binary>> || <<A:1/binary, B/integer>> <= <<"abcd">>, B > 98 >>.

c() ->
    << <<A/binary>> || <<A:1/binary, _/integer>> <= <<"abcd">> >>.

