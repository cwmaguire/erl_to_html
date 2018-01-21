%% comment
%% $$
-module(example_bc).

-export([a/0]).
-export([b/0]).
-export([c/0]).

a() ->
    << <<B/binary, A/binary>> || <<A:1/binary, B:1/binary>> <= <<"abcd">> >>.

b() ->
    << <<B, A/binary>> || <<A:1/binary, B/integer>> <= <<"abcd">>, B > 98 >>.

c() ->
    << <<A/binary>> || <<A:1/binary, _/integer>> <= <<"abcd">> >>.

