-module(parse_test2).

-export([a/1]).

%% This becomes a list of two ops
a(A) when A > 1, A < 3 ->
    ok;
a(A) when A > 1, A < 4; A > 8 ->
    ok;
a(A) when A > 1, A < 4; A > 8, A < 10 ->
    ok;
%% This becomes two lists of one op each
a(A) when A < 1; A > 4 ->
    ok;
%% This is a list of one op: andalso
a(A) when is_integer(A) andalso A == 2 ->
    ok;
%% This is a list of a call
a(A) when is_integer(A) ->
    ok.

