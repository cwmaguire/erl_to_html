-module(parse_print).

-export([parse_transform/2]).
-export([parse_print/1]).

%% Compile file and apply this module as the
%% parse transform
parse_print(Filename) ->
    io:format("Compiling ~p~n", [Filename]),
    compile:file(Filename, [{parse_transform, ?MODULE}, {d, filename, Filename}]).

parse_transform(Forms, Options) ->
    io:format("Parse transform forms: ~n\t~p~n\t with Options:~n\t~p~n",
              [Forms, Options]),
    Forms.
