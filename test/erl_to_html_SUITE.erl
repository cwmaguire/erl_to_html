-module(erl_to_html_SUITE).

-export([all/0]).

-export([compare_html/1]).

all() ->
    [compare_html].

compare_html(_Config) ->
    io:format("~p", [file:get_cwd()]),
    {ok, Filenames} = file:list_dir("../../test/modules"),
    io:format("Filenames: ~p~n", [Filenames]),
    [compare(F, example_file(F), html_file(F)) || F = "example_" ++ _ <- Filenames].

compare(Source, Example, Result) ->
    erl_to_html:write_html(Source),
    io:format("Reading result: ~p~n", [Result]),
    {ok, Result} = file:read_file(Result),
    io:format("Reading example: ~p~n", [Example]),
    {ok, Expected} = file:read_file(Example),
    Result = Expected.

html_file(Filename) ->
    "../../test/modules/" ++ filename:rootname(Filename) ++ ".html".

example_file(Filename) ->
    "../../test/modules/" ++ filename:rootname(Filename) ++ "_example.html".
