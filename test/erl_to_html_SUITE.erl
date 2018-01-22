-module(erl_to_html_SUITE).

-export([all/0]).

-export([compare_html/1]).

all() ->
    [compare_html].

compare_html(_Config) ->
    ct:pal("~p", [file:get_cwd()]),
    {ok, Filenames} = file:list_dir("../../test/modules"),
    ct:pal("Filenames: ~p~n", [Filenames]),
    SourceFiles = lists:filter(fun is_source_file/1, Filenames),
    ct:pal("Source files: ~p~n", [SourceFiles]),
    [compare("../../test/modules/" ++ F, example_file(F), html_file(F)) || F <- SourceFiles].

compare(SourceFile, ExampleFile, ResultFile) ->
    ct:pal("Comparing result ~p with example ~p~n", [ResultFile, ExampleFile]),
    erl_to_html:write_html(SourceFile),
    {ok, Result} = file:read_file(ResultFile),
    {ok, Expected} = file:read_file(ExampleFile),
    Result = Expected.

is_source_file("example_" ++ Rest) ->
    [T, X, E | _] = lists:reverse(Rest),
    [E, X, T] == "erl";
is_source_file(_) ->
    false.

html_file(Filename) ->
    "../../test/modules/" ++ filename:rootname(Filename) ++ ".html".

example_file(Filename) ->
    "../../test/modules/test_" ++ filename:rootname(Filename) ++ ".html".
