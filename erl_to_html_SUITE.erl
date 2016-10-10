-module(erl_to_html_SUITE).

-export([all/0]).

-export([basic/1]).

all() ->
    [basic].

basic(_Config) ->
    {ok, File} = file:open("basic.erl", [write]),
    Code = <<"-module(basic).\n-export([a/0]).\na() -> ok.">>,
    file:write(File, Code),
    erl_to_html:write_html("basic.erl"),
    {ok, HTML} = file:read_file("basic.html"),
    ct:pal("HTML = ~n~p", [HTML]),
    Expected = iolist_to_binary(["<html>"
                                 "<head></head><body>",
                                   "<div>"
                                     "<span class=\"line\">1</span>"
                                     "<span class=\"file\">basic.erl</span>"
                                   "</div>"
                                   "<div>"
                                     "<span class=\"line\">1</span>"
                                     "<span class=\"module\">basic</span>"
                                   "</div>"
                                   "<div>"
                                     "<span class=\"line\">2</span>"
                                     "<span class=\"dash\">-</span>"
                                     "<span class=\"export\">export</span>"
                                     "<span class=\"bracket\">[</span>"
                                     "<span class=\"function\">a</span>"
                                     "<span class=\"slash\">/</span>"
                                     "<span class=\"arity\">0</span>"
                                     "<span class=\"bracket\">]</span>"
                                   "</div>"
                                 "</body>"
                                 "</html>"]),
    ct:pal("Expected = ~n~p", [Expected]),
    HTML = Expected.
