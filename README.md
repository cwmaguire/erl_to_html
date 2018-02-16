# erl_to_html
Using a parse transform and erl_id_trans.erl to convert erlang code to HTML with spans for _everything_.

## Compatibility
I'm only testing it with Erlang 20

## How to use it
1. make
1. ./run
1. cat html_header my/source/file.html html_footer > my_file.html

## Tests
make tests

## Why do this? (Why not use ______?)

This project started when I started writing a tutorial on Erlang and was
hand-formatting Erlang as HTML. I got curious about using parse
transforms to turn Erlang into HTML.

## License
I have no idea what I need to do license-wise but since I'm copying
erl_id_trans.erl from the Erlang example code I'm using the Erlang
Public License.
