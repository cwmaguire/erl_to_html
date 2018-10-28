DEPS = erl_png erl_to_png

dep_erl_png = git https://github.com/cwmaguire/erl_png master
dep_erl_to_png = git https://github.com/cwmaguire/erl_to_png master

include erlang.mk
