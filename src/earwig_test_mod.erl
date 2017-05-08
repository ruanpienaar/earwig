-module(earwig_test_mod).
-export([do_some/0]).


do_some() ->
    httpc:info().
