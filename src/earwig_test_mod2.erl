-module(earwig_test_mod2).
-export([
    i_got_called/0,
    circular_call/1
]).

i_got_called() ->
    ok.

circular_call(X) when X =< 0 ->
    0;
circular_call(X) when X > 0 ->
    earwig_test_mod2:circular_call(X-1).