-module(earwig_test_mod).
-export([
	call_myself/0,
	call_other_mod/0,
	circular_call/1
]).

call_myself() ->
    myself().
    
myself() ->
   ok.
   
call_other_mod() ->
    earwig_test_mod2:i_got_called().

circular_call(X) when X =< 0 ->
    0;
circular_call(X) when X > 0 ->
    earwig_test_mod2:circular_call(X-1).