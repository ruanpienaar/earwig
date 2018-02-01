.PHONY: compile test clean rebar3

compile: rebar3
	@./rebar3 compile

clean:
	@./rebar3 clean

rebar3:
	@ls rebar3 || wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3