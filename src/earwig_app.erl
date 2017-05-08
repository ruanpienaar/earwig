-module(earwig_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("earwig.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    earwig_sup:start_link().

stop(_State) ->
    ok.
