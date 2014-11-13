-module(repl_delay_webm_app).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    repl_delay_webm_sup:start_link().

stop(_State) ->
    ok.
