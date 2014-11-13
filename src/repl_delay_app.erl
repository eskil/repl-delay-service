%% @author Eskil Olsen <eskil@uber.com>

-module(repl_delay_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    repl_delay_sup:start_link().

stop(_State) ->
    ok.
