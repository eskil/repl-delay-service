%% @author Eskil Olsen <eskil@uber.com>

-module(repl_delay_config).
-export([slaves/0]).

slaves() ->
    % [ [ {host, ...}, ... ] ]
    [
     [{host, "10.31.3.144"}],
     [{host, "10.31.7.140"}]
    ].
