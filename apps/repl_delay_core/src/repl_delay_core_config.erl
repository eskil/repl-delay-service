%% @author Eskil Olsen <eskil@uber.com>

-module(repl_delay_core_config).
-export([slaves/0]).

slaves() ->
    % [ [ {host, ...}, ... ] ]
    [
     [{host, "localhost"}, {port, 5432}],
     [{host, "localhost"}, {port, 5432}],
     [{host, "localhost"}, {port, 5432}]
    ].
