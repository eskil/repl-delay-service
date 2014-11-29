%% @author Eskil Olsen <eskil@eskil.org>

-module(repl_delay_core_config).
-export([slave_clusters/0]).

% Map from cluster to list of databases.
slave_clusters() ->
    [
     {cluster,
      [
       {name, "main"},
       {type, postgres},
       {settings,
	[
	 {user, "uber"},
	 {password, "uber"},
	 {database, "uber"}
	]
       },
       {slaves,
	[
	 [{host, "postgres01-sjc1"}, {port, 6432}],
	 [{host, "postgres03-sjc1"}, {port, 6432}],
	 [{host, "postgres04-sjc1"}, {port, 6432}],
	 [{host, "postgres05-sjc1"}, {port, 6432}],
	 [{host, "postgres06-sjc1"}, {port, 6432}],
	 [{host, "postgres08-sjc1"}, {port, 6432}],
	 [{host, "postgres14-peak1"}, {port, 6432}],
	 [{host, "postgres15-peak1"}, {port, 6432}],
	 [{host, "postgres19-peak1"}, {port, 6432}],
	 [{host, "postgres20-peak1"}, {port, 6432}],
	 [{host, "postgres02-sjc1"}, {port, 6432}],
	 [{host, "postgres21-peak1"}, {port, 6432}]
	]
       }
      ]
     }
    ].
