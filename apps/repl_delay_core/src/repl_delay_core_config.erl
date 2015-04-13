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
	 {user, "user"},
	 {password, "password"},
	 {database, "database"}
	]
       },
       {slaves,
	[
	 [{host, "postgres02"}, {port, 6432}],
	 [{host, "postgres03"}, {port, 6432}],
	]
       }
      ]
     }
    ].
