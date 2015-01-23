# repl-delay-service


Service to report database slaves' replication delay.

You'll need a process on the master that regularly (eg. every 1s)
writes a timestamp to a `heartbeat` table. On the slaves, the delta in
the value from the clock will be a good estimate of the slave's delay.

This assumes that the clock on the master and slaves are synchronised
to within your own tolerance.

Mostly written as a demonstration of erlang.


### Build

To build, you'll need `rebar` in your path.

```
make
./rel/repl_delay_service/bin/repl_delay_service start
./rel/repl_delay_service/bin/repl_delay_service console

# Get the entire 'foo' cluster info
curl localhost:8080/foo
# Get the entire 'foo' cluster info, as json
curl -H "Accept: application/json" localhost:8080/foo
# Get the least delayed slave in foo's delay
curl localhost:8080/foo/min
# Get the most delayed slave in foo's delay
curl localhost:8080/foo/max

./rel/repl_delay_service/bin/repl_delay_service stop
```


### Configuration

Config is currently a hardcoded list of servers in
[repl_delay_core_config.erl](apps/repl_delay_core/src/repl_delay_core_config.erl).

```
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
            [{host, "pgslave1"}, {port, 6432}],
            [{host, "pgslave2"}, {port, 6432}]
          ]
        }
      ]
    },
    {cluster,
      [
        {name, "services"},
        {type, mysql},
        {settings,
          [
            {user, "user"},
            {password, "password"},
            {database, "database"}
          ]
        },
        {slaves,
          [
            [{host, "mysqlslave1"}, {port, 3306}],
            [{host, "mysqlslave2"}, {port, 3306}]
          ]
        }
      ]
    }

  ].
```



### Heartbeat table

#### mysql

```
CREATE TABLE `heartbeat` (
  `ts` varchar(26) NOT NULL,
  `server_id` int(10) unsigned NOT NULL,
  `file` varchar(255) DEFAULT NULL,
  `position` bigint(20) unsigned DEFAULT NULL,
  `relay_master_log_file` varchar(255) DEFAULT NULL,
  `exec_master_log_pos` bigint(20) unsigned DEFAULT NULL,
  PRIMARY KEY (`server_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1
```

### postgres

```
CREATE TABLE heartbeat (
    hostname character varying(128),
    pg_time timestamp without time zone,
    sys_time timestamp without time zone
);
```


### Layout

#### [apps/repl_delay_core](apps/repl_delay_core)

Application that queries slaves and keeps replication front. The
[repl_delay_core_server.erl](apps/repl_delay_core/src/repl_delay_core_server.erl)
contains the server. This runs a process per database slave that
writes the slave's info into shared erlang ETS store.

Each type of database has it's own process loop defined in
* [watch_mysql.erl](apps/repl_delay_core/src/watch_mysql.erl)
* [watch_postgres.erl](apps/repl_delay_core/src/watch_postgres.erl)

#### [apps/repl_delay_webm](apps/repl_delay_webm)

Basho webmachine frontend. The resource endpoint is defined in
[repl_delay_webm_resource.erl](apps/repl_delay_webm/src/repl_delay_webm_resource.erl)
and routing in
[repl_delay_webm_config.erl](apps/repl_delay_webm/src/repl_delay_webm_config.erl). On
requests, it sends a synchronous call to `repl_delay_core_server` for
the info.


### Todo

* Makes more sense to return the name of the least delayed slave in /min.
* Finish mysql support (abuse minimez, grant privs to _ro role to access heartbeat table).
* Redo ets use to not be retarded.
** Index by cluster.
** use ets:match to find machines in a cluster, fold over these.
* Rewrite in elixir to try it because it looks awesome.
* Add endpoint to list available clusters.
