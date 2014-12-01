# repl-delay-service


Service to report database slaves' replication delay. 

You'll need a process on the master that regularly (eg. every 1s)
writes a timestamp to a `heartbeat` table. On the slaves, the delta in
the value from the clock will be a good estimate of the slave's delay.

This assumes that the clock on the master and slaves are synchronised
to within your own tolerance.

Mostly written as a demonstration of erlang.

## Build

To build, you'll need `rebar` in your path.

```
make make
./rel/repl_delay_service/bin/repl_delay_service start

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

## Configuration


Config is currently a hardcoded list of servers in
[repl_delay_core_config.erl](apps/repl_delay_core/src/repl_delay_core_config.erl).

```
slave_clusters() ->
  [
    {cluster,
      [
        {name, "foo"},
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
            [{host, "dbslave1"}, {port, 6432}],
            [{host, "dbslave2"}, {port, 6432}]
          ]
        }
      ]
    }
  ].
```


## Layout


### [apps/repl_delay_core](apps/repl_delay_core)

Application that queries slaves and keeps replication front. The
[repl_delay_core_server.erl](apps/repl_delay_core/src/repl_delay_core_server.erl)
contains the server. This runs a process per database slave that
writes the slave's info into shared erlang ETS store.

Each type of database has it's own process loop defined in
* [mysql](apps/repl_delay_core/src/watch_mysql.erl)
* [postgres](apps/repl_delay_core/src/watch_postgres.erl)

### [apps/repl_delay_webm](apps/repl_delay_webm)

Basho webmachine frontend. The resource endpoint is defined in
[repl_delay_webm_resource.erl](apps/repl_delay_webm/src/repl_delay_webm_resource.erl)
and routing in
[repl_delay_webm_config.erl](apps/repl_delay_webm/src/repl_delay_webm_config.erl). On
requests, it sends a synchronous call to `repl_delay_core_server` for
the info.


## Todo

* Makes more sense to return the name of the least delayed slave in /min.
* Finish mysql support.