repl-delay-service
==================

Service to monitor postgres slaves' replication delay. 

You'll need a process on the master that regularly (eg. every 1s)
writes a timestamp to a `heartbeat` table. On the slaves, the delta in
the value from the clock will be a good estimate of the slave's delay.

Build
=====

To build, you'll need `rebar` in your path.

```
make make
./rel/repl_delay_service/bin/repl_delay_service start

# Get the entire cluster info
curl localhost:8080/cluster
# Get the entire cluster info, as json
curl -H "Accept: application/json" localhost:8080/cluster
# Get the least delayed slave's delay
curl localhost:8080/custer/min
# Get the most delayed slave's delay
curl localhost:8080/cluster/max

./rel/repl_delay_service/bin/repl_delay_service stop
```

Configuration
-------------

Config is currently a hardcoded list of servers in
`apps/repl_delay_core/src/repl_delay_core_config.erl`.

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
            [{host, "dbslave1"}, {port, 6432}],
            [{host, "dbslave2"}, {port, 6432}]
          ]
        }
      ]
    }
  ].
```

Layout
======


apps/repl_delay_core
--------------------

Application that queries slaves and keeps replication front. The
`apps/repl_delay_core/src/repl_delay_core_server.erl` contains the
server.


apps/repl_delay_webm
--------------------

Basho webmachine frontend. The resource endpoint is defined in
`apps/repl_delay_webm/src/repl_delay_webm_resource.erl` and routing in
`apps/repl_delay_webm/src/repl_delay_webm_config.erl`.


Todo
====

Probably make more sense to return the name of the least delayed slave in /min.