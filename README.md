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
make release
./rel/repl_delay_service/bin/repl_delay_service start
curl localhost:8080
curl localhost:8080/min
curl localhost:8080/max
./rel/repl_delay_service/bin/repl_delay_service stop
```

Configuration
-------------

Config is currently a hardcoded list of servers in 
`apps/repl_delay_core/src/repl_delay_core_config.erl`.

Layout
======

apps/repl_delay_core
--------------------

Application that queries slaves and keeps replication front.

apps/repl_delay_webm
--------------------

Basho webmachine frontend.
