repl-delay-service
==================

Erlang service to monitor postgres slaves' replication delay. You'll need a process on the master thar regularly 
writes a timestamp to a `heartbeat` table. On the slaves, the delta in the value from the clock will 
be a good estimate of the slave's delay.

To build, you'll need rebar in your path.

```
make release
./rel/repl_delay_service/bin/repl_delay_service start
curl localhost:8080
curl localhost:8080/min
curl localhost:8080/max
```
