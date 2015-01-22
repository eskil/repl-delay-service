%% @author Eskil Olsen <eskil@eskil.org>

-module(watch_mysql).
-export([start_link/4, init/4]).
-record(state, {server, cluster, slave}).


% Main loop that queries the database for it's replication lag.
loop(S = #state{server=Server, cluster=Cluster, slave=Slave}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after 1000 ->
	    loop(S)
    end.

mysql_log(_File, _Line, Level, FmtArgsFun) ->
    % Don't log queries.
    %% {Fmt, Args} = FmtArgsFun(),
    %% lager:info(Fmt, Args),
    ok.

start_link(Server, Cluster, Settings, Slave) ->
    spawn_link(?MODULE, init, [Server, Cluster, Settings, Slave]).

init(Server, Cluster, Settings, Slave) ->
    Hostname = proplists:get_value(host, Slave),
    Port = proplists:get_value(port, Slave),
    User = proplists:get_value(user, Settings),
    Password = proplists:get_value(password, Settings),
    Database = proplists:get_value(database, Settings),
    mysql:start_link(pool, Hostname, Port, User, Password, Database, fun mysql_log/4, utf8),
    loop(#state{server=Server, cluster=Cluster, slave=Hostname}).
