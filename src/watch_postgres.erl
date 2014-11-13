%% @author Eskil Olsen <eskil@uber.com>

-module(watch_postgres).
-include_lib("repl_delay.hrl").
-export([start_link/2, init/2, cancel/1]).
-record(state, {server, pg_conn}).

loop(S = #state{server=Server, pg_conn=PgConn}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after 1000 ->
	    {ok, Columns, Rows} = pgsql:squery(PgConn,
					       "SELECT hostname, NOW() - pg_time FROM heartbeat;"),
	    io:format("~p ~p~n", [Columns, Rows]),
	    loop(S)
    end.

cancel(Pid) ->
        %% Monitor in case the process is already dead.
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
        receive
        {Ref, ok} ->

		erlang:demonitor(Ref, [flush]),
		ok;
	            {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    end.

%
start_link(Server, Slave) ->
    spawn_link(?MODULE, init, [Server, Slave]).

init(Server, Slave) ->
    {host, Hostname} = Slave,
    User = "uber",
    Opts = [{database, "uber"}, {timeout, 1000}, {port, 6432}],
    {ok, C} = pgsql:connect(Hostname, User, Opts),
    loop(#state{server=Server, pg_conn=C}).
