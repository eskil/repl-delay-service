%% @author Eskil Olsen <eskil@uber.com>

-module(watch_postgres).
-export([start_link/2, init/2]).
-record(state, {server, slave, pg_conn}).

as_proplist(Columns, Rows) ->
    ColNames = [binary_to_atom(Field, latin1) || {column, Field, _Type, _A, _B, _C} <- Columns],
    [[{K, V} || {K, V} <- lists:zip(ColNames, tuple_to_list(Row))] || Row <- Rows].

loop(S = #state{server=Server, slave=Slave, pg_conn=PgConn}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after 1000 ->
	    {ok, Columns, Rows} = pgsql:squery(PgConn,
					       "SELECT hostname AS master, EXTRACT(EPOCH FROM NOW() - pg_time) AS delay FROM heartbeat;"),
	    Result = as_proplist(Columns, Rows),
	    ets:insert(slaves, {Slave, Result}),
	    % ets:foldl(fun(A, Acc) -> io:format("~p~n", [A]), Acc end, 0, slaves),
	    loop(S)
    end.

start_link(Server, Slave) ->
    spawn_link(?MODULE, init, [Server, Slave]).

init(Server, Slave) ->
    [{host, Hostname}, {port, Port}] = Slave,
    User = "uber",
    Password = "uber",
    Opts = [{database, "uber"}, {timeout, 1000}, {port, Port}],
    {ok, C} = pgsql:connect(Hostname, User, Password, Opts),
    loop(#state{server=Server, slave=Hostname, pg_conn=C}).
