%% @author Eskil Olsen <eskil@eskil.org>

-module(watch_postgres).
-export([start_link/4, init/4]).
-record(state, {server, cluster, slave, pg_conn}).
-include("slave_record.hrl").


% Get the current time as a unix timestamp.
float_ts({MegaSecs, Secs, MicroSecs}) ->
    MegaSecs * 1000000 + Secs + (MicroSecs / 1000000.0).
float_ts() ->
    float_ts(now()).

% http://stackoverflow.com/questions/4328719/erlang-binary-string-to-integer-or-float
bin_to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

% Convert a pgsql result into a proplist and do some typeconversions along the way.
as_proplist(Columns, Rows) ->
    ColNames = [binary_to_atom(Field, latin1) || {column, Field, _Type, _A, _B, _C} <- Columns],
    ConvFun = fun(Type) ->
        case Type of
           float8 ->
               fun bin_to_num/1;
	   _ ->
               fun(V) -> V
	       end

        end
    end,
    ConvFuncs = [ConvFun(Type) || {column, _Field, Type, _A, _B, _C} <- Columns],
    [[{K, CF(V)} || {K, CF, V} <- lists:zip3(ColNames, ConvFuncs, tuple_to_list(Row))] || Row <- Rows].

% Main loop that queries the database for it's replication lag.
loop(S = #state{server=Server, cluster=Cluster, slave=Slave, pg_conn=PgConn}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after 1000 ->
	    {ok, Columns, Rows} = pgsql:squery(PgConn,
                "SELECT hostname AS master, EXTRACT(EPOCH FROM NOW() - pg_time) AS delay FROM heartbeat;"),
	    Result = as_proplist(Columns, Rows),
	    F = fun() ->
	        mnesia:write(#slave_record{cluster=Cluster, name=Slave, ts=float_ts(), delay=Result})
	    end,
	    mnesia:activity(transaction, F),
	    ets:insert(list_to_atom("slaves." ++ Cluster), {Slave, float_ts(), Result}),
	    loop(S)
    end.

start_link(Server, Cluster, Settings, Slave) ->
    spawn_link(?MODULE, init, [Server, Cluster, Settings, Slave]).

init(Server, Cluster, Settings, Slave) ->
    Hostname = proplists:get_value(host, Slave),
    Port = proplists:get_value(port, Slave),
    User = proplists:get_value(user, Settings),
    Password = proplists:get_value(password, Settings),
    Database = proplists:get_value(database, Settings),
    Opts = [{database, Database}, {timeout, 1000}, {port, Port}],
    {ok, Conn} = pgsql:connect(Hostname, User, Password, Opts),
    loop(#state{server=Server, cluster=Cluster, slave=Hostname, pg_conn=Conn}).
