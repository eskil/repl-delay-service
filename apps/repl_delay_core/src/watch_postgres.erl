%% @author Eskil Olsen <eskil@uber.com>

-module(watch_postgres).
-export([start_link/2, init/2]).
-record(state, {server, slave, pg_conn}).


% http://stackoverflow.com/questions/4328719/erlang-binary-string-to-integer-or-float
bin_to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.


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


loop(S = #state{server=Server, slave=Slave, pg_conn=PgConn}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after 1000 ->
	    {ok, Columns, Rows} = pgsql:squery(PgConn,
					       "SELECT hostname AS master, EXTRACT(EPOCH FROM NOW() - pg_time) AS delay FROM heartbeat;"),
	    Result = as_proplist(Columns, Rows),
	    ets:insert(slaves, {Slave, Result}),
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
