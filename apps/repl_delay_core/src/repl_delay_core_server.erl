%% @author Eskil Olsen <eskil@eskil.org>

-module(repl_delay_core_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("slave_record.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_replication_delays/1,
         get_min_replication_delay/1, get_max_replication_delay/1,
         exists/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_replication_delays(Cluster) ->
    gen_server:call(?MODULE, {get_replication_delay, Cluster}).

get_min_replication_delay(Cluster) ->
    gen_server:call(?MODULE, {get_min_replication_delay, Cluster}).

get_max_replication_delay(Cluster) ->
    gen_server:call(?MODULE, {get_max_replication_delay, Cluster}).

exists(Cluster) ->
    gen_server:call(?MODULE, {exists, Cluster}).



%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    init_tables(),
    watch_slave_clusters(repl_delay_core_config:slave_clusters()),
    {ok, Args}.

handle_call({get_replication_delay, Cluster}, _From, State) ->
    DelayFun =
        fun({Slave, Ts, [[{master, Master}, {delay, Delay}]]}, Acc) ->
            [{Slave, [{master, Master},  {delay, float_ts() - Ts + Delay}]} | Acc]
        end,
    Ets = list_to_atom("slaves." ++ Cluster),
    Result = ets:foldl(DelayFun, [], Ets),
    {reply, Result, State};

handle_call({get_min_replication_delay, Cluster}, _From, State) ->
    DelayFun =
        fun({_Slave, Ts, [[{master, _Master}, {delay, Delay}]]}, Acc) ->
            [float_ts() - Ts + Delay | Acc]
        end,
    Ets = list_to_atom("slaves." ++ Cluster),
    Delay = lists:min(ets:foldl(DelayFun, [1048577], Ets)),
    {reply, {min, Delay}, State};

handle_call({get_max_replication_delay, Cluster}, _From, State) ->
    DelayFun =
        fun({_Slave, Ts, [[{master, _Master}, {delay, Delay}]]}, Acc) ->
            [float_ts() - Ts + Delay | Acc]
        end,
    Ets = list_to_atom("slaves." ++ Cluster),
    Delays = ets:foldl(DelayFun, [-1], Ets),
    Delay = lists:max(Delays),
    {reply, {max, Delay}, State};

handle_call({exists, Cluster}, _From, State) ->
    Ets = list_to_atom("slaves." ++ Cluster),
    case ets:info(Ets) of
        undefined ->
            {reply, undefined, State};
        _ ->
            {reply, ok, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    mnesia:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

float_ts({MegaSecs, Secs, MicroSecs}) ->
    MegaSecs * 1000000 + Secs + (MicroSecs / 1000000.0).
float_ts() ->
    float_ts(now()).

watch_slave_cluster(postgres, Cluster, Settings, [Slave|Slaves]) ->
    watch_postgres:start_link(self(), Cluster, Settings, Slave),
    watch_slave_cluster(postgres, Cluster, Settings, Slaves);
watch_slave_cluster(mysql, Cluster, Settings, [Slave|Slaves]) ->
    watch_mysql:start_link(self(), Cluster, Settings, Slave),
    watch_slave_cluster(mysql, Cluster, Settings, Slaves);
watch_slave_cluster(_Type, _Cluster, _Settings, []) ->
    ok.

watch_slave_clusters([{cluster, Properties}|T]) ->
    Cluster = proplists:get_value(name, Properties),
    Type = proplists:get_value(type, Properties),
    Settings = proplists:get_value(settings, Properties),
    Slaves = proplists:get_value(slaves, Properties),
    Ets = list_to_atom("slaves." ++ Cluster),
    ets:new(Ets, [set, public, named_table]),
    watch_slave_cluster(Type, Cluster, Settings, Slaves),
    watch_slave_clusters(T);
watch_slave_clusters([]) ->
    ok.

init_tables() ->
    Nodes = [node()],
    ok = mnesia:start(),
    mnesia:create_schema(Nodes),
    mnesia:delete_table(slaves),
    {atomic, ok} = mnesia:create_table(slaves,
				       [
					{attributes, record_info(fields, slave_record)},
					% name is the first field, thus always indexed.
					{index, [#slave_record.cluster]},
					{ram_copies, Nodes}
				       ]).
