%% @author Eskil Olsen <eskil@uber.com>

-module(repl_delay_core_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_replication_delays/0,
    get_min_replication_delay/0, get_max_replication_delay/0]).

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

get_replication_delays() ->
    gen_server:call(?MODULE, {get_replication_delay}).

get_min_replication_delay() ->
    gen_server:call(?MODULE, {get_min_replication_delay}).

get_max_replication_delay() ->
    gen_server:call(?MODULE, {get_max_replication_delay}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    ets:new(slaves, [set, public, named_table]),
    watch_postgres_slaves(repl_delay_core_config:slaves()),
    {ok, Args}.

handle_call({get_replication_delay}, _From, State) ->
    Result = ets:foldl(fun(A, Acc) -> [A|Acc] end, [], slaves),
    % TODO: could add min/max here too.
    {reply, Result, State};

handle_call({get_min_replication_delay}, _From, State) ->
    MinDelayFun =
        fun({_HostName, [[{master, _Master}, {delay, Delay}]]}, 0) ->
            Delay;
        ({_HostName, [[{master, _Master}, {delay, Delay}]]}, Acc) when Delay < Acc ->
            Delay;
        ({_HostName, [[{master, _Master}, {delay, _Delay}]]}, Acc) ->
            Acc
        end,
    Result = ets:foldl(MinDelayFun, 0, slaves),
    {reply, {min, Result}, State};

handle_call({get_max_replication_delay}, _From, State) ->
    MaxDelayFun =
        fun({_HostName, [[{master, _Master}, {delay, Delay}]]}, Acc) when Delay > Acc ->
            Delay;
        ({_HostName, [[{master, _Master}, {delay, _Delay}]]}, Acc) ->
            Acc
        end,
    Result = ets:foldl(MaxDelayFun, 0, slaves),
    {reply, {max, Result}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

watch_postgres_slaves([Slave|T]) ->
    watch_postgres:start_link(self(), Slave),
    watch_postgres_slaves(T);
watch_postgres_slaves([]) ->
    ok.
