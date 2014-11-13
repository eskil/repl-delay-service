%% @author Eskil Olsen <eskil@uber.com>

-module(repl_delay_core_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_replication_delays/0]).

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


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    ets:new(slaves, [set, public, named_table]),
    watch_postgres_slaves(repl_delay_core_config:slaves()),
    {ok, Args}.

handle_call({get_replication_delay}, _From, State) ->
    Result = ets:foldl(fun(A, Acc) -> [A|Acc] end, [], slaves),
    {reply, Result, State};

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