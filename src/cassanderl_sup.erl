
-module(cassanderl_sup).
-behaviour(supervisor).

-define(SUPERVISOR, ?MODULE).

%% API
-export([start_link/0, pick_worker/0, expire_workers_cache/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    bootstrap_ets(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

pick_worker() ->
    Workers = get_workers(),
    Size = ets:lookup_element(cassanderl, worker_pool_size, 2),
    Index0 = erlang:phash2({self(), now()}, Size),
    element(Index0 + 1, Workers).
    
expire_workers_cache() ->
    ets:delete(cassanderl, workers).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    WorkerPoolSize = ets:lookup_element(cassanderl, worker_pool_size, 2),
    Workers = [ worker_spec(I) || I <- lists:seq(1, WorkerPoolSize) ],
    {ok, {{one_for_one, 10, 1}, Workers}}.
    
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

bootstrap_ets() ->
    ets:new(cassanderl, [set, public, named_table, {read_concurrency, true}]),
    {ok, WorkerPoolSize} = application:get_env(cassanderl, worker_pool_size),
    ets:insert(cassanderl, {worker_pool_size, WorkerPoolSize}).
    
worker_spec(N) ->
    {{cassanderl, N},
        {cassanderl, start_link, []},
        permanent, 1000, worker,
        [cassanderl]
    }.
    
get_workers() ->
    try 
        ets:lookup_element(cassanderl, workers, 2)    
    catch
        error:badarg ->
            L = supervisor:which_children(?SUPERVISOR),
            Pids = [ Child || {_Id, Child, _Type, _Modules} <- L, is_pid(Child) ],
            PidsTuple = list_to_tuple(Pids),
            ets:insert(cassanderl, {workers, PidsTuple}),
            PidsTuple
    end.
