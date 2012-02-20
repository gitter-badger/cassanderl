-module(cassanderl_sup).
-behaviour(supervisor).

-define(SUPERVISOR, ?MODULE).

%% API
-export([start_link/0, get_info/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

get_info() ->
    dispcount:dispatcher_info(cassanderl_dispatch).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    start_dispatcher(),
    {ok, {{one_for_one, 10, 1}, []}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
start_dispatcher() ->
    {ok, Num} = application:get_env(cassanderl,worker_pool_size),
    ok = dispcount:start_dispatch(
        cassanderl_dispatch,
        {cassanderl_dispatch, []},
        [{restart,permanent},
         {shutdown,1000},
         {maxr,10},
         {maxt,1},
         {resources,Num}]
    ).