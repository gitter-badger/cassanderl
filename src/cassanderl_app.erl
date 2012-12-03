-module(cassanderl_app).

-behaviour(application).

%% API
-export([get_info/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% API functions
%% ===================================================================

get_info() ->
    dispcount:dispatcher_info(cassanderl_dispatch).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(dispcount), % we could remove this
    start_dispatcher(),
    cassanderl_sup:start_link().

stop(_State) ->
    stop_dispatcher(),
    ok.

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

stop_dispatcher() ->
    dispcount:stop_dispatch(cassanderl_dispatch).
