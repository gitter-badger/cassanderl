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
    {ok, {{one_for_one, 10, 1}, []}}.

