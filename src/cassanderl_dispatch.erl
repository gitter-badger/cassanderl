-module(cassanderl_dispatch).
-behaviour(dispcount).
-include("cassanderl.hrl").

-export([init/1, checkout/2, checkin/2, dead/1, handle_info/2,
         code_change/3, terminate/2]).

-define(DEFAULT_TIMEOUT, 500).

-record(state, {hostname,
                port,
                keyspace,
                client}).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% CALLBACK MODULES %%%
%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    {ok, Hostname} = application:get_env(cassanderl, hostname),
    {ok, Port} = application:get_env(cassanderl, port),
    {ok, Keyspace} = application:get_env(cassanderl, default_keyspace),
    {ok, Client} = start_client(Hostname, Port, Keyspace),
    State = #state{
        hostname = Hostname,
        port = Port,
        keyspace = Keyspace,
        client = Client
    },
    {ok, State}.

checkout(_From, State = #state{client=Client}) ->
    {ok, Client, State}.

%% The caller killed the client and tells us
checkin(died, State) ->
    dead(State);
%% The client is alive.
checkin(Client, State) ->
    {ok, State#state{client=Client}}.

dead(State = #state{hostname=HostName,
                    port=Port,
                    keyspace=Keyspace}) ->
    {ok, Client} = start_client(HostName, Port, Keyspace),
    {ok, State#state{client=Client}}.

handle_info(_Msg, State) ->
    {ok, State}.

terminate(_,_) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE/HELPERS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start_client(Hostname, Port, Keyspace) ->
    case thrift_client_util:new(Hostname, Port, cassandra_thrift, [{framed, true}]) of
        {ok, Client} ->
            case Keyspace of
                undefined ->
                    ok;
                _ ->
                    {Client2, {ok, ok}} = thrift_client:call(Client, set_keyspace, [Keyspace]),
                    {ok, Client2}
            end;
        {error, _} ->
            undefined
    end.

