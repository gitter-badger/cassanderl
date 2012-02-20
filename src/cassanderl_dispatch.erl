-module(cassanderl_dispatch).
-behaviour(dispcount).
-include("cassanderl.hrl").

-export([init/1, checkout/2, checkin/2, dead/1, handle_info/2,
         code_change/3, terminate/2]).

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
    Keyspace =
        case application:get_env(cassanderl, default_keyspace) of
            undefined ->
                undefined;
            {ok, DefaultKeyspace} ->
                DefaultKeyspace
        end,
    Client =
        case start_client(Hostname, Port, Keyspace) of
            {ok, Client2} ->
                Client2;
            {error, _Error} ->
                undefined
        end,
    State = #state {
        hostname = Hostname,
        port = Port,
        keyspace = Keyspace,
        client = Client
    },
    {ok, State}.

checkout(_From, State = #state{hostname=HostName,
                               port=Port,
                               keyspace=Keyspace,
                               client=undefined}) ->
    case start_client(HostName, Port, Keyspace) of
        {ok, Client} ->
            {ok, Client, State#state{client=Client}};
        {error, Error} ->
            {error, Error, State}
    end;
checkout(_From, State = #state{client=Client}) ->
    {ok, Client, State}.

%% The caller killed the client and tells us
checkin(died, State) ->
    dead(State);
%% The client is alive.
checkin(Client, State) ->
    {ok, State#state{client=Client}}.

dead(State) ->
    {ok, State#state{client=undefined}}.

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
                    {ok, Client};
                _ ->
                    case thrift_client:call(Client, set_keyspace, [Keyspace]) of
                        {Client2, {exception, _Exception}} ->
                            {ok, Client2};
                        {Client2, {ok, ok}} ->
                            {ok, Client2}
                    end
            end;
        {error, Error} ->
            {error, Error}
    end.

