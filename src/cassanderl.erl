-module(cassanderl).
-include("cassanderl.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([get_info/0, call/3, set_keyspace/2, get/6, insert/9, describe_keyspace/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

get_info() ->
    cassanderl_sup:get_info().

call(Info, Function, Args) ->
    case dispcount:checkout(Info) of
        {ok, Ref, Client} ->
            {ok, BaseKey} = application:get_env(adgear_gateway, statsderl_key),
            Timestamp = os:timestamp(),
            try thrift_client:call(Client, Function, Args) of
                {error, Reason} ->
                    dispcount:checkin(Info, Ref, died),
                    {error, Reason};
                {Client2, Response = {exception, _}} ->
                    statsderl:timing([BaseKey,"cassanderl.call.exception"], timer:now_diff(os:timestamp(), Timestamp) div 1000, 0.005),
                    dispcount:checkin(Info, Ref, Client2),
                    Response;
                {Client2, Response = {error, _}} ->
                    statsderl:timing([BaseKey,"cassanderl.call.error"], timer:now_diff(os:timestamp(), Timestamp) div 1000, 0.005),
                    dispcount:checkin(Info, Ref, Client2),
                    Response;
                {Client2, Response} ->
                    statsderl:timing([BaseKey,"cassanderl.call.ok"], timer:now_diff(os:timestamp(), Timestamp) div 1000, 0.005),
                    dispcount:checkin(Info, Ref, Client2),
                    Response
            catch
                error:Reason ->
                    dispcount:checkin(Info, Ref, died),
                    {error, Reason}
            end;
        {error, busy} ->
            {error, busy};
        {error, econnrefused} ->
            {error, econnrefused}
    end.

set_keyspace(Info, Keyspace) ->
    call(Info, set_keyspace, [Keyspace]).

get(Info, Key, ColumnFamily, SuperColumn, Column, ConsistencyLevel) ->
    ColumnPath = #columnPath {
        column_family = ColumnFamily,
        super_column = SuperColumn,
        column = Column
    },
    call(Info, get, [Key, ColumnPath, ConsistencyLevel]).

insert(Info, Key, ColumnFamily, SuperColumn, Name, Value, Timestamp, Ttl, ConsistencyLevel) ->
    ColumnParent = #columnParent {
        column_family = ColumnFamily,
        super_column = SuperColumn
    },
    Column = #column {
        name = Name,
        value = Value,
        timestamp = Timestamp,
        ttl = Ttl
    },
    call(Info, insert, [Key, ColumnParent, Column, ConsistencyLevel]).

describe_keyspace(Info, Keyspace) ->
    call(Info, describe_keyspace, [Keyspace]).
