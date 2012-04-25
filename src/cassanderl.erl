-module(cassanderl).
-include("cassanderl.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([get_info/0, call/2, call/3, set_keyspace/2, get/6, insert/9, describe_keyspace/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

get_info() ->
    cassanderl_sup:get_info().

call(Function, Args) ->
    {ok, Config} = get_info(),
    call(Config, Function, Args).

call(Info = {config, _, _, _, _, _}, Function, Args) ->
    case dispcount:checkout(Info) of
        {ok, Ref, Client} ->
            case call(Client, Function, Args) of
                {undefined, Response} ->
                    dispcount:checkin(Info, Ref, died),
                    Response;
                {Client2, Response} ->
                    dispcount:checkin(Info, Ref, Client2),
                    Response
            end;
        {error, Reason} ->
            {error, Reason}
    end;

call(Client = {tclient, _, _, _}, Function, Args) ->
    {ok, BaseKey} = application:get_env(adgear_gateway, statsderl_key),
    Timestamp = os:timestamp(),
    try thrift_client:call(Client, Function, Args) of
        {Client2, Response = {ok, _}} ->
            statsderl:timing([BaseKey,"cassanderl.call.ok"], timer:now_diff(os:timestamp(), Timestamp) div 1000, 0.005),
            {Client2, Response};
        {_,  Response = {error, econnrefused}} ->
            {undefined, Response};
        {_,  Response = {error, closed}} ->
            {undefined, Response};
        {Client2, Response = {error, _}} ->
            statsderl:timing([BaseKey,"cassanderl.call.error"], timer:now_diff(os:timestamp(), Timestamp) div 1000, 0.005),
            {Client2, Response}
    catch
        Exception:Reason ->
            case {Exception, Reason} of
                {throw, {Client2, Response = {exception, _}}} ->
                    statsderl:timing([BaseKey,"cassanderl.call.exception"], timer:now_diff(os:timestamp(), Timestamp) div 1000, 0.005),
                    {Client2, Response}
            end
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
