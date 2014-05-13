-module(cassanderl).
-include("cassanderl.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([column_parent/1, column_parent/2,
         get_info/0,
         slice/2
        ]).

-export([add/5,
         call/2, call/3,
         describe_keyspace/2,
         get/6,
         get_slice/5,
         insert/9,
         set_keyspace/2,
         with_cassandra/2
        ]).

-define(SAMPLING, 0.005).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

get_info() ->
    cassanderl_app:get_info().

column_parent(Family) ->
    #columnParent { column_family = Family }.

column_parent(Super, Family) ->
    #columnParent { super_column = Super,
                    column_family = Family }.

%% @doc Create a slice_range object for slice queries
%% @end
slice(Start, End) ->
    R = #sliceRange { start = Start, finish = End, count = 1000 },
    #slicePredicate { slice_range = R,
                      column_names = undefined }.

call(Function, Args) ->
    {ok, Config} = get_info(),
    call(Config, Function, Args).

%% @doc Execute a set of Cassandra commands with the same connection
%% <p>This call allows you to execute several cassandra calls on the
%% same client connection. The call checks out a resource and then it
%% passes that resource to a function of your choosing. The function
%% can issue thrift_client calls directly to the underlying stack and
%% returns a pair {NewC, Response}.</p>
%% <p>The calling convention is that you end by returning the final
%% Client state for checkin into the dispcount pool again.</p>
%% @end
with_cassandra(Config, F) ->
    case dispcount:checkout(Config) of
        {ok, Ref, Client} ->
            try F(Client) of
                undefined ->
                    dispcount:checkin(Config, Ref, died),
                    ok;
                {error, Reason} ->
                    dispcount:checkin(Config, Ref, died),
                    {error, {'fun', Reason}};
                {ok, Client2} ->
                    dispcount:checkin(Config, Ref, Client2),
                    ok;
                {ok, Res, Client2} ->
                    dispcount:checkin(Config, Ref, Client2),
                    {ok, Res}
            catch
                C:E ->
                    dispcount:checkin(Config, Ref, died),
                    %% Re-raise the exception
                    ST = erlang:get_stacktrace(),
                    erlang:raise(C, E, ST)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

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
    Timestamp = os:timestamp(),
    try thrift_client:call(Client, Function, Args) of
        {Client2, Response = {ok, _}} ->
            statsderl:timing(["cassanderl.call.ok"], timer:now_diff(os:timestamp(), Timestamp) div 1000, ?SAMPLING),
            {Client2, Response};
        {_,  Response = {error, econnrefused}} ->
            {undefined, Response};
        {_,  Response = {error, closed}} ->
            {undefined, Response};
        {Client2, Response = {error, _}} ->
            statsderl:timing(["cassanderl.call.error"], timer:now_diff(os:timestamp(), Timestamp) div 1000, ?SAMPLING),
            {Client2, Response}
    catch
        Exception:Reason ->
            case {Exception, Reason} of
                {throw, {Client2, Response = {exception, _}}} ->
                    statsderl:timing(["cassanderl.call.exception"], timer:now_diff(os:timestamp(), Timestamp) div 1000, ?SAMPLING),
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

add(Config, Key, ColumnParent, {Name, Value}, ConsistencyLevel) ->
    add(Config, Key, ColumnParent, #counterColumn {
      name = Name,
      value = Value }, ConsistencyLevel);
add(Config, Key, ColumnParent, #counterColumn{} = Col, ConsistencyLevel) ->
    call(Config, add, [Key, ColumnParent, Col, ConsistencyLevel]).

%% @doc Call Cassandras `get_slice' command.
%% @end
get_slice(Config, Key, ColumnParent, SliceRange, Consistency) ->
    call(Config, get_slice, [Key, ColumnParent, SliceRange, Consistency]).
