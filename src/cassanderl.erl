-module(cassanderl).
-behaviour(gen_server).
-include("cassanderl.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 500).

-record(state, {
    hostname,
    port,
    client
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, call/3, set_keyspace/1, set_keyspace/2, get/5, 
        get/6, insert/8, insert/9, describe_keyspace/1, describe_keyspace/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).
    
call(Function, Args, Timeout) ->
    case red() of
        true ->
            {error, too_busy};
        false ->
            increase_pending_requests(),
            Worker = cassanderl_sup:pick_worker(),
            Response = 
                try gen_server:call(Worker, {call, Function, Args}, Timeout) of
                    Reply -> Reply
                catch
                    exit:_ ->
                        {error, timeout}
                end, 
            decrease_pending_requests(),
            Response
    end.
    
set_keyspace(Keyspace) ->
    set_keyspace(Keyspace, ?DEFAULT_TIMEOUT).

set_keyspace(Keyspace, Timeout) ->
    call(set_keyspace, [Keyspace], Timeout).
    
get(Key, ColumnFamily, SuperColumn, Column, ConsistencyLevel) ->
    get(Key, ColumnFamily, SuperColumn, Column, ConsistencyLevel, ?DEFAULT_TIMEOUT).
    
get(Key, ColumnFamily, SuperColumn, Column, ConsistencyLevel, Timeout) ->
    ColumnPath = #columnPath {
        column_family = ColumnFamily,
        super_column = SuperColumn,
        column = Column
    },
    call(get, [Key, ColumnPath, ConsistencyLevel], Timeout).
    
insert(Key, ColumnFamily, SuperColumn, Name, Value, Timestamp, Ttl, ConsistencyLevel) ->
    insert(Key, ColumnFamily, SuperColumn, Name, Value, Timestamp, Ttl, ConsistencyLevel, ?DEFAULT_TIMEOUT).
    
insert(Key, ColumnFamily, SuperColumn, Name, Value, Timestamp, Ttl, ConsistencyLevel, Timeout) ->
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
    call(insert, [Key, ColumnParent, Column, ConsistencyLevel], Timeout).
    
describe_keyspace(Keyspace) ->
    describe_keyspace(Keyspace, ?DEFAULT_TIMEOUT).
    
describe_keyspace(Keyspace, Timeout) ->
    call(describe_keyspace, [Keyspace], Timeout).
    
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, Hostname} = application:get_env(cassanderl, hostname),
    {ok, Port} = application:get_env(cassanderl, port),
    State = #state {
        hostname = Hostname,
        port = Port  
    },
    {ok, State}.

handle_call(Msg, From, #state{hostname=Hostname, port=Port, client=undefined}=State) ->
    case new_thrift_client(Hostname, Port) of 
        undefined ->
            {reply, {error, econnrefused}, State};
        Client ->
            handle_call(Msg, From, State#state{client=Client})
    end;
handle_call({call, Function, Args}, _From, #state{client=Client}=State) ->
    case thrift_client:call(Client, Function, Args) of
        {error, Reason} ->
            {reply, {error, Reason}, State#state{client=undefined}};
        {Client2, Response} ->
            {reply, Response, State#state{client=Client2}}
    end;
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

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

new_thrift_client(Hostname, Port) ->
    case thrift_client_util:new(Hostname, Port, cassandra_thrift, [{framed, true}]) of
        {ok, Client} ->
            {Client2, {ok, ok}} = thrift_client:call(Client, set_keyspace, ["AdGear"]),
            Client2;
        {error, _} ->
            undefined
    end.
    
red() ->
    Requests = ets:lookup_element(cassanderl, pending_requests, 2),
    Low = ets:lookup_element(cassanderl, low_pending, 2),
    High = ets:lookup_element(cassanderl, high_pending, 2),
    case Requests of
        _ when Low >= Requests -> 
            false;
        _ when Requests >= High  ->
            true;
        _ ->
            random_drop(High - Requests)
    end.

random_drop(Distribution) ->
    case erlang:phash2({self(), now()}, Distribution) + 1 of
        Distribution ->
            true;
        _ ->
            false
    end.
    
increase_pending_requests() ->
    ets:update_counter(cassanderl, pending_requests, 1).
    
decrease_pending_requests() ->
    ets:update_counter(cassanderl, pending_requests, -1).
    
            

    