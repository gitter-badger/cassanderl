-ifndef(_cassandra_types_included).
-define(_cassandra_types_included, yeah).

-define(cassandra_ConsistencyLevel_ONE, 1).
-define(cassandra_ConsistencyLevel_QUORUM, 2).
-define(cassandra_ConsistencyLevel_LOCAL_QUORUM, 3).
-define(cassandra_ConsistencyLevel_EACH_QUORUM, 4).
-define(cassandra_ConsistencyLevel_ALL, 5).
-define(cassandra_ConsistencyLevel_ANY, 6).
-define(cassandra_ConsistencyLevel_TWO, 7).
-define(cassandra_ConsistencyLevel_THREE, 8).

-define(cassandra_IndexOperator_EQ, 0).
-define(cassandra_IndexOperator_GTE, 1).
-define(cassandra_IndexOperator_GT, 2).
-define(cassandra_IndexOperator_LTE, 3).
-define(cassandra_IndexOperator_LT, 4).

-define(cassandra_IndexType_KEYS, 0).

-define(cassandra_Compression_GZIP, 1).
-define(cassandra_Compression_NONE, 2).

-define(cassandra_CqlResultType_ROWS, 1).
-define(cassandra_CqlResultType_VOID, 2).
-define(cassandra_CqlResultType_INT, 3).

%% struct column

-record(column, {name :: string() | binary(),
                 value :: string() | binary(),
                 timestamp :: integer(),
                 ttl :: integer()}).

%% struct superColumn

-record(superColumn, {name :: string() | binary(),
                      columns = [] :: list()}).

%% struct counterColumn

-record(counterColumn, {name :: string() | binary(),
                        value :: integer()}).

%% struct counterSuperColumn

-record(counterSuperColumn, {name :: string() | binary(),
                             columns = [] :: list()}).

%% struct columnOrSuperColumn

-record(columnOrSuperColumn, {column = #column{} :: #column{}, 
                              super_column = #superColumn{} :: #superColumn{}, 
                              counter_column = #counterColumn{} :: #counterColumn{}, 
                              counter_super_column = #counterSuperColumn{} :: #counterSuperColumn{}}).

%% struct notFoundException

-record(notFoundException, {}).

%% struct invalidRequestException

-record(invalidRequestException, {why :: string()}).

%% struct unavailableException

-record(unavailableException, {}).

%% struct timedOutException

-record(timedOutException, {}).

%% struct authenticationException

-record(authenticationException, {why :: string()}).

%% struct authorizationException

-record(authorizationException, {why :: string()}).

%% struct schemaDisagreementException

-record(schemaDisagreementException, {}).

%% struct columnParent

-record(columnParent, {column_family :: string(), 
                       super_column :: string()}).

%% struct columnPath

-record(columnPath, {column_family :: string(), 
                     super_column :: string(), 
                     column :: string()}).

%% struct sliceRange

-record(sliceRange, {start :: string() | binary(),
                     finish :: string() | binary(),
                     reversed = false :: boolean(),
                     count = 100 :: integer()}).

%% struct slicePredicate

-record(slicePredicate, {column_names = [] :: list() | undefined,
                         slice_range = #sliceRange{} :: #sliceRange{} | undefined}).

%% struct indexExpression

-record(indexExpression, {column_name :: string(), 
                          op :: integer(), 
                          value :: string()}).

%% struct indexClause

-record(indexClause, {expressions = [] :: list(), 
                      start_key :: string(), 
                      count = 100 :: integer()}).

%% struct keyRange

-record(keyRange, {start_key :: string(), 
                   end_key :: string(), 
                   start_token :: string(), 
                   end_token :: string(), 
                   count = 100 :: integer()}).

%% struct keySlice

-record(keySlice, {key :: string(), 
                   columns = [] :: list()}).

%% struct keyCount

-record(keyCount, {key :: string(), 
                   count :: integer()}).

%% struct deletion

-record(deletion, {timestamp :: integer(), 
                   super_column :: string(), 
                   predicate = #slicePredicate{} :: #slicePredicate{}}).

%% struct mutation

-record(mutation, {column_or_supercolumn = #columnOrSuperColumn{} :: #columnOrSuperColumn{}, 
                   deletion = #deletion{} :: #deletion{}}).

%% struct tokenRange

-record(tokenRange, {start_token :: string(), 
                     end_token :: string(), 
                     endpoints = [] :: list()}).

%% struct authenticationRequest

-record(authenticationRequest, {credentials = dict:new() :: dict()}).

%% struct columnDef

-record(columnDef, {name :: string(), 
                    validation_class :: string(), 
                    index_type :: integer(), 
                    index_name :: string()}).

%% struct cfDef

-record(cfDef, {keyspace :: string(), 
                name :: string(), 
                column_type = "Standard" :: string(), 
                comparator_type = "BytesType" :: string(), 
                subcomparator_type :: string(), 
                comment :: string(), 
                row_cache_size = 0 :: float(), 
                key_cache_size = 200000 :: float(), 
                read_repair_chance = 1 :: float(), 
                column_metadata = [] :: list(), 
                gc_grace_seconds :: integer(), 
                default_validation_class :: string(), 
                id :: integer(), 
                min_compaction_threshold :: integer(), 
                max_compaction_threshold :: integer(), 
                row_cache_save_period_in_seconds :: integer(), 
                key_cache_save_period_in_seconds :: integer(), 
                memtable_flush_after_mins :: integer(), 
                memtable_throughput_in_mb :: integer(), 
                memtable_operations_in_millions :: float(), 
                replicate_on_write :: boolean(), 
                merge_shards_chance :: float(), 
                key_validation_class :: string(), 
                row_cache_provider = "org.apache.cassandra.cache.ConcurrentLinkedHashCacheProvider" :: string(), 
                key_alias :: string()}).

%% struct ksDef

-record(ksDef, {name :: string(), 
                strategy_class :: string(), 
                strategy_options = dict:new() :: dict(), 
                replication_factor :: integer(), 
                cf_defs = [] :: list(), 
                durable_writes = true :: boolean()}).

%% struct cqlRow

-record(cqlRow, {key :: string(), 
                 columns = [] :: list()}).

%% struct cqlResult

-record(cqlResult, {type :: integer(), 
                    rows = [] :: list(), 
                    num :: integer()}).

-endif.
