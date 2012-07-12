## Cassanderl version 0.4 ##

## Configuration ##

To start cassanderl, you will need to configure a section in your
application config file:

     {cassanderl,
       [{hostname, "cassandra.talented-startup.com"},
        {port, 9160},
        {default_keyspace, "big_data"},
        {worker_pool_size, 10}
       ]},

The options are as follows:

* `hostname` - The hostname to connect to
* `port` - The port to connect to. Cassandra RPC is on port 9160 by default
* `default_keyspace` - If you omit setting a keyspace, this keyspace
will be used for a newly formed connection.
* `worker_pool_size` - The size of the worker pool. How many concurrent
resource-connections do you allow to the cassandra cluster.

Second, you will need to start up cassanderl (or make it part of your
boot script):

     application:start(cassanderl).

## Example of Usage ##

To use Cassanderl, you must first ask it for the current
configuration.

    {ok, Config} = cassanderl:get_info().

This will return the current dispatcher configuration. Cassanderl
spawns a pool of dispatchers by default which it then uses whenever
you want to access your Cassandra cluster. The hostname and portname
of the cluster will be the defaults as well when doing this.

To issue a call to Cassandra, issue:

    cassanderl:call(Config, describe_keyspace, ["keyspace1"]).

Which performs a low-level call to Cassandra with the given
configuration, the given (thrift) method and the given parameters to
the call.

The module `cassanderl` has certain helpers as well for often-executed
functions.

### Commands ###

### Add ###

To issue an `add` towards Cassandra, do the following:

     {ok, Config} = cassanderl:get_info(),
     CP = cassanderl:column_parent(<<"superhero_stats">>),
     {ok, CassandraResult} =
       add(Config, <<"gotham city">>, CP, {<<"batmans_spotted">>, 7}, 1).

In this example, we first generate a "column parent" for the column
family. There are no super-columns here, so just referring to the
superhero stats is enough. The `CP` acts like an accessor pattern on
the data we wish to update and can be reused in subsequent calls if we
want.

Finally, we increment a counter on the *Gotham City* row. We spotted
some Batmans. The last parameter is the consistency level desired.

