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

    {ok, Config} = cassanderl_sup:get_info().
    cassanderl:call(Config, describe_keyspace, ["keyspace1"]).

