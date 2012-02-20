## Cassanderl version 0.4 ##

{ok, Config} = cassanderl_sup:get_info().
cassanderl:call(Config, describe_keyspace, ["keyspace1"]).

TODO