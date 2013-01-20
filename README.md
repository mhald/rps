rps
===

Request Per Second Tracker for Erlang

This implements a feed-forward request per second counter.  For the initial launch please pass in the stats you want to track.

```
gen_info:start_link().
rps_sup:start_link([ [{name, api_call}, {module, gen_info}, {time, 5000}, {send, stats}] ]).
```

A callback example has been included called gen_info to show the handle_cast that receives the stats.

The RPS supervisor takes a property list of 

* name   The name of the event
* module The callback module to send statistics
* time   The interval for sending stats to the callback
* send   The type of data, either raw or stats


