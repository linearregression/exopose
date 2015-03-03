# Exopose

Exopose is an Erlang library to easily manage [`exometer`](https://github.com/Feuerlabs/exometer) metrics and callbacks to sample gauges. It is statically configured to monitor Erlang system metrics by default.

# Features

Exopose is tightly coupled to `exometer_core`, this being the only dependency. You can easily add it to any Erlang application, getting:

- Preconfigured monitoring of Erlang VM metrics.
- Management of function callbacks for metrics.
- Auto-sampling mechanism.

## Default Erlang VM monitoring

Exopose is [preconfigured](https://github.com/layerhq/exopose/blob/master/src/exopose.app.src) with a list of the most representative Erlang VM metrics. `exometer_core` will read these entries and create the corresponding metric objects. In addition, `exopose` will setup metrics sampling through callbacks defined under `callbacks` variable.

```
$ erl -pa ebin/ -pa deps/*/ebin -s exopose_app
Erlang/OTP 17 [erts-6.2.1] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.2.1  (abort with ^G)
1> 17:21:06.443 [info] Application lager started on node nonode@nohost
17:21:06.455 [info] Starting reporters with []
17:21:06.455 [info] Application exometer_core started on node nonode@nohost
17:21:06.459 [info] exopose has started monotoring 11 metrics
17:21:06.459 [info] Application exopose started on node nonode@nohost
1> exopose:vm().

[{[vm,erlang,atom],339441},
 {[vm,erlang,atom_used],319944},
 {[vm,erlang,binary],30184},
 {[vm,erlang,ets],467216},
 {[vm,erlang,processes],5177672},
 {[vm,erlang,processes_used],5183848},
 {[vm,erlang,system],13239848},
 {[vm,erlang,total],18499624},
 {[sys,erlang,port_count],7},
 {[sys,erlang,process_count],60},
 {[sys,erlang,run_queue],0}]
```

## Metric and function callback management

Metrics can be statically or dinamically configured.

- For static configuration, see [`exopose.app.src`](https://github.com/layerhq/exopose/blob/master/src/exopose.app.src) as example.
- You can use `exopose` API to dinamically create `gauges`, `counters` and `histograms`. `exopose` delegates on `exometer` the responsability to keep track of existing metrics, only keeping information about sampling, callbacks, etc. Metrics can be created by calling the functions `exopose:new_counter/1`, `exopose:new_gauge/2` or `exopose:new_histogram/2`, e.g:

```erlang
1> exopose:new_counter([test,counter]).
ok
20:14:13.728 [info] exopose has installed a new counter: [test,counter]
2> exopose:incr([test,counter]).
ok
3> exopose:incr([test,counter]).
ok
4> exopose:get_counters().
[[{name,[test,counter]},{value,2},{ms_since_reset,23319}]]
```

When it comes to `gauges` and `histogram`, note that both need to be provided with a function callback, e.g:

```erlang
5> exopose:new_gauge([test,gauge], {fun() -> {_,_,MS} = erlang:now(), MS end, []}).
ok
20:15:46.685 [info] exopose has installed a new gauge: [test,gauge]
6> exopose:get_gauges().
[[{name,[test,gauge]},{value,900886},{ms_since_reset,11224}],
 ...]
```

Callbacks can be provided in two ways, (see `include/exopose.hrl` for type definitions). First parameter can be either `mfa()` or `function()`, and second parameter is always expected as a list of atoms, which stands for the parameter list for the function provided. Callbacks are stored in memory, and therefore non-fault-tolerant. If they are provided as part of the initial configuration (`.config` or `app.src` files) they will always be available). If callback is not found, `exopose` will not send any updates to the `exometer` entry object.

## Metrics sampling mechanism

Gauges and histograms are continuously sampled and their values are updated through `exometer:update/2` interface. The sampling frequency can be statically configured with the variable `sample_timeout` defaulted to 10 seconds or dinamically by calling `exopose:set_timeout/1`. Current timeout value is returned through `exopose:get_timeout/0`. When this process happens, `gauges` and `histograms` are sampled, that is, their current values get updated as result of evaluating their callbacks.

# Compiling and running tests

```
$ make
./rebar compile
....
Compiled src/exopose_sup.erl
Compiled src/exopose_app.erl
Compiled src/exopose_util.erl
Compiled src/exopose.erl

$ make tests
./rebar eunit skip_deps=true
WARN:  Ignoring sub_dirs for /Users/juan/layer/exopose/deps/exometer_core
==> exopose (eunit)
Compiled src/exopose_sup.erl
Compiled src/exopose_app.erl
Compiled src/exopose_util.erl
Compiled test/exopose_tests.erl
Compiled src/exopose.erl
...
```

# Reading application and metrics information

The call `exopose:i/0` returns general information about the current configuration and state of the application,

```erlang
> erl -pa ebin -pa deps/*/ebin -s exopose_app
...
1> exopose:i().
[{total,11},
{exometer_reporters,[]},
{counters,[]},
{histograms,[]},
{gauges,[[sys,erlang,port_count],
         [sys,erlang,process_count],
         [sys,erlang,run_queue],
         [vm,erlang,atom],
         [vm,erlang,atom_used],
         [vm,erlang,binary],
         [vm,erlang,ets],
         [vm,erlang,processes],
         [vm,erlang,processes_used],
         [vm,erlang,system],
         [vm,erlang,total]]},
 {sample_timeout,5000},
 {callbacks,[
        {[vm,erlang,total],{{erlang,memory,1},[total]}},
        {[vm,erlang,processes],{{erlang,memory,1},[processes]}},
        {[vm,erlang,processes_used],{{erlang,memory,1},[processes_used]}},
        {[vm,erlang,system],{{erlang,memory,1},[system]}},
        {[vm,erlang,atom],{{erlang,memory,1},[atom]}},
        {[vm,erlang,atom_used],{{erlang,memory,1},[atom_used]}},
        {[vm,erlang,binary],{{erlang,memory,1},[binary]}},
        {[vm,erlang,ets],{{erlang,memory,1},[ets]}},
        {[sys,erlang,process_count],{{erlang,system_info,1},[process_count]}},
        {[sys,erlang,port_count],{{erlang,system_info,1},[port_count]}},
        {[sys,erlang,run_queue],{{erlang,statistics,1},[run_queue]}}]}]
```

Use `get_counters/0`, `get_gauges/0` and `get_histograms/0` to return metrics information, e.g:

```erlang
1> exopose:new_counter([test,counter,1]).
ok
20:44:20.071 [info] exopose has installed a new counter: [test,counter,1]
2> exopose:new_counter([test,counter,2]).
ok
20:44:22.407 [info] exopose has installed a new counter: [test,counter,2]
3> exopose:get_counters().
[[{name,[test,counter,1]},{value,0},{ms_since_reset,10167}],
 [{name,[test,counter,2]},{value,0},{ms_since_reset,7831}]]
4> exopose:incr([test,counter,2]).
ok
5> exopose:get_counters().
[[{name,[test,counter,1]},{value,0},{ms_since_reset,20201}],
 [{name,[test,counter,2]},{value,1},{ms_since_reset,17865}]]
6> exopose:get_histograms().
[...]
7> exopose:get_gauges().
[...]
```

Default configured Erlang VM are returned by `exopose:vm/0`.

# License

[Apache License, Version 2.0](https://github.com/layerhq/exopose/blob/master/LICENSE)

# Changelog

- 0.1.0: `exopose` to depend only on `exometer_core`
- 0.0.1: initial commit