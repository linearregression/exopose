exopose
=================

## Introduction

`exopose` is a simple application that leverages [`exometer`](https://github.com/Feuerlabs/exometer) in such a way that can be integrated as an unique Erlang application in any Erlang application.

`exopose` includes default configuration for `exometer` where `exometer_report_collectd` reporter is pre-configured. That config includes the following definitions of Erlang VM metrics,

```erlang
{predefined,
 [{[erlang, processes], gauge, []},
  {[erlang, system],    gauge, []},
  {[erlang, atom],      gauge, []},
  {[erlang, binary],    gauge, []},
  {[erlang, ets],       gauge, []},
  {[erlang, run_queue], gauge, []}
]}
```

## Features

- Metric sampling mechanism to provide value updates to `exometer`.
- Configuration compatibility with `exometer` environment variables.
- Pre-configured Erlang VM metrics monitoring.
- Pre-configured data reporting to `collectd` daemon (Fallback to silent app behavior should target host not have `collectd` unix socket configured)

## Topology

- `exopose_app` implements `application` behavior being in charged of starting a custom `collectd` reporter if the corresponding `collectd` unix socket is ready given the provided configuration in the `app.src` file unless it's overwritten by a general `.config` file.
- `exopose_sup` supervises `exopose` `gen_server` that samples defined metrics every `TIMEOUT` and update their values.

## Dependencies

- [`exometer`](https://github.com/Feuerlabs/exometer)

## Installation

As simple as the following two steps:

1. Add `exopose` to your `rebar.config`.
2. Include `exopose` to `applications` list in your `app.src` file.

Note that as side-effect of depending on `exometer` the following sub-dependencies will be fetched. If you're looking at dependency locking your code base, you might want to lock the following extra dependencies to a specific version:

- `afunix`
- `bear`
- `folsom`
- `netlink`
- `setup`

## Usage

```erlang
> erl -pa ebin -pa deps/*/ebin -s exopose_app
Erlang/OTP 17 [erts-6.2.1] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.2.1  (abort with ^G)
1> 15:14:08.470 [info] Application lager started on node nonode@nohost
15:14:08.491 [info] Starting reporters with []
15:14:08.491 [info] Application exometer started on node nonode@nohost
15:14:08.496 [info] exopose has started monotoring 6 metrics
15:14:08.496 [info] Application exopose started on node nonode@nohost
```

The call `exopose:i/0` returns general information about the current configuration and state of the application,

```erlang
1> exopose:i().
[{total,6},
 {exometer_reporters,[]},
 {counters,[]},
 {histograms,[]},
 {gauges,[[vm,erlang,atom],
          [vm,erlang,binary],
          [vm,erlang,ets],
          [vm,erlang,processes],
          [vm,erlang,run_queue],
          [vm,erlang,system]]},
 {callbacks,[{[vm,erlang,processes],
              {{erlang,memory,1},[processes]}},
             {[vm,erlang,system],{{erlang,memory,1},[system]}},
             {[vm,erlang,atom],{{erlang,memory,1},[atom]}},
             {[vm,erlang,binary],{{erlang,memory,1},[binary]}},
             {[vm,erlang,ets],{{erlang,memory,1},[ets]}},
             {[vm,erlang,run_queue],
             {{erlang,statistics,1},[run_queue]}}]}]
```

Default configured Erlang VM are returned by `exopose:vm/0`,

```erlang
2> exopose:vm().
[{[vm,erlang,atom],339441},
 {[vm,erlang,binary],36016},
 {[vm,erlang,ets],462816},
 {[vm,erlang,processes],5068480},
 {[vm,erlang,run_queue],0},
 {[vm,erlang,system],13260024}]
```     

## Managing metrics

You can use `exopose` API to create `gauges`, `counters` and `histograms`. For further info, browse documentation inside `src/exopose.erl`. `exopose` delegates on `exometer` the responsability to keep track of existing metrics, not keeping a state for such purpose. 

```erlang
3> exopose:new_counter([test,counter]).
ok
15:14:54.304 [info] exopose has installed a new counter: [test,counter]
4> exopose:incr([test,counter]).
ok
5> exopose:incr([test,counter]).
ok
6> exopose:get_counters().
[[{name,[test,counter]},
  {value,2},
  {ms_since_reset,15401}]]  
```

Note that both `gauges` and `histogram` behave similarly in such a way that both need to be provided with a function callback,

```erlang
7> exopose:new_gauge([test,gauge], {fun() -> {_,_,MS} = erlang:now(), MS end, []}).
ok
15:34:28.414 [info] exopose has installed a new gauge: [test,gauge]
8> exopose:get_gauges().
[[{name,[test,gauge]},
  {value,688761},
  {ms_since_reset,36866}],
 [...]]
```

Callbacks can be provided in two ways, (see `include/exopose.hrl` for type definitions). First parameter can be either `mfa()` or `function()`, and second parameter is always expected as a list of atoms, which stands for the parameter list for the function provided. Callbacks are stored in memory, and therefore non-fault-tolerant. If they are provided as part of the initial configuration (`.config` or `app.src` files) they will always be available). If callback is not found, `exopose` will not provide `exometer` with any update.

## Metric automatic sampling

By default, `exopose` server samples metrics every 10 seconds, that can be configured with `exopose:set_timeout/1` (milisecond as time unit). Current timeout value is returned through `exopose:get_timeout/0`.

```erlang
15:24:18.963 [info] Sampling metrics...
15:24:28.969 [info] Sampling metrics...
15:24:38.971 [info] Sampling metrics...
7> 15:30:39.106 [info] Sampling metrics...
exopose:set_timeout(100000).
ok
```

When this process happens, `gauges` and `histograms` are sampled, in other words, their current values get updated with what their callbacks return, and `exometer` updates their records.

The value of the sampling timeout can be customized during deployment by overwriting `sample_timeout` variable in the `exopose` Application.