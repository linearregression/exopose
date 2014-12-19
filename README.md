layer-erl-metrics
=================

# Introduction

Layer Erlang Metrics, a.k.a `lyr_metrics`, is a simple application that leverages [`exometer`](https://github.com/Feuerlabs/exometer) to be integrated as an unique Erlang application in any of the Layer Erlang Systems (e.g: `tmc`, `shift`, `ctrl`).

Given the described [Operational Metrics](https://github.com/layerhq/docs/blob/operational_metrics/infra/metrics/README.md) document,  `collectd` is the daemon that glues stats collection tools such Exometer, and Kafka broker. Therefore, `lyr_metrics` includes default environment configuration for `exometer` where `exometer_report_collectd` reporter is pre-configured. That config includes definitions of Erlang VM metrics,

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

# Features

- Compatible with exometer official environment variables.
- Out-of-the-box Erlang VM metrics monitoring.
- Data reporting to collectd daemon.
- OS hostname based custom reporter (specially designed for Docker containers).
- Fallback to silent app behavior should target host not have `collectd` configured.

# Topology

- `lyr_metrics_app` implements `application` behavior being in charged of starting a custom `collectd` reporter if the corresponding `collectd` unix socket is ready given the provided configuration in the `app.src` file unless it's overwritten by a general `.config` file.
- `lyr_metrics_sup` supervises `lyr_metrics` `gen_server` that samples defined gauges every `TIMEOUT` and update their values, besides exposing an interface to create and manipulate counters.

# Dependencies

- Exometer, listed in `rebar.config`.

# Integration

As simple as the following two steps:

1. Add `lyr_metrics` to your `rebar.config`.
2. Include `lyr_metrics` to `applications` list in your `app.src`.

Note that if the integrator uses `layer-prebuild` script to fetch dependencies, you will want to lock all sub-dependencies down to your rebar.config, therefore you would need to include as well:

1. `afunix`
2. `bear`
3. `exometer`
4. `folsom`
6. `netlink`
7. `setup`