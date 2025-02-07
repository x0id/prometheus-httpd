# Prometheus.io inets httpd exporter #

[![Hex.pm](https://img.shields.io/hexpm/v/prometheus_httpd.svg?maxAge=2592000?style=plastic)](https://hex.pm/packages/prometheus_httpd)
[![Hex.pm](https://img.shields.io/hexpm/dt/prometheus_httpd.svg?maxAge=2592000)](https://hex.pm/packages/prometheus_httpd)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/prometheus_httpd/)
[![GitHub Actions](https://github.com/prometheus-erl/prometheus-httpd/actions/workflows/ci.yml/badge.svg)](https://github.com/prometheus-erl/prometheus-httpd/actions/workflows/ci.yml)
[![Codecov](https://codecov.io/github/prometheus-erl/prometheus-httpd/graph/badge.svg?token=G9HB5UKNIY)](https://codecov.io/github/prometheus-erl/prometheus-httpd)

Provides [httpd middleware](http://erlang.org/doc/man/httpd.html) "mod-module" (`prometheus_httpd`) for exposing [Prometheus.io](https://github.com/prometheus-erl/prometheus.erl) metrics in various formats.

Also can start its own httpd instance with just `prometheus_httpd` enabled.

## Usage

```
prometheus_httpd:start()
```

More in the `prometheus_httpd` module [documentation](https://hexdocs.pm/prometheus_httpd/prometheus_httpd.md)

![BEAM Dashboard](https://raw.githubusercontent.com/prometheus-erl/beam-dashboards/master/BEAM.png)

- IRC: #erlang on Freenode; 
- [Slack](https://elixir-slackin.herokuapp.com/): #prometheus channel - [Browser](https://elixir-lang.slack.com/messages/prometheus) or App(slack://elixir-lang.slack.com/messages/prometheus).

## Integrations
- [Ecto Instrumenter](https://hex.pm/packages/prometheus_ecto)
- [Erlang client](https://github.com/prometheus-erl/prometheus.erl)
- [Elixir client](https://github.com/prometheus-erl/prometheus.ex)
- [Elixir plugs Instrumenters and Exporter](https://hex.pm/packages/prometheus_plugs)
- [Extatus - App to report metrics to Prometheus from Elixir GenServers](https://github.com/gmtprime/extatus)
- [Fuse plugin](https://github.com/jlouis/fuse#fuse_stats_prometheus)
- [OS process info Collector](https://hex.pm/packages/prometheus_process_collector) (linux-only)
- [Phoenix Instrumenter](https://hex.pm/packages/prometheus_phoenix)
- [RabbitMQ Exporter](https://github.com/prometheus-erl/prometheus_rabbitmq_exporter).

## Dashboards

- [Beam Dashboards](https://github.com/prometheus-erl/beam-dashboards).

## Blogs

- [Monitoring Elixir apps in 2016: Prometheus and Grafana](https://aldusleaf.org/monitoring-elixir-apps-in-2016-prometheus-and-grafana/)
- [A Simple Erlang Application, with Prometheus](http://markbucciarelli.com/2016-11-23_a_simple_erlang_application_with_prometheus.html).
