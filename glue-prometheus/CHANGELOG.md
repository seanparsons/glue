# Changelog for glue-prometheus

## 0.8.0.0

- First release: Prometheus metrics through prometheus-client for services
  (`instrumentService`, `instrumentServiceEither`) and for glue's combinators through their
  observer hooks: circuit breaker phase and transitions, retries and their waits, keyed
  concurrency slots and refusals, hedge launches, attempt outcomes and winners, and graceful
  shutdown in-flight counts (#15).
