# monteriskr 0.0.0.9000

First development release.

* `simulate_events()` generates synthetic event data; `example_events` is a
  bundled example.
* `check_data()` validates event data before fitting.
* `fit_demand()` fits a Bayesian negative binomial demand model with brms.
  Sold-out events are treated as right-censored. Sampling starts from
  data-informed initial values, and `fit_demand()` now stops with an error if no
  chain produces draws (previously a fit with zero draws could be returned).
* `simulate_risk()` simulates demand, attendance, revenue and profit from the
  posterior for a planned set of events.
* `risk_summary()` and `compare_scenarios()` report mean, quantiles, loss
  probability, VaR and CVaR for one or several scenarios.
* A parameter-recovery study (`data-raw/validation.R`) compares the censored
  model with a naive model that ignores capacity.
* Added a vignette and README.
