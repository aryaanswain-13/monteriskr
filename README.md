# monteriskr

<!-- badges: start -->
[![R-CMD-check](https://github.com/aryaanswain-13/monteriskr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aryaanswain-13/monteriskr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Bayesian demand modelling and Monte Carlo risk analysis for events.

Event attendance is a count that is capped by venue capacity. `monteriskr` fits a
negative binomial demand model with [brms](https://paul-buerkner.github.io/brms/),
treats sold-out events as **right-censored** (true demand was at least capacity),
simulates profit from the posterior, and reports risk metrics (VaR, CVaR, loss
probability) for a plan or for several alternative plans.

## Installation

```r
# install.packages("remotes")
remotes::install_github("aryaanswain-13/monteriskr")
```

Fitting needs a working Stan toolchain (`rstan`); see the
[rstan installation guide](https://mc-stan.org/rstan/). The first fit compiles a
Stan model and takes about a minute.

## Workflow

```r
library(monteriskr)

events <- read.csv("my_events.csv")   # one row per past event

check_data(events, outcome = "attendance",
           covariates = c("day_type", "marketing"), capacity = 250)

fit <- fit_demand(events, outcome = "attendance",
                  covariates = c("day_type", "marketing"), capacity = 250)

plan <- data.frame(day_type  = factor(c("weekday", "weekend"),
                                      levels = c("weekday", "weekend")),
                   marketing = c(0, 1))

risk <- simulate_risk(fit, plan, n_sims = 4000, price = 40,
                      variable_cost = 5, fixed_cost = 3000)
risk_summary(risk, level = 0.95)

# Compare alternative plans (the first is the baseline)
compare_scenarios(current = risk, other_plan = risk2)
```

See the vignette (`vignette("monteriskr")`) for a full worked example that runs
without fitting.

## Data format

A data frame with one row per past event:

* an outcome column of non-negative whole numbers (e.g. `attendance`), no blanks;
* optional covariate columns, no blanks (numeric covariates not constant, factors
  with at least two levels);
* optional capacity: one number, or the name of a column of positive values.

At least 8 rows are required (20 or more recommended). Rows where attendance
equals capacity are treated as sold out. Do not name a column `censored`.

## Functions

| Function | Purpose |
|---|---|
| `check_data()` | Validate data before fitting |
| `fit_demand()` | Bayesian negative binomial fit with capacity censoring |
| `simulate_risk()` | Posterior Monte Carlo simulation of demand, attendance, profit |
| `risk_summary()` | Mean, quantiles, loss probability, VaR and CVaR |
| `compare_scenarios()` | Side-by-side risk metrics for several scenarios |
| `simulate_events()` | Generate synthetic event data |

Loss is `-profit`; VaR is the `level`-quantile of loss and CVaR is the mean loss
at or beyond VaR.

## Does the censoring help?

A parameter-recovery study simulated 50 datasets of 150 events per scenario with
known parameters. With about 25% of events sold out, the censored model was close
to unbiased (weekend effect bias +0.01, 90% interval coverage 92%) while a naive
model that ignores capacity was biased (weekend effect bias -0.09, coverage 23%;
shape parameter overestimated by about 10). With almost no sell-outs the two
models agreed. The script is in `data-raw/validation.R`; results ship in
`inst/extdata/validation_results.rds`.

## Scope (v0.1)

One count outcome, covariates, optional capacity censoring. No time-series
structure, no multiple outcomes, no web app. Prices and costs are user
assumptions; price changes are not modelled unless price is a covariate.
