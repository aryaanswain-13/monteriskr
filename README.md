# monteriskr

<!-- badges: start -->
[![R-CMD-check](https://github.com/aryaanswain-13/monteriskr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aryaanswain-13/monteriskr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Bayesian demand modelling and Monte Carlo risk analysis for events.

Event attendance is a count that is capped by venue capacity. `monteriskr` fits a
negative binomial demand model with [brms](https://paulbuerkner.com/brms/),
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

### Example: a college fest committee

`past_events.csv`, one row per past event:

| event | day_type | marketing_k | venue_capacity | attendance |
|---|---|---|---|---|
| Freshers Night | weekend | 4.0 | 300 | 300 |
| Quiz Evening | weekday | 0.5 | 120 | 78 |
| Guest Lecture | weekday | 1.0 | 120 | 95 |
| Dance Night | weekend | 2.5 | 300 | 300 |

```r
past <- read.csv("past_events.csv")
fit  <- fit_demand(past, outcome = "attendance",
                   covariates = c("day_type", "marketing_k"),
                   capacity = "venue_capacity")
```

Freshers Night recorded 300 with capacity 300, so it counts as sold out: true
demand was *at least* 300. The model uses that as a lower bound instead of
treating 300 as the exact demand. Columns the model is not told about (here
`event`) are ignored.

The planned events go in a second file with the same covariate columns and no
outcome (`day_type`, `marketing_k`, `venue_capacity`), and the category labels
must match the historical data exactly (`weekday` / `weekend`).

### Scale your numeric covariates

The default prior is `normal(0, 1)` on each coefficient, which is only weakly
informative *per unit of the covariate*: it assumes a one-unit change shifts
log-demand by roughly 2 or less. Use units where that is plausible, for example
marketing spend in thousands of rupees (as above) or standardised to mean 0 and
standard deviation 1. With raw rupees the prior becomes effectively flat (sampling
can be slow or unstable); with very coarse units (such as lakhs) it can shrink a
real effect. You can change the prior with the `prior` argument of `fit_demand()`.

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

## Check the fit, and known limitations

Always check the sampler before trusting a fit:

```r
np <- brms::nuts_params(fit$brmsfit)
sum(np$Value[np$Parameter == "divergent__"])   # want 0
max(brms::rhat(fit$brmsfit), na.rm = TRUE)     # want below 1.01
```

* **Mostly sold-out history.** Sold-out events only tell the model that demand
  was *at least* capacity. If most of your events sold out, there is little to
  learn from, estimates are weakly determined, and the sampler can produce many
  divergent transitions. In tests on real college football attendance, a team that
  sold out 65% to 98% of the time (depending on how "sold out" was defined) gave
  hundreds of divergences; a team at 44% fit cleanly, but one at 47% did not, so
  there is no safe threshold. If diagnostics are bad, try fewer covariates, more
  iterations, or `control = list(adapt_delta = 0.95)` (passed on to `brms`).
* **What counts as "sold out".** The package treats `attendance >= capacity` as
  sold out. If "full" in your data really means 98-99% of listed capacity, the
  model will treat those games as unconstrained and tend to over-predict sell-outs.
  You can set capacity to an effective value (e.g. 98% of listed capacity).
* **No time trend or repeated-venue structure.** Games are treated as exchangeable
  given the covariates, so a drift in attendance over seasons is not captured
  unless you add a covariate for it.
* **Covariates must vary.** A covariate that is constant in your data is rejected
  by `check_data()`.

## Scope (v0.1)

One count outcome, covariates, optional capacity censoring. No time-series
structure, no multiple outcomes, no web app. Prices and costs are user
assumptions; price changes are not modelled unless price is a covariate.
