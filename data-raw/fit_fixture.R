# Script to generate saved fit fixture for test suite speed
source("R/simulate_events.R")
source("R/check_data.R")
source("R/fit_demand.R")

load("data/example_events.rda")

dir.create("tests/testthat/fixtures", recursive = TRUE, showWarnings = FALSE)

example_fit <- fit_demand(
  data = example_events,
  outcome = "attendance",
  covariates = c("day_type", "marketing"),
  capacity = 250,
  chains = 2,
  iter = 500,
  warmup = 250,
  seed = 123
)

saveRDS(example_fit, file = "tests/testthat/fixtures/example_fit.rds")
