source("R/simulate_events.R")

# Script to generate shipped dataset example_events
set.seed(42)
example_events <- simulate_events(
  n = 60,
  beta = c(intercept = 5.2, weekend = 0.4, marketing = 0.25),
  phi = 25,
  capacity = 250,
  seed = 42
)

dir.create("data", showWarnings = FALSE)
save(example_events, file = "data/example_events.rda", compress = "xz")
