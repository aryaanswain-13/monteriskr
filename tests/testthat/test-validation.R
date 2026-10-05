test_that("censored model recovers parameters; naive model on capped data is biased", {
  skip_on_cran()

  truth <- c(b_Intercept = 5, b_day_typeweekend = 0.3, b_marketing = 0.2, shape = 20)
  df <- simulate_events(n = 200, capacity = 200, seed = 2024)
  expect_gt(mean(df$sold_out), 0.15)  # heavy censoring scenario

  covs <- c("day_type", "marketing")
  fit_cens <- suppressWarnings(fit_demand(df, "attendance", covs, capacity = 200,
                                          chains = 2, iter = 1000, seed = 1))
  fit_naive <- suppressWarnings(fit_demand(df, "attendance", covs, capacity = NULL,
                                           chains = 2, iter = 1000, seed = 1))

  get_draws <- function(f) as.data.frame(f$brmsfit)[, names(truth)]
  dc <- get_draws(fit_cens)
  dn <- get_draws(fit_naive)

  # Censored model: 90% credible intervals contain the true values
  for (p in names(truth)) {
    ci <- stats::quantile(dc[[p]], c(0.05, 0.95), names = FALSE)
    expect_true(ci[1] <= truth[[p]] && truth[[p]] <= ci[2], info = p)
  }

  # Naive model treats capped attendance as true demand: it understates the
  # weekend effect and overstates the shape (less dispersion) relative to truth
  expect_lt(mean(dn$b_day_typeweekend), mean(dc$b_day_typeweekend))
  expect_lt(mean(dn$b_day_typeweekend), truth[["b_day_typeweekend"]])
  expect_gt(mean(dn$shape), truth[["shape"]])
})

test_that("saved validation study supports the censoring claims", {
  path <- system.file("extdata", "validation_results.rds", package = "monteriskr")
  skip_if(path == "", "validation results not installed")
  s <- readRDS(path)$summary
  get <- function(sc, m, p) s[s$scenario == sc & s$model == m & s$parameter == p, ]

  # Heavy censoring: the censored model is close to unbiased on every parameter
  for (p in c("Intercept", "day_typeweekend", "marketing")) {
    expect_lt(abs(get("heavy_censoring", "censored", p)$bias), 0.02)
  }
  expect_lt(abs(get("heavy_censoring", "censored", "shape")$bias), 1)
  expect_true(all(s$n_fits[s$model == "censored"] == 50))  # all converged

  # ... while the naive model understates the weekend and marketing effects
  # and overstates the shape, with poor interval coverage
  expect_lt(get("heavy_censoring", "naive", "day_typeweekend")$bias, -0.05)
  expect_lt(get("heavy_censoring", "naive", "marketing")$bias, -0.03)
  expect_gt(get("heavy_censoring", "naive", "shape")$bias, 5)
  expect_lt(get("heavy_censoring", "naive", "marketing")$coverage90, 0.3)

  # Light censoring: both models agree (almost nothing is censored)
  for (p in c("Intercept", "day_typeweekend", "marketing")) {
    expect_lt(abs(get("light_censoring", "naive", p)$bias -
                    get("light_censoring", "censored", p)$bias), 0.01)
  }
})
