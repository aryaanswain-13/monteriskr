test_that("fit_demand handles invalid inputs via check_data", {
  df <- simulate_events(n = 5, seed = 1) # < 8 rows
  expect_error(fit_demand(df, outcome = "attendance"), "Data quality check failed")
})

test_that("fit_demand produces valid monteriskr_fit from fixture", {
  fixture_path <- testthat::test_path("fixtures", "example_fit.rds")
  expect_true(file.exists(fixture_path))

  fit <- readRDS(fixture_path)
  expect_s3_class(fit, "monteriskr_fit")
  expect_equal(fit$outcome, "attendance")
  expect_equal(fit$covariates, c("day_type", "marketing"))
  expect_equal(fit$capacity, 250)
  expect_s3_class(fit$brmsfit, "brmsfit")
  expect_s3_class(fit$priors, "brmsprior")

  # Test print and summary methods
  expect_output(print(fit), "monteriskr Bayesian Demand Fit")
  expect_output(print(fit), "Outcome: attendance")
  expect_output(summary(fit), "monteriskr Demand Fit Summary")
})

test_that("fit_demand runs live fitting with censoring and uncensored formula", {
  skip_on_cran()

  df <- simulate_events(n = 25, capacity = 300, seed = 42)

  # Censored model
  fit_cens <- fit_demand(
    data = df,
    outcome = "attendance",
    covariates = "marketing",
    capacity = 300,
    chains = 1,
    iter = 100,
    seed = 123
  )
  expect_s3_class(fit_cens, "monteriskr_fit")
  expect_equal(deparse(fit_cens$formula), "attendance | cens(censored) ~ marketing")

  # Uncensored model
  fit_uncens <- fit_demand(
    data = df,
    outcome = "attendance",
    covariates = "marketing",
    capacity = NULL,
    chains = 1,
    iter = 100,
    seed = 123
  )
  expect_s3_class(fit_uncens, "monteriskr_fit")
  expect_equal(deparse(fit_uncens$formula), "attendance ~ marketing")
})
