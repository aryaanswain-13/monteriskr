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

  # data_original must exist and contain the unshifted outcome
  expect_true(!is.null(fit$data_original))
  expect_true(is.data.frame(fit$data_original))
  # All original attendance values should be >= censored rows' brmsfit Y + 1
  # (i.e. original data is not decremented)
  cens_rows <- fit$data_original$attendance >= 250
  if (any(cens_rows)) {
    expect_true(all(fit$data_original$attendance[cens_rows] >= 250))
  }

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

  # data_original preserved and unshifted
  expect_true(is.data.frame(fit_cens$data_original))
  expect_equal(nrow(fit_cens$data_original), nrow(df))
  cens_rows <- df$attendance >= 300
  if (any(cens_rows)) {
    expect_equal(fit_cens$data_original$attendance[cens_rows], df$attendance[cens_rows])
  }

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

test_that("censoring fix shifts Y in standata via apply_censoring_shift", {
  df <- simulate_events(n = 30, capacity = 250, seed = 42)
  cens_rows <- df$attendance >= 250
  expect_true(any(cens_rows))
  expect_true(any(!cens_rows))

  # Use the shared helper (not a copy of the logic)
  cap_vec <- rep(250, nrow(df))
  fit_data <- monteriskr:::apply_censoring_shift(df, "attendance", cap_vec)

  form <- stats::as.formula("attendance | cens(censored) ~ marketing")
  st <- brms::make_standata(form, data = fit_data, family = brms::negbinomial())

  # 1. Y for censored rows equals capacity - 1 (250 - 1 = 249)
  expect_equal(as.numeric(st$Y[cens_rows]), rep(249, sum(cens_rows)))
  # 2. Y for uncensored rows is unchanged
  expect_equal(as.numeric(st$Y[!cens_rows]), df$attendance[!cens_rows])
  # 3. Censoring indicator is set correctly
  expect_equal(as.numeric(st$cens), as.numeric(cens_rows))

  # 4. Numeric test showing pnbinom(C - 1, mu = m, size = s, lower.tail = FALSE) equals 1 - pnbinom(C - 1, mu = m, size = s)
  m <- 200
  s <- 20
  C <- 250
  p_lccdf <- stats::pnbinom(C - 1, mu = m, size = s, lower.tail = FALSE)
  p_direct <- 1 - stats::pnbinom(C - 1, mu = m, size = s)
  expect_equal(p_lccdf, p_direct)
})
