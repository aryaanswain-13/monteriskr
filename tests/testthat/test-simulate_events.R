test_that("simulate_events produces expected columns and structure", {
  res <- simulate_events(n = 50, seed = 123)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 50)
  expect_named(res, c("day_type", "marketing", "true_demand", "attendance", "sold_out"))

  params <- attr(res, "true_params")
  expect_type(params, "list")
  expect_named(params, c("beta", "phi", "capacity", "seed"))
  expect_equal(params$seed, 123)
})

test_check_cap <- function() {
  res <- simulate_events(n = 100, capacity = 100, beta = c(intercept = 6, weekend = 0, marketing = 0), seed = 1)
  expect_true(all(res$attendance <= 100))
  expect_equal(res$sold_out, res$true_demand >= 100)
  expect_equal(res$attendance, pmin(res$true_demand, 100))
}

test_that("capacity cap and sold_out logic work correctly", {
  test_check_cap()
})

test_that("simulate_events handles uncapped capacity (NULL or Inf)", {
  res_null <- simulate_events(n = 30, capacity = NULL, seed = 5)
  expect_true(all(!res_null$sold_out))
  expect_equal(res_null$attendance, res_null$true_demand)

  res_inf <- simulate_events(n = 30, capacity = Inf, seed = 5)
  expect_equal(res_null$attendance, res_inf$attendance)
})

test_that("reproducibility with seed works", {
  s1 <- simulate_events(n = 40, seed = 99)
  s2 <- simulate_events(n = 40, seed = 99)
  expect_identical(s1, s2)
})

test_that("simulate_events validates input arguments", {
  expect_error(simulate_events(n = -5), "`n` must be a single positive integer.")
  expect_error(simulate_events(n = c(10, 20)), "`n` must be a single positive integer.")
  expect_error(simulate_events(n = 10.5), "`n` must be a single positive integer.")
  expect_error(simulate_events(beta = c(1, 2)), "`beta` must be a numeric vector of length 3.")
  expect_error(simulate_events(beta = c(a = 1, b = 2, c = 3)), "`beta` must contain named elements")
  expect_error(simulate_events(phi = -1), "`phi` must be a single positive number.")
  expect_error(simulate_events(capacity = -100), "`capacity` must be a single positive number")
  expect_error(simulate_events(seed = "abc"), "`seed` must be a single integer or NULL.")
})
