fixture_fit <- function() {
  readRDS(testthat::test_path("fixtures", "example_fit.rds"))
}

scenario <- function() {
  data.frame(
    day_type = factor(c("weekday", "weekend"), levels = c("weekday", "weekend")),
    marketing = c(0, 1)
  )
}

test_that("simulate_risk returns correctly shaped, consistent results", {
  fit <- fixture_fit()
  risk <- simulate_risk(fit, scenario(), n_sims = 200, price = 40,
                        variable_cost = 5, fixed_cost = 1000, seed = 1)
  expect_s3_class(risk, "monteriskr_risk")
  for (nm in c("demand", "attendance", "sold_out", "revenue", "profit")) {
    expect_equal(dim(risk[[nm]]), c(200L, 2L))
  }
  expect_length(risk$total_profit, 200)
  expect_length(risk$draw_ids, 200)

  # Capacity from the fit (250) is respected, sold_out matches demand >= cap
  expect_true(all(risk$attendance <= 250))
  expect_equal(risk$attendance, pmin(risk$demand, 250))
  expect_equal(risk$sold_out, risk$demand >= 250)

  # Revenue and profit are derived from attendance
  expect_equal(risk$revenue, risk$attendance * 40)
  expect_equal(risk$profit, risk$attendance * 35 - 1000)
  expect_equal(risk$total_profit, rowSums(risk$profit))

  expect_output(print(risk), "Risk Simulation")
})

test_that("simulate_risk is reproducible and leaves the global RNG alone", {
  fit <- fixture_fit()
  set.seed(99)
  before <- .Random.seed
  a <- simulate_risk(fit, scenario(), n_sims = 50, seed = 7)
  expect_identical(.Random.seed, before)
  b <- simulate_risk(fit, scenario(), n_sims = 50, seed = 7)
  c <- simulate_risk(fit, scenario(), n_sims = 50, seed = 8)
  expect_equal(a$demand, b$demand)
  expect_false(isTRUE(all.equal(a$demand, c$demand)))
})

test_that("each run uses one posterior draw; n_sims may exceed posterior draws", {
  fit <- fixture_fit()
  n_post <- brms::ndraws(fit$brmsfit)
  r1 <- simulate_risk(fit, scenario(), n_sims = 100, seed = 1)
  expect_equal(length(unique(r1$draw_ids)), 100)
  r2 <- simulate_risk(fit, scenario(), n_sims = n_post + 20, seed = 1)
  expect_equal(nrow(r2$demand), n_post + 20)
  expect_true(all(r2$draw_ids %in% seq_len(n_post)))
})

test_that("capacity, price and costs can be scalars, vectors or columns", {
  fit <- fixture_fit()
  nd <- scenario()
  nd$cap <- c(100, 150)
  nd$p <- c(10, 20)
  risk <- simulate_risk(fit, nd, n_sims = 50, price = "p", capacity = "cap",
                        fixed_cost = c(0, 500), seed = 3)
  expect_true(all(risk$attendance[, 1] <= 100))
  expect_true(all(risk$attendance[, 2] <= 150))
  expect_equal(risk$revenue[, 2], risk$attendance[, 2] * 20)
  expect_equal(risk$profit[, 2], risk$attendance[, 2] * 20 - 500)

  # Explicit capacity overrides the fit's capacity
  big <- simulate_risk(fit, scenario(), n_sims = 50, capacity = 1e6, seed = 3)
  expect_equal(big$attendance, big$demand)
  expect_false(any(big$sold_out))
})

test_that("simulate_risk validates inputs", {
  fit <- fixture_fit()
  expect_error(simulate_risk(list(), scenario()), "monteriskr_fit")
  expect_error(simulate_risk(fit, "x"), "data.frame")
  expect_error(simulate_risk(fit, scenario()[0, ]), "at least one row")
  expect_error(simulate_risk(fit, scenario(), n_sims = 0), "n_sims")
  expect_error(simulate_risk(fit, scenario(), n_sims = 2.5), "n_sims")
  expect_error(simulate_risk(fit, scenario()["marketing"]), "missing covariate")
  nd <- scenario(); nd$marketing[1] <- NA
  expect_error(simulate_risk(fit, nd), "missing values")
  expect_error(simulate_risk(fit, scenario(), price = "nope"), "not found")
  expect_error(simulate_risk(fit, scenario(), price = c(1, 2, 3)), "price")
  expect_error(simulate_risk(fit, scenario(), capacity = -5), "positive")
  expect_error(simulate_risk(fit, scenario(), seed = "a"), "seed")
})
