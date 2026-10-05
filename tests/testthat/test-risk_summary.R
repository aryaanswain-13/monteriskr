fake_risk <- function(profit) {
  profit <- as.matrix(profit)
  n <- nrow(profit)
  structure(
    list(
      demand = profit, attendance = pmax(profit, 0), sold_out = profit > 1,
      revenue = profit, profit = profit, total_profit = rowSums(profit),
      draw_ids = seq_len(n), newdata = data.frame(x = seq_len(ncol(profit))),
      inputs = list()
    ),
    class = "monteriskr_risk"
  )
}

test_that("VaR and CVaR follow the loss = -profit convention", {
  # loss = 1..100 -> VaR(95%) = 95.05, CVaR = mean(96:100) = 98
  r <- fake_risk(-(1:100))
  s <- risk_summary(r, level = 0.95)
  expect_s3_class(s, "monteriskr_summary")
  expect_equal(s$total$var, 95.05)
  expect_equal(s$total$cvar, 98)
  expect_equal(s$total$mean_profit, -50.5)
  expect_equal(s$total$prob_loss, 1)
  expect_gte(s$total$cvar, s$total$var)

  # Profits that are all positive give a negative VaR (a gain even in the tail)
  s2 <- risk_summary(fake_risk(101:200), level = 0.9)
  expect_lt(s2$total$var, 0)
  expect_equal(s2$total$prob_loss, 0)
})

test_that("a higher level gives a larger (or equal) VaR and CVaR", {
  r <- fake_risk(seq(-50, 150, length.out = 500))
  lo <- risk_summary(r, 0.8)$total
  hi <- risk_summary(r, 0.99)$total
  expect_gte(hi$var, lo$var)
  expect_gte(hi$cvar, lo$cvar)
})

test_that("risk_summary reports per-event and total metrics", {
  fit <- readRDS(testthat::test_path("fixtures", "example_fit.rds"))
  nd <- data.frame(
    day_type = factor(c("weekday", "weekend"), levels = c("weekday", "weekend")),
    marketing = c(0, 1)
  )
  risk <- simulate_risk(fit, nd, n_sims = 300, price = 40, fixed_cost = 3000, seed = 1)
  s <- risk_summary(risk)
  expect_equal(nrow(s$total), 1)
  expect_equal(nrow(s$events), 2)
  expect_equal(s$n_sims, 300)
  expect_equal(s$total$mean_profit, mean(risk$total_profit))
  expect_equal(s$events$mean_attendance, unname(colMeans(risk$attendance)))
  expect_equal(s$events$prob_sold_out, unname(colMeans(risk$sold_out)))
  expect_output(print(s), "Risk Summary")
})

test_that("risk_summary validates inputs", {
  r <- fake_risk(1:10)
  expect_error(risk_summary(list()), "monteriskr_risk")
  expect_error(risk_summary(r, level = 1), "level")
  expect_error(risk_summary(r, level = 0), "level")
  expect_error(risk_summary(r, level = c(0.9, 0.95)), "level")
  expect_error(risk_summary(r, level = "a"), "level")
})

test_that("compare_scenarios builds a comparison table against a baseline", {
  base <- fake_risk(matrix(c(10, 20, 30, 40), ncol = 1))
  better <- fake_risk(matrix(c(110, 120, 130, 140), ncol = 1))
  cmp <- compare_scenarios(base = base, better = better)
  expect_s3_class(cmp, "monteriskr_comparison")
  expect_equal(cmp$scenario, c("base", "better"))
  expect_equal(cmp$mean_profit, c(25, 125))
  expect_equal(cmp$diff_mean_profit, c(0, 100))
  expect_equal(cmp$n_events, c(1, 1))
  expect_output(print(cmp), "Scenario Comparison")

  # A named list is accepted; scenarios may have different numbers of events
  two <- fake_risk(cbind(c(1, 2, 3, 4), c(5, 6, 7, 8)))
  cmp2 <- compare_scenarios(list(a = base, b = two), level = 0.9)
  expect_equal(cmp2$n_events, c(1, 2))
  expect_equal(attr(cmp2, "level"), 0.9)
})

test_that("compare_scenarios validates inputs", {
  r <- fake_risk(1:10)
  expect_error(compare_scenarios(), "at least one")
  expect_error(compare_scenarios(r), "unique, non-empty names")
  expect_error(compare_scenarios(a = r, a = r), "unique, non-empty names")
  expect_error(compare_scenarios(a = r, b = 1), "monteriskr_risk")
  expect_error(compare_scenarios(a = r, level = 2), "level")
})
