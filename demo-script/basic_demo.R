# monteriskr: basic demo (about 1 minute; no downloads, no CSV needed)
# Run line by line in R/RStudio.
#
# Setup, once:  devtools::load_all("/Users/aryaan/Documents/monteriskr")
#          or:  devtools::install_github("aryaanswain-13/monteriskr")
library(monteriskr)

# By default this uses a pre-computed model so it runs instantly.
# Set to TRUE to fit the model live (compiles Stan: about 1 minute extra).
FIT_LIVE <- FALSE

# ---- 1. The data ------------------------------------------------------------
# 60 past events: day type, marketing level, and attendance capped at a 250-seat
# venue. When attendance == 250 the event SOLD OUT, so true demand was >= 250.
data(example_events)
head(example_events[, c("day_type", "marketing", "attendance")])
cat(sprintf("%.0f%% of these events sold out\n", 100 * mean(example_events$attendance >= 250)))

# ---- 2. Check the data before fitting ------------------------------------------
check_data(example_events, outcome = "attendance",
           covariates = c("day_type", "marketing"), capacity = 250)

# The check catches problems. Here: attendance above capacity and a missing value.
bad <- example_events
bad$attendance[1] <- 400
bad$marketing[2]  <- NA
check_data(bad, outcome = "attendance",
           covariates = c("day_type", "marketing"), capacity = 250)

# ---- 3. Fit the demand model ----------------------------------------------------
# Sold-out events are treated as right-censored (demand >= capacity).
if (FIT_LIVE) {
  fit <- fit_demand(example_events, outcome = "attendance",
                    covariates = c("day_type", "marketing"), capacity = 250, seed = 1)
} else {
  fit <- readRDS(system.file("extdata", "example_fit.rds", package = "monteriskr"))
}
fit

# Always check the sampler: want 0 divergences and Rhat < 1.01.
np <- brms::nuts_params(fit$brmsfit)
c(divergences = sum(np$Value[np$Parameter == "divergent__"]),
  max_rhat    = round(max(brms::rhat(fit$brmsfit), na.rm = TRUE), 3))

# ---- 4. Describe a plan and simulate it ---------------------------------------
# Three planned events. Price and costs are YOUR assumptions.
plan <- data.frame(
  day_type  = factor(c("weekday", "weekend", "weekend"), levels = c("weekday", "weekend")),
  marketing = c(0, 1, 2)
)
plan

risk <- simulate_risk(fit, plan, n_sims = 4000, price = 40,
                      variable_cost = 5, fixed_cost = 3000, seed = 1)
risk

hist(risk$total_profit, breaks = 40, col = "grey85", border = "white",
     main = "Total profit across the 3 planned events", xlab = "Profit")
abline(v = mean(risk$total_profit), lwd = 2)

# ---- 5. Risk numbers -------------------------------------------------------------
# VaR/CVaR: loss = -profit. VaR = 95th percentile of loss; CVaR = average loss in
# the worst 5% of outcomes. A negative value means a profit even in the bad tail.
risk_summary(risk, level = 0.95)

# ---- 6. Compare two plans ----------------------------------------------------------
# Marketing is a model input, so we can ask: what if we double it (capped at the
# highest level in the data) and pay for it?
plan_big <- plan
plan_big$marketing <- pmin(plan$marketing * 2, max(example_events$marketing))

current <- simulate_risk(fit, plan,     n_sims = 4000, price = 40, variable_cost = 5,
                         fixed_cost = 3000 + 500 * plan$marketing,     seed = 2)
bigger  <- simulate_risk(fit, plan_big, n_sims = 4000, price = 40, variable_cost = 5,
                         fixed_cost = 3000 + 500 * plan_big$marketing, seed = 2)
compare_scenarios(current_marketing = current, more_marketing = bigger)

# ---- 7. Why the censoring matters --------------------------------------------------
# Stored simulation study: 50 datasets, ~25% sold out, known true values.
val <- readRDS(system.file("extdata", "validation_results.rds", package = "monteriskr"))$summary
subset(val, scenario == "heavy_censoring" & parameter != "Intercept",
       select = c(model, parameter, truth, bias, coverage90))
# 'naive' ignores capacity. It understates the effects and its 90% intervals
# rarely contain the truth; the censored model (what fit_demand() does) is close.
