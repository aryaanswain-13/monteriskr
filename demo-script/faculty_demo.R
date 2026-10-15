# monteriskr demo: college fest risk planning
# Run line by line in RStudio (or source the whole file).
# The model fit compiles Stan once and takes ~1 minute: run Step 3 BEFORE the
# audience arrives, or set USE_SAVED_FIT <- TRUE to skip the compile.

USE_SAVED_FIT <- FALSE

# install.packages("remotes")
# remotes::install_github("aryaanswain-13/monteriskr")
library(monteriskr)

# ---- Step 0: make the "committee's" CSV files -------------------------------
# (Stand-in for real records. Venue: weekday events in a 120-seat room,
#  weekend events in a 300-seat hall. Marketing is in thousands of rupees.)
set.seed(2026)
n <- 48
past <- data.frame(
  event       = paste("Event", seq_len(n)),
  day_type    = sample(c("weekday", "weekend"), n, replace = TRUE, prob = c(0.6, 0.4)),
  marketing_k = round(runif(n, 0, 6), 1)
)
past$venue_capacity <- ifelse(past$day_type == "weekend", 300, 120)
mu <- exp(4.3 + 0.8 * (past$day_type == "weekend") + 0.18 * (past$marketing_k - 3))
true_demand <- rnbinom(n, mu = mu, size = 15)
past$attendance <- pmin(true_demand, past$venue_capacity)   # what the register shows

planned <- data.frame(
  event          = c("Techfest Opening", "Alumni Talk", "Cultural Night"),
  day_type       = c("weekend", "weekday", "weekend"),
  marketing_k    = c(5, 1.5, 3),
  venue_capacity = c(300, 120, 300)
)
dir <- tempdir()
write.csv(past,    file.path(dir, "past_events.csv"),    row.names = FALSE)
write.csv(planned, file.path(dir, "next_semester.csv"), row.names = FALSE)

# ---- Step 1: the user's input: two CSV files --------------------------------
past    <- read.csv(file.path(dir, "past_events.csv"))
planned <- read.csv(file.path(dir, "next_semester.csv"))
head(past, 8)
cat(sprintf("%.0f%% of past events sold out (attendance == capacity)\n",
            100 * mean(past$attendance >= past$venue_capacity)))
planned

# ---- Step 2: check the data -------------------------------------------------
check_data(past, outcome = "attendance",
           covariates = c("day_type", "marketing_k"),
           capacity = "venue_capacity")

# ---- Step 3: fit the demand model (sold-out events treated as censored) -----
if (USE_SAVED_FIT) {
  fit <- readRDS(system.file("extdata", "example_fit.rds", package = "monteriskr"))
  planned <- data.frame(
    day_type = factor(c("weekend", "weekday", "weekend"), levels = c("weekday", "weekend")),
    marketing = c(1, 0, 0.5)
  )
  cap_arg <- 250
} else {
  fit <- fit_demand(past, outcome = "attendance",
                    covariates = c("day_type", "marketing_k"),
                    capacity = "venue_capacity", seed = 1)
  planned$day_type <- factor(planned$day_type, levels = c("weekday", "weekend"))
  cap_arg <- "venue_capacity"
}
round(summary(fit$brmsfit)$fixed[, 1:4], 3)

# ---- Step 4: simulate next semester ----------------------------------------
# Ticket Rs 100, Rs 20 variable cost per attendee, Rs 3000 fixed cost per event.
risk <- simulate_risk(fit, planned, n_sims = 4000, price = 100,
                      variable_cost = 20, fixed_cost = 3000,
                      capacity = cap_arg, seed = 1)
risk

hist(risk$total_profit, breaks = 40, col = "grey85", border = "white",
     main = "Next semester: total profit across the planned events",
     xlab = "Profit (Rs)")
abline(v = mean(risk$total_profit), lwd = 2)

# ---- Step 5: the risk numbers ----------------------------------------------
risk_summary(risk, level = 0.95)

# ---- Step 6: compare two plans ----------------------------------------------
# Marketing IS a model covariate, so we can ask: is doubling the marketing budget
# worth its cost (capped at the historical maximum)? Marketing costs Rs 1000 per unit (thousand) of spend per event.
base_cost <- 3000
planned_big <- planned
mk <- if (USE_SAVED_FIT) "marketing" else "marketing_k"
# Cap at 6 (the largest value in the history) so we do not extrapolate.
planned_big[[mk]] <- pmin(2 * planned[[mk]], 6)

current <- simulate_risk(fit, planned,     n_sims = 4000, price = 100, variable_cost = 20,
                         fixed_cost = base_cost + 1000 * planned[[mk]],
                         capacity = cap_arg, seed = 2)
double  <- simulate_risk(fit, planned_big, n_sims = 4000, price = 100, variable_cost = 20,
                         fixed_cost = base_cost + 1000 * planned_big[[mk]],
                         capacity = cap_arg, seed = 2)
compare_scenarios(current_marketing = current, double_marketing = double)
# Capacity limits the upside: sold-out events cannot use extra demand, so more
# marketing pays off least where the venue is already full.
# (Price is not compared here: demand does not respond to price unless price is a
#  model covariate, so a price comparison would only reflect our assumption.)

# ---- Step 7: why censoring matters (stored validation study) ----------------
val <- readRDS(system.file("extdata", "validation_results.rds", package = "monteriskr"))$summary
subset(val, scenario == "heavy_censoring" & parameter != "Intercept",
       select = c(model, parameter, truth, bias, coverage90))
