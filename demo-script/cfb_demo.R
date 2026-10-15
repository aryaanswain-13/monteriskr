# monteriskr demo on real data: Arkansas home football games, 2000-2018
# Data: "College Football Games (2000 to 2018)" on Kaggle (CFBeattendance.csv).
# Plan: fit on 2000-2015, test on the held-out 2016-2018 seasons, then use the
# model to compare scheduling scenarios. Run line by line.
# Step 3 compiles Stan (~1 minute): run it before the audience arrives.

CSV_PATH <- "/Users/aryaan/Downloads/CFBeattendance.csv"   # <- change if moved
TEAM     <- "Arkansas"
library(monteriskr)

# ---- Step 1: load and prepare the data --------------------------------------
raw <- read.csv(CSV_PATH, stringsAsFactors = FALSE, fileEncoding = "latin1")
g <- raw[raw$Team == TEAM, ]
hour <- as.integer(sub(":.*", "", g$Time)); pm <- grepl("PM", g$Time)
hour <- ifelse(pm & hour < 12, hour + 12, hour)

games <- data.frame(
  season      = g$Year,
  capacity    = g$Stadium.Capacity,
  # Reported attendance can exceed listed capacity; cap it and treat as sold out.
  attendance  = pmin(g$Attendance, g$Stadium.Capacity),
  ranked      = as.numeric(g$Rank != "NR"),            # team ranked going in
  opp_ranked  = as.numeric(g$Opponent_Rank != "NR"),
  wins        = g$Current.Wins,                        # wins so far this season
  temp10      = g$TMAX / 10,                           # high temp, tens of deg F
  tv          = as.numeric(g$TV != "Not on TV"),
  tagged      = as.numeric(grepl("\\(", g$Site)),      # rivalry / trophy / special game
  early       = as.numeric(hour < 12)                  # morning kickoff
)
cat(sprintf("%d home games, %d-%d; %.0f%% at/above capacity (sold out)\n",
            nrow(games), min(games$season), max(games$season),
            100 * mean(games$attendance >= games$capacity)))
head(games)

train <- games[games$season <= 2015, ]
test  <- games[games$season >= 2016, ]
covs  <- c("ranked", "opp_ranked", "wins", "temp10", "tv", "tagged", "early")

# ---- Step 2: data check -------------------------------------------------------
check_data(train, outcome = "attendance", covariates = covs, capacity = "capacity")

# ---- Step 3: fit (sold-out games are censored) ------------------------------
fit <- fit_demand(train, outcome = "attendance", covariates = covs,
                  capacity = "capacity", seed = 1, iter = 2000,
                  control = list(adapt_delta = 0.95))
# Always look at sampler diagnostics before trusting a fit:
np <- brms::nuts_params(fit$brmsfit)
cat(sprintf("Divergent transitions: %d | max Rhat: %.3f (want 0 and < 1.01)\n",
            sum(np$Value[np$Parameter == "divergent__"]),
            max(brms::rhat(fit$brmsfit), na.rm = TRUE)))
round(summary(fit$brmsfit)$fixed[, c(1, 3, 4, 5)], 3)   # log scale; 0 is not in the interval = clear effect

# ---- Step 4: out-of-sample test on 2016-2018 -------------------------------
pred <- simulate_risk(fit, test, n_sims = 4000, capacity = "capacity", seed = 1)
holdout <- data.frame(
  season    = test$season,
  actual    = test$attendance,
  predicted = round(colMeans(pred$attendance)),
  lo90      = apply(pred$attendance, 2, quantile, 0.05),
  hi90      = apply(pred$attendance, 2, quantile, 0.95),
  p_soldout = round(colMeans(pred$sold_out), 2),
  was_soldout = test$attendance >= test$capacity
)
holdout
cat(sprintf("Actual attendance inside the 90%% interval: %d of %d games\n",
            sum(holdout$actual >= holdout$lo90 & holdout$actual <= holdout$hi90), nrow(holdout)))
cat(sprintf("Sell-outs: predicted %.1f, actual %d\n",
            sum(holdout$p_soldout), sum(holdout$was_soldout)))
cat(sprintf("Mean absolute error: %.0f fans (stadium holds ~%.0f)\n",
            mean(abs(holdout$actual - holdout$predicted)), mean(test$capacity)))

# ---- Step 5: risk for a hypothetical next season ---------------------------
# Plan = the 2018 home schedule (hypothetical next season). Prices/costs are ASSUMPTIONS (the data has none).
plan <- games[games$season == 2018, ]
base <- simulate_risk(fit, plan, n_sims = 4000, capacity = "capacity",
                      price = 70, variable_cost = 12, fixed_cost = 2.5e6, seed = 2)
risk_summary(base, level = 0.95)
hist(base$total_profit / 1e6, breaks = 40, col = "grey85", border = "white",
     main = paste("Season profit,", nrow(plan), "home games (assumed price/costs)"), xlab = "Profit ($ millions)")

# ---- Step 6: a scheduling decision --------------------------------------------
# Kickoff time is something the athletic department can influence.
early_all <- plan; early_all$early <- 1
late_all  <- plan; late_all$early  <- 0
r_early <- simulate_risk(fit, early_all, n_sims = 4000, capacity = "capacity",
                         price = 70, variable_cost = 12, fixed_cost = 2.5e6, seed = 3)
r_late  <- simulate_risk(fit, late_all,  n_sims = 4000, capacity = "capacity",
                         price = 70, variable_cost = 12, fixed_cost = 2.5e6, seed = 3)
compare_scenarios(morning_kickoffs = r_early, afternoon_kickoffs = r_late)

# Notes for discussion:
# * Teams that almost always sell out (e.g. Wisconsin, 65%+) give the model little
#   to learn from, and very large counts can make the sampler struggle (many
#   divergent transitions for some teams). Check diagnostics every time.
# * The model tends to over-predict sell-outs here: "full" in this data often
#   means 98-99% of the listed capacity, not exactly 100%.

# ---- Step 7: why censoring matters (stored validation study) ---------------
val <- readRDS(system.file("extdata", "validation_results.rds", package = "monteriskr"))$summary
subset(val, scenario == "heavy_censoring" & parameter != "Intercept",
       select = c(model, parameter, truth, bias, coverage90))
