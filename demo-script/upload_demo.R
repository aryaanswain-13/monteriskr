# monteriskr: upload a CSV, get demand and risk results
# ---------------------------------------------------------------------------
# HOW TO USE
#   1. Open this file in RStudio and press "Source" (or run it line by line).
#   2. A file picker opens: choose your PAST events CSV (one row per event).
#   3. Pick the outcome column (attendance), the covariates (things that might
#      drive attendance) and the capacity (a column, or type a number).
#   4. Optionally pick a second CSV of PLANNED events (same covariate columns,
#      no attendance). Cancel it to use your 5 most recent events as the plan.
#
# Sample files to try: demo-script/sample-data/fest_past.csv (+ fest_plan.csv as the plan;
# outcome attendance, covariates day_type + marketing_k, capacity venue_capacity).
#
# The first model fit compiles Stan and takes about a minute.
# Prices and costs below are ASSUMPTIONS you set; the data does not contain them.
# ---------------------------------------------------------------------------

# ---- Settings (edit the prices/costs below; the rest is asked for) -------------
# Settings are only read from options(monteriskr.demo = list(...)) (used for testing);
# earlier answers left in your R session are never reused, so every run asks fresh.
cfg <- function(name, default) {
  s <- getOption("monteriskr.demo")
  if (!is.null(s) && !is.null(s[[name]])) s[[name]] else default
}

CSV_PATH    <- cfg("CSV_PATH",    NULL)   # past events CSV; NULL = file picker
PLAN_PATH   <- cfg("PLAN_PATH",   NULL)   # planned events CSV; NULL = ask / use recent
OUTCOME     <- cfg("OUTCOME",     NULL)   # e.g. "attendance"
COVARIATES  <- cfg("COVARIATES",  NULL)   # e.g. c("day_type", "marketing_k")
CAPACITY    <- cfg("CAPACITY",    NULL)   # column name, a number, or "none"
PRICE         <- cfg("PRICE",         50) # ticket price (assumption)
VARIABLE_COST <- cfg("VARIABLE_COST", 10) # cost per attendee (assumption)
FIXED_COST    <- cfg("FIXED_COST",     0) # fixed cost per event (assumption)
N_SIMS      <- cfg("N_SIMS", 4000)
SEED        <- cfg("SEED", 1)

library(monteriskr)
out_dir <- tempdir()   # results are saved here; the path is printed at the end

# ---- Step 1: load the CSV ---------------------------------------------------
if (is.null(CSV_PATH)) {
  message("Choose your PAST events CSV...")
  CSV_PATH <- file.choose()
}
raw <- read.csv(CSV_PATH, stringsAsFactors = FALSE, fileEncoding = "latin1")
cat(sprintf("\nLoaded %d rows x %d columns from %s\n", nrow(raw), ncol(raw), basename(CSV_PATH)))
print(utils::head(raw, 6))

# ---- Step 2: choose the columns --------------------------------------------
ask_one <- function(title, choices, none = FALSE) {
  if (!interactive()) stop("Set ", title, " in the settings at the top (not interactive).")
  ch <- c(if (none) "(none)", choices)
  pick <- utils::select.list(ch, title = title)
  if (pick == "" || pick == "(none)") NULL else pick
}
if (is.null(OUTCOME))
  OUTCOME <- ask_one("Which column is the outcome (attendance count)?", names(raw))
if (is.null(COVARIATES)) {
  if (!interactive()) stop("Set COVARIATES in the settings at the top.")
  COVARIATES <- utils::select.list(setdiff(names(raw), OUTCOME), multiple = TRUE,
                                   title = "Covariates (columns that may drive attendance)")
}
if (is.null(CAPACITY)) {
  pick <- ask_one("Capacity: choose a column, or (none)", setdiff(names(raw), OUTCOME), none = TRUE)
  CAPACITY <- if (is.null(pick)) "none" else pick
}
if (identical(CAPACITY, "none")) CAPACITY <- NULL
stopifnot(OUTCOME %in% names(raw), all(COVARIATES %in% names(raw)))
cap_is_col <- is.character(CAPACITY) && CAPACITY %in% names(raw)
cat(sprintf("\nOutcome: %s | Covariates: %s | Capacity: %s\n", OUTCOME,
            if (length(COVARIATES)) paste(COVARIATES, collapse = ", ") else "(none)",
            if (is.null(CAPACITY)) "none" else CAPACITY))

# ---- Step 3: prepare the data ----------------------------------------------
use <- c(OUTCOME, COVARIATES, if (cap_is_col) CAPACITY)
dat <- raw[, unique(use), drop = FALSE]
n0 <- nrow(dat); dat <- dat[stats::complete.cases(dat), , drop = FALSE]
if (nrow(dat) < n0) cat(sprintf("Dropped %d rows with missing values.\n", n0 - nrow(dat)))

# Text/logical covariates -> factors/numbers; numeric covariates -> standardised
# (mean 0, sd 1), because the default prior assumes covariates on a modest scale.
cov_info <- list()
for (v in COVARIATES) {
  x <- dat[[v]]
  if (is.logical(x)) x <- as.numeric(x)
  if (is.character(x)) {
    dat[[v]] <- factor(x); cov_info[[v]] <- list(type = "factor", levels = levels(dat[[v]]))
  } else {
    m <- mean(x); s <- stats::sd(x)
    dat[[v]] <- if (s > 0) (x - m) / s else x
    cov_info[[v]] <- list(type = "numeric", mean = m, sd = s)
  }
}
# Capacity: cap the outcome at capacity (reported counts can exceed it) -> "sold out"
cap_vec <- if (cap_is_col) dat[[CAPACITY]] else if (is.numeric(CAPACITY)) rep(CAPACITY, nrow(dat)) else NULL
if (!is.null(cap_vec)) {
  over <- sum(dat[[OUTCOME]] > cap_vec)
  if (over > 0) cat(sprintf("%d rows had %s above capacity; capped at capacity (treated as sold out).\n", over, OUTCOME))
  dat[[OUTCOME]] <- pmin(dat[[OUTCOME]], cap_vec)
  share <- mean(dat[[OUTCOME]] >= cap_vec)
  cat(sprintf("%.0f%% of events sold out (%s at capacity).\n", 100 * share, OUTCOME))
  if (share > 0.5) cat("WARNING: most events sold out. Sold-out rows only say demand was AT LEAST capacity,\n",
                       "         so estimates will be weak. Check the diagnostics in Step 5 carefully.\n")
}
dat[[OUTCOME]] <- as.integer(round(dat[[OUTCOME]]))

# ---- Step 4: data check ------------------------------------------------------
cap_arg <- CAPACITY
chk <- check_data(dat, outcome = OUTCOME, covariates = if (length(COVARIATES)) COVARIATES else NULL,
                  capacity = cap_arg)
print(chk)
if (!chk$valid) stop("Fix the data problems listed above and run again.")

# ---- Step 5: fit the demand model ------------------------------------------
cat("\nFitting the model (first run compiles Stan, ~1 minute)...\n")
fit <- fit_demand(dat, outcome = OUTCOME,
                  covariates = if (length(COVARIATES)) COVARIATES else NULL,
                  capacity = cap_arg, seed = SEED, iter = 2000,
                  control = list(adapt_delta = 0.95))

np  <- brms::nuts_params(fit$brmsfit)
div <- sum(np$Value[np$Parameter == "divergent__"])
rh  <- max(brms::rhat(fit$brmsfit), na.rm = TRUE)
cat(sprintf("\nDiagnostics: %d divergent transitions, max Rhat %.3f  (want 0 and < 1.01)%s\n",
            div, rh, if (div > 0 || rh > 1.01) "  <-- CAUTION: results may be unreliable" else "  OK"))

co <- summary(fit$brmsfit)$fixed[, c(1, 3, 4)]
co <- data.frame(term = rownames(co), estimate = round(co[, 1], 3),
                 lower95 = round(co[, 2], 3), upper95 = round(co[, 3], 3),
                 effect_on_demand = sprintf("%+.0f%%", 100 * (exp(co[, 1]) - 1)),
                 clear = ifelse(co[, 2] > 0 | co[, 3] < 0, "yes", "no"), row.names = NULL)
co$effect_on_demand[co$term == "Intercept"] <- ""
cat("\nWhat drives demand (log scale; 'effect' = % change in demand per +1 unit,\n",
    "numeric covariates are in standard deviations; 'clear' = interval excludes 0):\n", sep = "")
print(co, row.names = FALSE)

# ---- Step 6: in-sample sanity check ----------------------------------------
chk_pred <- simulate_risk(fit, dat, n_sims = 2000, capacity = cap_arg, seed = SEED)
lo <- apply(chk_pred$attendance, 2, stats::quantile, 0.05)
hi <- apply(chk_pred$attendance, 2, stats::quantile, 0.95)
cat(sprintf("\nSanity check on the past events: actual %s fell inside the model's 90%% range for %.0f%% of events\n",
            OUTCOME, 100 * mean(dat[[OUTCOME]] >= lo & dat[[OUTCOME]] <= hi)))
if (!is.null(cap_vec))
  cat(sprintf("Sell-outs: model expects %.1f, actual %d\n", sum(colMeans(chk_pred$sold_out)), sum(dat[[OUTCOME]] >= cap_vec)))

# ---- Step 7: the planned events --------------------------------------------
plan <- NULL
if (is.null(PLAN_PATH) && interactive()) {
  message("Choose a CSV of PLANNED events (same covariate columns), or Cancel to use your last 5 events...")
  PLAN_PATH <- tryCatch(file.choose(), error = function(e) NULL)
}
if (!is.null(PLAN_PATH)) {
  plan_raw <- read.csv(PLAN_PATH, stringsAsFactors = FALSE, fileEncoding = "latin1")
  miss <- setdiff(COVARIATES, names(plan_raw))
  if (length(miss)) stop("Planned-events file is missing columns: ", paste(miss, collapse = ", "))
  plan <- plan_raw[, intersect(c(COVARIATES, if (cap_is_col) CAPACITY), names(plan_raw)), drop = FALSE]
  label <- if ("event" %in% names(plan_raw)) plan_raw$event else paste("Event", seq_len(nrow(plan)))
  cat(sprintf("\nLoaded %d planned events from %s\n", nrow(plan), basename(PLAN_PATH)))
} else {
  k <- min(5, nrow(dat)); plan <- dat[(nrow(dat) - k + 1):nrow(dat), , drop = FALSE]
  plan[[OUTCOME]] <- NULL; label <- paste("Recent event", seq_len(k))
  cat(sprintf("\nNo planned-events file: using your %d most recent past events as the plan.\n", k))
}
if (is.null(PLAN_PATH)) {
  plan_ready <- plan   # already prepared (scaled/factors)
} else {
  plan_ready <- plan
  for (v in COVARIATES) {
    ci <- cov_info[[v]]; x <- plan[[v]]
    if (is.logical(x)) x <- as.numeric(x)
    if (ci$type == "factor") {
      bad <- setdiff(unique(as.character(x)), ci$levels)
      if (length(bad)) stop("Planned events have values not seen in the past data for '", v, "': ",
                            paste(bad, collapse = ", "))
      plan_ready[[v]] <- factor(as.character(x), levels = ci$levels)
    } else {
      plan_ready[[v]] <- if (ci$sd > 0) (x - ci$mean) / ci$sd else x
    }
  }
  if (cap_is_col && !CAPACITY %in% names(plan_ready)) {
    cat(sprintf("Planned file has no '%s' column: using the median past capacity (%.0f).\n",
                CAPACITY, stats::median(dat[[CAPACITY]])))
    plan_ready[[CAPACITY]] <- stats::median(dat[[CAPACITY]])
  }
}

# ---- Step 8: predictions and risk ------------------------------------------
risk <- simulate_risk(fit, plan_ready, n_sims = N_SIMS, price = PRICE,
                      variable_cost = VARIABLE_COST, fixed_cost = FIXED_COST,
                      capacity = cap_arg, seed = SEED)
pred <- data.frame(
  event          = label,
  expected       = round(colMeans(risk$attendance)),
  low_5pct       = round(apply(risk$attendance, 2, stats::quantile, 0.05)),
  high_95pct     = round(apply(risk$attendance, 2, stats::quantile, 0.95)),
  prob_sold_out  = round(colMeans(risk$sold_out), 2),
  expected_profit = round(colMeans(risk$profit)),
  prob_loss      = round(colMeans(risk$profit < 0), 3)
)
cat("\nPREDICTIONS for the planned events:\n"); print(pred, row.names = FALSE)

cat(sprintf("\nProfit assumptions: price %g, variable cost %g per attendee, fixed cost %g per event.\n",
            PRICE, VARIABLE_COST, FIXED_COST))
rs <- risk_summary(risk, level = 0.95)
print(rs)

graphics::hist(risk$total_profit, breaks = 40, col = "grey85", border = "white",
               main = "Total profit across the planned events", xlab = "Profit")
graphics::abline(v = mean(risk$total_profit), lwd = 2)

# ---- Step 9: a what-if comparison -------------------------------------------
# Only covariates that vary continuously make sense to shift by +1 standard deviation
# (a yes/no flag would be pushed to impossible values like 1.4).
is_cont <- function(v) cov_info[[v]]$type == "numeric" && length(unique(raw[[v]])) > 2
num_covs <- COVARIATES[vapply(COVARIATES, is_cont, logical(1))]
if (length(num_covs)) {
  v <- num_covs[1]
  up <- plan_ready; up[[v]] <- up[[v]] + 1     # +1 standard deviation
  r_up <- simulate_risk(fit, up, n_sims = N_SIMS, price = PRICE, variable_cost = VARIABLE_COST,
                        fixed_cost = FIXED_COST, capacity = cap_arg, seed = SEED + 1)
  cat(sprintf("\nWHAT-IF: raise '%s' by one standard deviation (%.3g original units) for every event.\n",
              v, cov_info[[v]]$sd))
  cat("(Costs of making that change are NOT included; add them to fixed_cost to compare fairly.)\n")
  print(compare_scenarios(as_planned = risk, plus_1sd = r_up))
} else {
  cat("\nNo continuous covariate to run a what-if on (yes/no flags and categories are skipped).\n")
}

# ---- Save results -----------------------------------------------------------
utils::write.csv(pred, file.path(out_dir, "predictions.csv"), row.names = FALSE)
utils::write.csv(co,   file.path(out_dir, "coefficients.csv"), row.names = FALSE)
cat(sprintf("\nSaved predictions.csv and coefficients.csv to:\n  %s\n", out_dir))
