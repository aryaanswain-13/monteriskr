# Parameter-recovery study: right-censored model vs. naive model (M6)
#
# For each scenario and replication, events are simulated with known
# parameters (simulate_events()), then fitted two ways:
#   * censored: sold-out rows treated as right-censored (what fit_demand()
#     does when `capacity` is supplied)
#   * naive:    capped attendance treated as true demand (capacity = NULL)
#
# To avoid recompiling Stan for every replication, each model is compiled once
# through fit_demand() and then refitted with brms::update(newdata = ...) using
# the same formula, family and prior. The study therefore checks the model, not
# the thin fit_demand() wrapper (tested separately in tests/). Refits use the
# same data-informed starting values as fit_demand() (make_inits()).
#
# Run from the package root:  Rscript data-raw/validation.R
# Output: inst/extdata/validation_results.rds (list: reps, summary, settings)

devtools::load_all(quiet = TRUE)

settings <- list(
  n_reps    = 50,
  n_events  = 150,
  truth     = c(Intercept = 5, day_typeweekend = 0.3, marketing = 0.2, shape = 20),
  scenarios = c(heavy_censoring = 200, light_censoring = 400),  # capacity
  chains    = 2,
  iter      = 1000
)
covs  <- c("day_type", "marketing")
truth <- settings$truth
cols  <- c(Intercept = "b_Intercept", day_typeweekend = "b_day_typeweekend",
           marketing = "b_marketing", shape = "shape")

extract <- function(brmsfit) {
  draws <- as.data.frame(brmsfit)[, unname(cols)]
  names(draws) <- names(cols)
  list(
    mean  = colMeans(draws),
    lo    = vapply(draws, stats::quantile, numeric(1), probs = 0.05, names = FALSE),
    hi    = vapply(draws, stats::quantile, numeric(1), probs = 0.95, names = FALSE),
    rhat  = max(brms::rhat(brmsfit), na.rm = TRUE)
  )
}

reps <- list()
for (sc in names(settings$scenarios)) {
  cap <- settings$scenarios[[sc]]
  cat(sprintf("== Scenario %s (capacity %g) ==\n", sc, cap))

  # Compile each model once on a pilot dataset
  pilot <- simulate_events(n = settings$n_events, capacity = cap, seed = 0)
  base_cens  <- fit_demand(pilot, "attendance", covs, capacity = cap,
                           chains = settings$chains, iter = settings$iter, seed = 1)$brmsfit
  base_naive <- fit_demand(pilot, "attendance", covs, capacity = NULL,
                           chains = settings$chains, iter = settings$iter, seed = 1)$brmsfit

  for (r in seq_len(settings$n_reps)) {
    d <- simulate_events(n = settings$n_events, capacity = cap, seed = r)
    sold_out_share <- mean(d$sold_out)
    d_cens <- apply_censoring_shift(d, "attendance", rep(cap, nrow(d)))

    refit <- function(base, data, model) {
      f <- suppressWarnings(update(base, newdata = data, recompile = FALSE,
                                   chains = settings$chains, iter = settings$iter,
                                   init = make_inits(data$attendance, 2),
                                   seed = r, refresh = 0))
      ok <- tryCatch({ as.data.frame(f); TRUE }, error = function(e) FALSE)
      if (!ok) {
        stop(sprintf("Sampling failed: scenario %s, rep %d, model %s", sc, r, model),
             call. = FALSE)
      }
      f
    }
    fits <- list(censored = refit(base_cens, d_cens, "censored"),
                 naive    = refit(base_naive, d, "naive"))
    for (m in names(fits)) {
      e <- extract(fits[[m]])
      reps[[length(reps) + 1]] <- data.frame(
        scenario = sc, capacity = cap, rep = r, model = m,
        sold_out_share = sold_out_share, max_rhat = e$rhat,
        parameter = names(truth), truth = unname(truth),
        post_mean = unname(e$mean), lo90 = unname(e$lo), hi90 = unname(e$hi),
        row.names = NULL
      )
    }
    if (r %% 10 == 0) cat(sprintf("  rep %d/%d\n", r, settings$n_reps))
  }
}
reps <- do.call(rbind, reps)
reps$error   <- reps$post_mean - reps$truth
reps$covered <- reps$lo90 <= reps$truth & reps$truth <= reps$hi90

# Save the raw per-replication results, then summarise (can be re-run alone:
# Rscript data-raw/validation_summary.R)
saveRDS(list(reps = reps, settings = settings),
        file.path("inst", "extdata", "validation_results.rds"), compress = "xz")
source(file.path("data-raw", "validation_summary.R"))
