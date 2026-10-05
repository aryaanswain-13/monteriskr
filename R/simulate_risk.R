#' Evaluate an expression under a temporary random seed
#'
#' Internal helper. If `seed` is `NULL` the expression is evaluated as is.
#' Otherwise the global `.Random.seed` is saved, `set.seed(seed)` is called,
#' and the original state is restored on exit.
#'
#' @param seed Optional single number.
#' @param code Expression to evaluate.
#' @return The value of `code`.
#' @keywords internal
with_temp_seed <- function(seed, code) {
  if (is.null(seed)) {
    return(code)
  }
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv)
    on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  } else {
    on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  }
  set.seed(as.integer(seed))
  force(code)
}


#' Resolve a per-event input (scalar, vector, or column name)
#'
#' @param x Numeric scalar, numeric vector of length `nrow(newdata)`, or a
#'   single column name in `newdata`.
#' @param newdata Scenario data frame.
#' @param name Argument name used in error messages.
#' @return Numeric vector of length `nrow(newdata)`.
#' @keywords internal
resolve_per_event <- function(x, newdata, name) {
  n <- nrow(newdata)
  if (is.character(x) && length(x) == 1 && !is.na(x)) {
    if (!x %in% names(newdata)) {
      stop(sprintf("`%s` column '%s' not found in `newdata`.", name, x), call. = FALSE)
    }
    x <- newdata[[x]]
  }
  if (!is.numeric(x) || anyNA(x) || !(length(x) %in% c(1L, n))) {
    stop(sprintf(
      "`%s` must be a number, a numeric vector with one value per row of `newdata`, or a column name in `newdata`.",
      name
    ), call. = FALSE)
  }
  rep_len(as.numeric(x), n)
}


#' Simulate Demand, Attendance and Profit from a Fitted Demand Model
#'
#' Runs a Monte Carlo simulation for a planned set of events. Each simulation
#' run draws one parameter set (regression coefficients and negative binomial
#' shape) from the posterior of a [fit_demand()] model, simulates unconstrained
#' demand for every event in `newdata`, caps it at venue capacity to obtain
#' attendance, and derives revenue and profit from attendance.
#'
#' @param fit A `"monteriskr_fit"` object from [fit_demand()].
#' @param newdata A `data.frame` with one row per planned event. It must contain
#'   every covariate used in `fit`, plus any columns referenced by name in
#'   `price`, `variable_cost`, `fixed_cost` or `capacity`.
#' @param n_sims Single positive integer; number of Monte Carlo runs. Default is
#'   1000. If `n_sims` exceeds the number of posterior draws, draws are reused
#'   (sampled with replacement); each run still simulates fresh demand.
#' @param price Ticket price: a number, a numeric vector with one value per row
#'   of `newdata`, or a column name in `newdata`. Default is 1.
#' @param variable_cost Cost per attendee, specified like `price`. Default is 0.
#' @param fixed_cost Fixed cost per event, specified like `price`. Default is 0.
#' @param capacity Venue capacity, specified like `price`. If `NULL` (default),
#'   the capacity used in `fit` is taken; if `fit` had no capacity, attendance
#'   is uncapped.
#' @param seed Optional integer seed. The global random number state is
#'   restored afterwards. Default is `NULL`.
#'
#' @details
#' Revenue is derived from attendance, never modelled separately. Prices and
#' costs are user assumptions, not model outputs. For each event,
#' `profit = (price - variable_cost) * attendance - fixed_cost`.
#'
#' @return An S3 object of class `"monteriskr_risk"` with elements:
#'   \item{demand, attendance, sold_out, revenue, profit}{Matrices with
#'     `n_sims` rows and one column per event in `newdata`.}
#'   \item{total_profit}{Numeric vector of length `n_sims`; profit summed over events.}
#'   \item{draw_ids}{Integer vector of posterior draws used, one per run.}
#'   \item{newdata}{The scenario data frame.}
#'   \item{inputs}{List of resolved `price`, `variable_cost`, `fixed_cost`, `capacity`.}
#'
#' @export
#' @examples
#' fit_file <- system.file("extdata", "example_fit.rds", package = "monteriskr")
#' fit <- readRDS(fit_file)
#' scenario <- data.frame(
#'   day_type = factor(c("weekday", "weekend"), levels = c("weekday", "weekend")),
#'   marketing = c(0, 1)
#' )
#' risk <- simulate_risk(fit, scenario, n_sims = 200, price = 40,
#'                       variable_cost = 5, fixed_cost = 3000, seed = 1)
#' risk
simulate_risk <- function(fit,
                          newdata,
                          n_sims = 1000,
                          price = 1,
                          variable_cost = 0,
                          fixed_cost = 0,
                          capacity = NULL,
                          seed = NULL) {
  if (!inherits(fit, "monteriskr_fit")) {
    stop("`fit` must be a monteriskr_fit object from fit_demand().", call. = FALSE)
  }
  if (!is.data.frame(newdata) || nrow(newdata) == 0) {
    stop("`newdata` must be a data.frame with at least one row.", call. = FALSE)
  }
  if (!is.numeric(n_sims) || length(n_sims) != 1 || is.na(n_sims) ||
      n_sims < 1 || n_sims != as.integer(n_sims)) {
    stop("`n_sims` must be a single positive integer.", call. = FALSE)
  }
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || is.na(seed))) {
    stop("`seed` must be a single integer or NULL.", call. = FALSE)
  }

  missing_covs <- setdiff(fit$covariates, names(newdata))
  if (length(missing_covs) > 0) {
    stop("`newdata` is missing covariate column(s): ",
         paste(sprintf("'%s'", missing_covs), collapse = ", "), call. = FALSE)
  }
  if (length(fit$covariates) > 0 && anyNA(newdata[fit$covariates])) {
    stop("`newdata` covariates must not contain missing values.", call. = FALSE)
  }

  n_events <- nrow(newdata)
  price_v <- resolve_per_event(price, newdata, "price")
  vcost_v <- resolve_per_event(variable_cost, newdata, "variable_cost")
  fcost_v <- resolve_per_event(fixed_cost, newdata, "fixed_cost")

  if (is.null(capacity)) {
    capacity <- fit$capacity
  }
  cap_v <- if (is.null(capacity)) {
    rep(Inf, n_events)
  } else {
    resolve_per_event(capacity, newdata, "capacity")
  }
  if (any(cap_v <= 0)) {
    stop("`capacity` must be positive.", call. = FALSE)
  }

  n_sims <- as.integer(n_sims)
  n_post <- brms::ndraws(fit$brmsfit)

  demand <- with_temp_seed(seed, {
    draw_ids <- sample.int(n_post, n_sims, replace = n_sims > n_post)
    list(
      draw_ids = draw_ids,
      demand = brms::posterior_predict(fit$brmsfit, newdata = newdata,
                                       draw_ids = draw_ids)
    )
  })
  draw_ids <- demand$draw_ids
  demand <- demand$demand
  dimnames(demand) <- NULL

  cap_m <- matrix(cap_v, nrow = n_sims, ncol = n_events, byrow = TRUE)
  attendance <- pmin(demand, cap_m)
  sold_out <- demand >= cap_m
  revenue <- attendance * matrix(price_v, n_sims, n_events, byrow = TRUE)
  profit <- attendance * matrix(price_v - vcost_v, n_sims, n_events, byrow = TRUE) -
    matrix(fcost_v, n_sims, n_events, byrow = TRUE)

  res <- list(
    demand       = demand,
    attendance   = attendance,
    sold_out     = sold_out,
    revenue      = revenue,
    profit       = profit,
    total_profit = rowSums(profit),
    draw_ids     = draw_ids,
    newdata      = newdata,
    inputs       = list(price = price_v, variable_cost = vcost_v,
                        fixed_cost = fcost_v, capacity = cap_v)
  )
  class(res) <- "monteriskr_risk"
  res
}

#' @export
print.monteriskr_risk <- function(x, ...) {
  cat("--- monteriskr Risk Simulation ---\n")
  cat(sprintf("Simulations: %d | Events: %d\n", nrow(x$demand), ncol(x$demand)))
  cat(sprintf("Mean attendance per event: %s\n",
              paste(format(round(colMeans(x$attendance), 1)), collapse = ", ")))
  cat(sprintf("Sell-out probability per event: %s\n",
              paste(format(round(colMeans(x$sold_out), 3)), collapse = ", ")))
  cat(sprintf("Total profit: mean %s, 5%% quantile %s, 95%% quantile %s\n",
              format(round(mean(x$total_profit), 1)),
              format(round(stats::quantile(x$total_profit, 0.05, names = FALSE), 1)),
              format(round(stats::quantile(x$total_profit, 0.95, names = FALSE), 1))))
  cat("Use risk_summary() for VaR/CVaR.\n")
  invisible(x)
}
