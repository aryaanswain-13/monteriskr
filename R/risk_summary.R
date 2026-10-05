#' Risk metrics for a vector of simulated profits
#'
#' Internal helper. Loss is defined as `-profit`; VaR is the `level`-quantile of
#' loss and CVaR is the mean loss at or beyond VaR.
#'
#' @param profit Numeric vector of simulated profits.
#' @param level Single number in (0, 1).
#' @return Named numeric vector.
#' @keywords internal
risk_metrics <- function(profit, level) {
  loss <- -profit
  var_level <- stats::quantile(loss, level, names = FALSE)
  c(
    mean_profit   = mean(profit),
    sd_profit     = stats::sd(profit),
    p05_profit    = stats::quantile(profit, 0.05, names = FALSE),
    median_profit = stats::quantile(profit, 0.50, names = FALSE),
    p95_profit    = stats::quantile(profit, 0.95, names = FALSE),
    prob_loss     = mean(profit < 0),
    var           = var_level,
    cvar          = mean(loss[loss >= var_level])
  )
}

check_level <- function(level) {
  if (!is.numeric(level) || length(level) != 1 || is.na(level) ||
      level <= 0 || level >= 1) {
    stop("`level` must be a single number strictly between 0 and 1.", call. = FALSE)
  }
}


#' Summarise Simulated Profit Risk
#'
#' Computes headline risk metrics from a [simulate_risk()] result, for total
#' profit (summed over all events) and for each event separately.
#'
#' @param risk A `"monteriskr_risk"` object from [simulate_risk()].
#' @param level Single number in (0, 1); confidence level for VaR and CVaR.
#'   Default is 0.95.
#'
#' @details
#' Convention: loss is `-profit`. VaR is the `level`-quantile of loss, so a VaR
#' of 500 at level 0.95 means that in 95% of simulated outcomes the loss is at
#' most 500 (a negative VaR means a profit even in the bad tail). CVaR is the
#' mean loss in the simulated outcomes at or beyond VaR, i.e. the average
#' loss in the worst `1 - level` share of outcomes.
#'
#' @return An S3 object of class `"monteriskr_summary"` with elements:
#'   \item{total}{One-row `data.frame` of metrics for total profit.}
#'   \item{events}{`data.frame` with one row per event: the same metrics for
#'     event profit, plus `mean_attendance` and `prob_sold_out`.}
#'   \item{level}{The confidence level used.}
#'   \item{n_sims}{Number of simulation runs.}
#'
#'   Metric columns are `mean_profit`, `sd_profit`, `p05_profit`,
#'   `median_profit`, `p95_profit`, `prob_loss`, `var` and `cvar`.
#'
#' @export
#' @examples
#' fit <- readRDS(system.file("extdata", "example_fit.rds", package = "monteriskr"))
#' scenario <- data.frame(
#'   day_type = factor(c("weekday", "weekend"), levels = c("weekday", "weekend")),
#'   marketing = c(0, 1)
#' )
#' risk <- simulate_risk(fit, scenario, n_sims = 200, price = 40,
#'                       fixed_cost = 3000, seed = 1)
#' risk_summary(risk, level = 0.95)
risk_summary <- function(risk, level = 0.95) {
  if (!inherits(risk, "monteriskr_risk")) {
    stop("`risk` must be a monteriskr_risk object from simulate_risk().", call. = FALSE)
  }
  check_level(level)

  total <- as.data.frame(as.list(risk_metrics(risk$total_profit, level)))

  n_events <- ncol(risk$profit)
  ev_rows <- lapply(seq_len(n_events), function(j) {
    as.data.frame(as.list(risk_metrics(risk$profit[, j], level)))
  })
  events <- cbind(
    data.frame(event = seq_len(n_events)),
    do.call(rbind, ev_rows),
    mean_attendance = colMeans(risk$attendance),
    prob_sold_out   = colMeans(risk$sold_out)
  )

  res <- list(total = total, events = events, level = level,
              n_sims = nrow(risk$profit))
  class(res) <- "monteriskr_summary"
  res
}

#' @export
print.monteriskr_summary <- function(x, digits = 3, ...) {
  cat("--- monteriskr Risk Summary ---\n")
  cat(sprintf("Simulations: %d | VaR/CVaR level: %g (loss = -profit)\n\n",
              x$n_sims, x$level))
  cat("Total profit:\n")
  print.data.frame(x$total, row.names = FALSE, digits = digits)
  cat("\nPer event:\n")
  print.data.frame(x$events, row.names = FALSE, digits = digits)
  invisible(x)
}


#' Compare Risk Across Scenarios
#'
#' Summarises several [simulate_risk()] results side by side, so alternative
#' plans (for example different prices, marketing levels, venues or event mixes)
#' can be compared on the same risk metrics.
#'
#' @param ... Named `"monteriskr_risk"` objects, one per scenario. A single named
#'   list of such objects is also accepted. The first scenario is the baseline.
#' @param level Single number in (0, 1); confidence level for VaR and CVaR.
#'   Default is 0.95.
#'
#' @details
#' Metrics refer to total profit summed over the events of each scenario; see
#' [risk_summary()] for definitions. Scenarios may have different numbers of
#' events and simulation runs. `diff_mean_profit` is the scenario's mean profit
#' minus the baseline's mean profit.
#'
#' @return An S3 object of class `"monteriskr_comparison"`: a `data.frame` with
#'   one row per scenario and columns `scenario`, `n_events`, `mean_profit`,
#'   `sd_profit`, `p05_profit`, `median_profit`, `p95_profit`, `prob_loss`,
#'   `var`, `cvar`, `mean_attendance` (total over events) and `diff_mean_profit`.
#'
#' @export
#' @examples
#' fit <- readRDS(system.file("extdata", "example_fit.rds", package = "monteriskr"))
#' weekend <- data.frame(
#'   day_type = factor("weekend", levels = c("weekday", "weekend")),
#'   marketing = 1
#' )
#' low  <- simulate_risk(fit, weekend, n_sims = 200, price = 30,
#'                       fixed_cost = 3000, seed = 1)
#' high <- simulate_risk(fit, weekend, n_sims = 200, price = 50,
#'                       fixed_cost = 3000, seed = 1)
#' compare_scenarios(low_price = low, high_price = high)
compare_scenarios <- function(..., level = 0.95) {
  scenarios <- list(...)
  if (length(scenarios) == 1 && is.list(scenarios[[1]]) &&
      !inherits(scenarios[[1]], "monteriskr_risk")) {
    scenarios <- scenarios[[1]]
  }
  if (length(scenarios) < 1) {
    stop("Provide at least one monteriskr_risk object.", call. = FALSE)
  }
  if (!all(vapply(scenarios, inherits, logical(1), what = "monteriskr_risk"))) {
    stop("All scenarios must be monteriskr_risk objects from simulate_risk().",
         call. = FALSE)
  }
  nms <- names(scenarios)
  if (is.null(nms) || any(is.na(nms)) || any(nms == "") || anyDuplicated(nms)) {
    stop("Scenarios must have unique, non-empty names.", call. = FALSE)
  }
  check_level(level)

  rows <- lapply(scenarios, function(r) {
    m <- as.data.frame(as.list(risk_metrics(r$total_profit, level)))
    cbind(n_events = ncol(r$profit), m,
          mean_attendance = mean(rowSums(r$attendance)))
  })
  res <- cbind(scenario = nms, do.call(rbind, rows), stringsAsFactors = FALSE)
  rownames(res) <- NULL
  res$diff_mean_profit <- res$mean_profit - res$mean_profit[1]

  attr(res, "level") <- level
  class(res) <- c("monteriskr_comparison", "data.frame")
  res
}

#' @export
print.monteriskr_comparison <- function(x, digits = 3, ...) {
  cat("--- monteriskr Scenario Comparison ---\n")
  cat(sprintf("VaR/CVaR level: %g (loss = -profit) | baseline: %s\n\n",
              attr(x, "level"), x$scenario[1]))
  print.data.frame(as.data.frame(unclass(x), stringsAsFactors = FALSE),
                   row.names = FALSE, digits = digits)
  invisible(x)
}
