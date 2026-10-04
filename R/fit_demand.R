#' Fit Bayesian Demand Model with Capacity Censoring
#'
#' Fits a Bayesian negative binomial regression model to historical event
#' attendance data using `brms`. When venue capacity is specified, events where
#' attendance reached capacity are modeled as right-censored observations.
#'
#' @param data A `data.frame` containing event data.
#' @param outcome Single character string; name of the count outcome column (e.g. attendance).
#' @param covariates Optional character vector of covariate column names.
#' @param capacity Optional character string (column name in `data`) or single positive number.
#' @param prior Prior specification passed to `brms::brm()`. Defaults to
#'   `brms::prior(normal(0, 1), class = "b")`, placing a weakly informative
#'   \eqn{N(0, 1)} prior on regression coefficients. Set to `NULL` for flat priors.
#' @param chains Number of Markov chains for MCMC sampling. Default is 2.
#' @param iter Total number of iterations per chain. Default is 1000.
#' @param seed Optional random seed passed directly to `brms::brm()`. Default is `NULL`.
#' @param ... Additional arguments passed to `brms::brm()`.
#'
#' @details
#' By default, `fit_demand()` applies a weakly informative \eqn{N(0, 1)} prior on
#' regression coefficients (`class = "b"`). The intercept uses `brms`'s default
#' `student_t(3, y_median, 2.5)` prior, and the negative binomial shape parameter
#' \eqn{\phi} uses `inv_gamma(0.4, 0.3)`.
#'
#' When capacity censoring is applied, observations where outcome equals or exceeds
#' capacity are right-censored. For these rows, `outcome - 1` is passed to `brms` so
#' that `neg_binomial_2_lccdf` evaluates \eqn{P(D \ge \text{capacity}) = P(D > \text{capacity} - 1)}.
#' The original outcome is retained in the returned fit object data.
#'
#' @return An S3 object of class `"monteriskr_fit"` containing:
#'   \item{brmsfit}{The fitted `brmsfit` object.}
#'   \item{formula}{The `brms` model formula used.}
#'   \item{outcome}{Name of the outcome variable.}
#'   \item{covariates}{Vector of covariate names used.}
#'   \item{capacity}{Capacity value or column name supplied.}
#'   \item{priors}{Data frame of priors used by `brms` for this model.}
#'   \item{check}{Result of `check_data()`.}
#'
#' @export
#' @examples
#' \donttest{
#' # Use pre-compiled fit fixture if available to avoid long Stan compilation
#' fit_file <- system.file("extdata", "example_fit.rds", package = "monteriskr")
#' if (file.exists(fit_file)) {
#'   fit <- readRDS(fit_file)
#' } else {
#'   data(example_events)
#'   fit <- fit_demand(
#'     data = example_events,
#'     outcome = "attendance",
#'     covariates = "marketing",
#'     capacity = 250,
#'     chains = 1,
#'     iter = 100,
#'     seed = 123
#'   )
#' }
#' print(fit)
#' }
fit_demand <- function(data,
                       outcome,
                       covariates = NULL,
                       capacity = NULL,
                       prior = brms::prior(normal(0, 1), class = "b"),
                       chains = 2,
                       iter = 1000,
                       seed = NULL,
                       ...) {
  # 1. Run check_data
  check_res <- check_data(data = data, outcome = outcome, covariates = covariates, capacity = capacity)
  if (!check_res$valid) {
    stop("Data quality check failed:\n", paste("  -", check_res$errors, collapse = "\n"), call. = FALSE)
  }

  fit_data <- data

  # 2. Build formula with right-censoring if capacity is specified
  if (!is.null(covariates) && length(covariates) > 0) {
    cov_str <- paste(covariates, collapse = " + ")
  } else {
    cov_str <- "1"
  }

  if (!is.null(capacity)) {
    if (is.character(capacity)) {
      cap_vec <- fit_data[[capacity]]
    } else {
      cap_vec <- rep(capacity, nrow(fit_data))
    }
    # Censored indicator: 1 = right-censored (true demand >= capacity), 0 = uncensored
    is_cens <- fit_data[[outcome]] >= cap_vec
    fit_data$censored <- as.numeric(is_cens)
    # Censoring fix: pass outcome - 1 for right-censored rows so neg_binomial_2_lccdf evaluates P(D >= capacity)
    fit_data[[outcome]] <- ifelse(is_cens, fit_data[[outcome]] - 1, fit_data[[outcome]])

    form_str <- sprintf("%s | cens(censored) ~ %s", outcome, cov_str)
  } else {
    form_str <- sprintf("%s ~ %s", outcome, cov_str)
  }

  form <- stats::as.formula(form_str)

  # Handle prior specification
  if (!is.null(prior)) {
    if (is.null(covariates) || length(covariates) == 0) {
      # For intercept-only model, drop class = 'b' prior to avoid brms validation error
      if (inherits(prior, "brmsprior")) {
        prior_to_use <- prior[prior$class != "b", ]
        if (nrow(prior_to_use) == 0) {
          prior_to_use <- NULL
        }
      } else {
        prior_to_use <- prior
      }
    } else {
      prior_to_use <- prior
    }
  } else {
    prior_to_use <- NULL
  }

  # 3. Retrieve priors for transparency
  brms_priors <- brms::get_prior(formula = form, data = fit_data, family = brms::negbinomial(), prior = prior_to_use)

  # 4. Fit model via brms::brm
  brms_args <- list(
    formula = form,
    data = fit_data,
    family = brms::negbinomial(),
    prior = prior_to_use,
    chains = chains,
    iter = iter,
    refresh = 0
  )

  if (!is.null(seed)) {
    brms_args$seed <- as.integer(seed)
  }

  extra_args <- list(...)
  if (length(extra_args) > 0) {
    brms_args <- utils::modifyList(brms_args, extra_args)
  }

  fit_obj <- do.call(brms::brm, brms_args)

  # Keep original outcome in stored brmsfit data object
  fit_obj$data[[outcome]] <- data[[outcome]]

  res <- list(
    brmsfit = fit_obj,
    formula = form,
    outcome = outcome,
    covariates = covariates,
    capacity = capacity,
    priors = brms_priors,
    check = check_res
  )

  class(res) <- "monteriskr_fit"
  res
}

#' @export
print.monteriskr_fit <- function(x, ...) {
  cat("--- monteriskr Bayesian Demand Fit ---\n")
  cat(sprintf("Outcome: %s\n", x$outcome))
  cat(sprintf("Covariates: %s\n", if (is.null(x$covariates)) "None (intercept-only)" else paste(x$covariates, collapse = ", ")))
  cat(sprintf("Capacity Censoring: %s\n", if (is.null(x$capacity)) "None" else paste(x$capacity)))
  cat(sprintf("Formula: %s\n\n", deparse(x$formula)))

  cat("Priors Used:\n")
  priors_df <- as.data.frame(x$priors[, c("prior", "class", "coef", "group")])
  print.data.frame(priors_df, row.names = FALSE)
  cat("\nModel Fit Summary:\n")
  print(x$brmsfit)

  invisible(x)
}

#' @export
summary.monteriskr_fit <- function(object, ...) {
  cat("=== monteriskr Demand Fit Summary ===\n")
  summary(object$brmsfit, ...)
}

