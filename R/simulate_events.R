#' Simulate Synthetic Event Data
#'
#' Generates synthetic event data under a negative binomial demand model with
#' optional capacity capping and right-censored attendance records.
#'
#' @param n Single positive integer; number of events to simulate. Default is 100.
#' @param beta Named numeric vector of regression coefficients with components
#'   `intercept`, `weekend`, and `marketing`. Default is `c(intercept = 5, weekend = 0.3, marketing = 0.2)`.
#' @param phi Single positive number; negative binomial precision parameter (\eqn{\phi > 0}). Default is 20.
#' @param capacity Single positive number or `NULL`/`Inf`; event venue capacity. Default is 400.
#' @param seed Optional integer; random seed for reproducibility. Default is `NULL`.
#'
#' @return A `data.frame` with columns:
#'   \item{day_type}{Factor with levels `"weekday"` and `"weekend"`.}
#'   \item{marketing}{Numeric standardised marketing predictor.}
#'   \item{true_demand}{Integer unconstrained negative binomial demand.}
#'   \item{attendance}{Integer observed attendance capped at capacity.}
#'   \item{sold_out}{Logical indicator for `true_demand >= capacity`.}
#'   The returned object also has a `"true_params"` attribute containing the input parameters.
#'
#' @export
#' @examples
#' events <- simulate_events(n = 50, seed = 123)
#' head(events)
#' attr(events, "true_params")
simulate_events <- function(n = 100,
                            beta = c(intercept = 5, weekend = 0.3, marketing = 0.2),
                            phi = 20,
                            capacity = 400,
                            seed = NULL) {
  # Input validation
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n <= 0 || n != as.integer(n)) {
    stop("`n` must be a single positive integer.", call. = FALSE)
  }

  if (!is.numeric(beta) || length(beta) != 3) {
    stop("`beta` must be a numeric vector of length 3.", call. = FALSE)
  }

  expected_names <- c("intercept", "weekend", "marketing")
  if (is.null(names(beta))) {
    names(beta) <- expected_names
  } else if (!all(expected_names %in% names(beta))) {
    stop("`beta` must contain named elements: 'intercept', 'weekend', 'marketing'.", call. = FALSE)
  }
  beta <- beta[expected_names]

  if (!is.numeric(phi) || length(phi) != 1 || is.na(phi) || phi <= 0) {
    stop("`phi` must be a single positive number.", call. = FALSE)
  }

  if (!is.null(capacity)) {
    if (!is.numeric(capacity) || length(capacity) != 1 || is.na(capacity) || capacity <= 0) {
      stop("`capacity` must be a single positive number or NULL/Inf.", call. = FALSE)
    }
  } else {
    capacity <- Inf
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
      stop("`seed` must be a single integer or NULL.", call. = FALSE)
    }
    seed <- as.integer(seed)

    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
    } else {
      on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
    }
    set.seed(seed)
  }

  # Simulation
  day_type <- factor(
    sample(c("weekday", "weekend"), size = n, replace = TRUE, prob = c(0.7, 0.3)),
    levels = c("weekday", "weekend")
  )
  marketing <- stats::rnorm(n, mean = 0, sd = 1)
  is_weekend <- as.numeric(day_type == "weekend")

  log_mu <- beta["intercept"] + beta["weekend"] * is_weekend + beta["marketing"] * marketing
  mu <- exp(log_mu)

  true_demand <- stats::rnbinom(n = n, mu = mu, size = phi)

  if (is.finite(capacity)) {
    attendance <- pmin(true_demand, capacity)
    sold_out <- true_demand >= capacity
  } else {
    attendance <- true_demand
    sold_out <- rep(FALSE, n)
  }

  df <- data.frame(
    day_type = day_type,
    marketing = marketing,
    true_demand = as.integer(true_demand),
    attendance = as.integer(attendance),
    sold_out = sold_out,
    stringsAsFactors = FALSE
  )

  attr(df, "true_params") <- list(
    beta = beta,
    phi = phi,
    capacity = capacity,
    seed = seed
  )

  df
}
