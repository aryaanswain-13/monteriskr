#' Example Event Attendance Data
#'
#' A synthetic dataset containing 60 historical event records with attendance,
#' marketing expenditure, day type (weekday vs. weekend), and venue capacity.
#'
#' @format A data frame with 60 rows and 5 variables:
#' \describe{
#'   \item{day_type}{Factor indicating whether the event occurred on a `"weekday"` or `"weekend"`.}
#'   \item{marketing}{Standardised marketing spend (mean ~ 0, sd ~ 1).}
#'   \item{true_demand}{Latent unconstrained demand generated from a negative binomial distribution.}
#'   \item{attendance}{Observed attendance, capped at the venue capacity (250).}
#'   \item{sold_out}{Logical flag indicating whether true demand reached or exceeded capacity (`attendance >= 250`).}
#' }
#' @source Simulated using [simulate_events()].
"example_events"
