#' Check Event Data Quality and Suitability
#'
#' Inspects input event data for potential issues prior to model fitting,
#' including missing columns, missing values, non-integer outcomes, capacity
#' violations, zero-variance covariates, single-level factors, perfect
#' collinearity, and sample size warnings/errors.
#'
#' @param data A `data.frame` containing event data.
#' @param outcome Single character string; name of the count outcome column.
#' @param covariates Optional character vector of covariate column names.
#' @param capacity Optional character string (column name in `data`) or single positive number.
#'
#' @return An S3 object of class `"monteriskr_check"` with elements:
#'   \item{valid}{Logical indicator; `TRUE` if no critical errors were found.}
#'   \item{errors}{Character vector of error messages.}
#'   \item{warnings}{Character vector of warning messages.}
#'   \item{info}{List summarizing dataset dimensions and columns evaluated.}
#'
#' @export
#' @examples
#' data(example_events)
#' check_res <- check_data(
#'   example_events,
#'   outcome = "attendance",
#'   covariates = "marketing",
#'   capacity = 250
#' )
#' check_res

check_data <- function(data, outcome, covariates = NULL, capacity = NULL) {
  errors <- character(0)
  warnings <- character(0)

  # 1. Validate data argument type
  if (!is.data.frame(data)) {
    errors <- c(errors, "`data` must be a data.frame or tibble.")
    res <- list(valid = FALSE, errors = errors, warnings = warnings, info = list())
    class(res) <- "monteriskr_check"
    return(res)
  }

  # 2. Check sample size (row count)
  n_rows <- nrow(data)
  if (n_rows < 8) {
    errors <- c(errors, sprintf("Data has only %d rows; minimum required for fitting is 8.", n_rows))
  } else if (n_rows < 20) {
    warnings <- c(warnings, sprintf("Data has only %d rows; fitting a model with fewer than 20 rows may yield unreliable estimates.", n_rows))
  }

  # 3. Validate outcome specification and column presence
  if (!is.character(outcome) || length(outcome) != 1 || is.na(outcome) || nchar(outcome) == 0) {
    errors <- c(errors, "`outcome` must be a single non-empty character string.")
    outcome_col_exists <- FALSE
  } else if (!outcome %in% names(data)) {
    errors <- c(errors, sprintf("Outcome column '%s' not found in data.", outcome))
    outcome_col_exists <- FALSE
  } else {
    outcome_col_exists <- TRUE
  }

  # 4. Validate covariates specification and column presence
  existing_covariates <- character(0)
  if (!is.null(covariates)) {
    if (!is.character(covariates) || length(covariates) == 0) {
      errors <- c(errors, "`covariates` must be a character vector of column names or NULL.")
    } else {
      missing_covs <- setdiff(covariates, names(data))
      if (length(missing_covs) > 0) {
        errors <- c(errors, sprintf("Covariate column(s) not found in data: %s", paste(sprintf("'%s'", missing_covs), collapse = ", ")))
      }
      existing_covariates <- intersect(covariates, names(data))
    }
  }

  # 5. Validate capacity specification and column/value presence
  capacity_val <- NULL
  capacity_col <- NULL
  capacity_valid <- FALSE

  if (!is.null(capacity)) {
    if (is.character(capacity) && length(capacity) == 1) {
      if (!capacity %in% names(data)) {
        errors <- c(errors, sprintf("Capacity column '%s' not found in data.", capacity))
      } else {
        capacity_col <- capacity
        capacity_valid <- TRUE
      }
    } else if (is.numeric(capacity) && length(capacity) == 1 && !is.na(capacity) && capacity > 0) {
      capacity_val <- capacity
      capacity_valid <- TRUE
    } else {
      errors <- c(errors, "`capacity` must be a single positive number or a column name string present in `data`.")
    }
  }

  # 6. Detailed checks on outcome column
  if (outcome_col_exists) {
    y <- data[[outcome]]
    if (anyNA(y)) {
      errors <- c(errors, sprintf("Outcome column '%s' contains missing values (NA/NaN).", outcome))
    }
    if (!is.numeric(y)) {
      errors <- c(errors, sprintf("Outcome column '%s' must be numeric.", outcome))
    } else {
      if (any(y < 0, na.rm = TRUE)) {
        errors <- c(errors, sprintf("Outcome column '%s' contains negative values.", outcome))
      }
      if (any(y != as.integer(y), na.rm = TRUE)) {
        errors <- c(errors, sprintf("Outcome column '%s' contains non-integer values.", outcome))
      }
    }
  }

  # 7. Detailed checks on capacity values & outcome vs capacity
  if (capacity_valid) {
    if (!is.null(capacity_col)) {
      cap_vec <- data[[capacity_col]]
      if (anyNA(cap_vec)) {
        errors <- c(errors, sprintf("Capacity column '%s' contains missing values (NA/NaN).", capacity_col))
      }
      if (!is.numeric(cap_vec) || any(cap_vec <= 0, na.rm = TRUE)) {
        errors <- c(errors, sprintf("Capacity column '%s' must contain positive numeric values.", capacity_col))
      } else if (outcome_col_exists && is.numeric(data[[outcome]])) {
        if (any(data[[outcome]] > cap_vec, na.rm = TRUE)) {
          errors <- c(errors, sprintf("Outcome column '%s' exceeds capacity specified in column '%s'.", outcome, capacity_col))
        }
      }
    } else if (!is.null(capacity_val)) {
      if (outcome_col_exists && is.numeric(data[[outcome]])) {
        if (any(data[[outcome]] > capacity_val, na.rm = TRUE)) {
          errors <- c(errors, sprintf("Outcome column '%s' exceeds specified capacity limit of %g.", outcome, capacity_val))
        }
      }
    }
  }

  # 8. Detailed checks on covariates
  if (length(existing_covariates) > 0) {
    num_covs <- character(0)
    for (cov in existing_covariates) {
      val <- data[[cov]]
      if (anyNA(val)) {
        errors <- c(errors, sprintf("Covariate column '%s' contains missing values (NA/NaN).", cov))
      }

      if (is.factor(val) || is.character(val)) {
        n_levels <- length(unique(val[!is.na(val)]))
        if (n_levels <= 1) {
          errors <- c(errors, sprintf("Covariate column '%s' has only %d unique level/value.", cov, n_levels))
        }
      } else if (is.numeric(val)) {
        u_vals <- unique(val[!is.na(val)])
        if (length(u_vals) <= 1) {
          errors <- c(errors, sprintf("Covariate column '%s' has zero variance (constant values).", cov))
        } else {
          num_covs <- c(num_covs, cov)
        }
      }
    }

    # Perfect collinearity check among numeric covariates
    if (length(num_covs) >= 2) {
      cov_mat <- stats::model.matrix(~ . - 1, data = data[num_covs])
      cor_mat <- stats::cor(cov_mat, use = "pairwise.complete.obs")

      for (i in 1:(ncol(cor_mat) - 1)) {
        for (j in (i + 1):ncol(cor_mat)) {
          if (!is.na(cor_mat[i, j]) && abs(abs(cor_mat[i, j]) - 1) < 1e-7) {
            errors <- c(errors, sprintf("Covariates '%s' and '%s' are perfectly collinear (correlation = %g).",
                                        colnames(cor_mat)[i], colnames(cor_mat)[j], cor_mat[i, j]))
          }
        }
      }
    }
  }

  valid_flag <- length(errors) == 0

  res <- list(
    valid = valid_flag,
    errors = errors,
    warnings = warnings,
    info = list(
      n_obs = n_rows,
      outcome = outcome,
      covariates = covariates,
      capacity = capacity
    )
  )
  class(res) <- "monteriskr_check"
  res
}

#' @export
print.monteriskr_check <- function(x, ...) {
  cat("--- monteriskr Data Check ---\n")
  cat(sprintf("Status: %s\n", if (x$valid) "PASSED" else "FAILED"))
  cat(sprintf("Observations: %d\n", x$info$n_obs))

  if (length(x$errors) > 0) {
    cat("\nErrors:\n")
    for (err in x$errors) {
      cat(sprintf("  - %s\n", err))
    }
  }

  if (length(x$warnings) > 0) {
    cat("\nWarnings:\n")
    for (warn in x$warnings) {
      cat(sprintf("  - %s\n", warn))
    }
  }

  if (x$valid && length(x$warnings) == 0) {
    cat("\nNo data quality issues detected.\n")
  }

  invisible(x)
}
