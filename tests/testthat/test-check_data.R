test_that("check_data passes on valid data", {
  df <- simulate_events(n = 30, seed = 123)
  res <- check_data(df, outcome = "attendance", covariates = c("day_type", "marketing"), capacity = 400)
  expect_true(res$valid)
  expect_length(res$errors, 0)
  expect_length(res$warnings, 0)
})

test_that("check_data warns for rows under 20 and errors under 8", {
  df_small <- simulate_events(n = 15, seed = 123)
  res_small <- check_data(df_small, outcome = "attendance", covariates = "marketing")
  expect_true(res_small$valid)
  expect_length(res_small$warnings, 1)
  expect_match(res_small$warnings[1], "fewer than 20 rows")

  df_tiny <- simulate_events(n = 5, seed = 123)
  res_tiny <- check_data(df_tiny, outcome = "attendance", covariates = "marketing")
  expect_false(res_tiny$valid)
  expect_true(any(grepl("minimum required for fitting is 8", res_tiny$errors)))
})

test_that("check_data detects missing columns", {
  df <- simulate_events(n = 25, seed = 1)
  res <- check_data(df, outcome = "nonexistent_outcome", covariates = c("marketing", "missing_cov"), capacity = "missing_cap")
  expect_false(res$valid)
  expect_true(any(grepl("Outcome column 'nonexistent_outcome' not found", res$errors)))
  expect_true(any(grepl("Covariate column\\(s\\) not found", res$errors)))
  expect_true(any(grepl("Capacity column 'missing_cap' not found", res$errors)))
})

test_that("check_data validates invalid outcome values", {
  df <- simulate_events(n = 25, seed = 1)

  # Non-numeric outcome
  df_chr <- df
  df_chr$attendance <- as.character(df_chr$attendance)
  res_chr <- check_data(df_chr, outcome = "attendance")
  expect_false(res_chr$valid)
  expect_true(any(grepl("must be numeric", res_chr$errors)))

  # Negative outcome
  df_neg <- df
  df_neg$attendance[1] <- -5
  res_neg <- check_data(df_neg, outcome = "attendance")
  expect_false(res_neg$valid)
  expect_true(any(grepl("contains negative values", res_neg$errors)))

  # Non-integer outcome
  df_dec <- df
  df_dec$attendance[1] <- 12.5
  res_dec <- check_data(df_dec, outcome = "attendance")
  expect_false(res_dec$valid)
  expect_true(any(grepl("contains non-integer values", res_dec$errors)))
})

test_that("check_data detects missing values (NA/NaN)", {
  df <- simulate_events(n = 25, seed = 1)

  # NA in outcome
  df_na_y <- df
  df_na_y$attendance[3] <- NA
  res_na_y <- check_data(df_na_y, outcome = "attendance")
  expect_false(res_na_y$valid)
  expect_true(any(grepl("Outcome column 'attendance' contains missing values", res_na_y$errors)))

  # NA in covariate
  df_na_x <- df
  df_na_x$marketing[2] <- NA
  res_na_x <- check_data(df_na_x, outcome = "attendance", covariates = "marketing")
  expect_false(res_na_x$valid)
  expect_true(any(grepl("Covariate column 'marketing' contains missing values", res_na_x$errors)))

  # NA in capacity column
  df_cap_na <- df
  df_cap_na$cap_col <- 300
  df_cap_na$cap_col[5] <- NA
  res_cap_na <- check_data(df_cap_na, outcome = "attendance", capacity = "cap_col")
  expect_false(res_cap_na$valid)
  expect_true(any(grepl("Capacity column 'cap_col' contains missing values", res_cap_na$errors)))
})

test_that("check_data detects outcome exceeding capacity", {
  df <- simulate_events(n = 25, seed = 1)

  # Scalar capacity violation
  df$attendance[10] <- 500
  res_scalar <- check_data(df, outcome = "attendance", capacity = 400)
  expect_false(res_scalar$valid)
  expect_true(any(grepl("exceeds specified capacity limit of 400", res_scalar$errors)))

  # Column capacity violation
  df$cap_col <- 400
  df$cap_col[10] <- 450 # attendance is 500 > 450
  res_col <- check_data(df, outcome = "attendance", capacity = "cap_col")
  expect_false(res_col$valid)
  expect_true(any(grepl("exceeds capacity specified in column 'cap_col'", res_col$errors)))
})

test_that("check_data detects zero-variance covariates and single-level factors", {
  df <- simulate_events(n = 25, seed = 1)

  # Zero variance numeric covariate
  df$const_num <- 5.0
  res_const <- check_data(df, outcome = "attendance", covariates = "const_num")
  expect_false(res_const$valid)
  expect_true(any(grepl("has zero variance", res_const$errors)))

  # Single level factor
  df$const_fact <- factor(rep("A", 25))
  res_fact <- check_data(df, outcome = "attendance", covariates = "const_fact")
  expect_false(res_fact$valid)
  expect_true(any(grepl("has only 1 unique level/value", res_fact$errors)))
})

test_that("check_data detects perfectly collinear covariates", {
  df <- simulate_events(n = 25, seed = 1)
  df$x1 <- df$marketing
  df$x2 <- df$marketing * 2 + 3 # Perfectly correlated with x1
  res_coll <- check_data(df, outcome = "attendance", covariates = c("x1", "x2"))
  expect_false(res_coll$valid)
  expect_true(any(grepl("are perfectly collinear", res_coll$errors)))
})

test_that("print.monteriskr_check works", {
  df <- simulate_events(n = 15, seed = 1)
  df$attendance[1] <- -1
  res <- check_data(df, outcome = "attendance", covariates = "marketing")
  expect_output(print(res), "monteriskr Data Check")
  expect_output(print(res), "Status: FAILED")
  expect_output(print(res), "Errors:")
  expect_output(print(res), "Warnings:")
})
