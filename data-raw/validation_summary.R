# Summarise the parameter-recovery study (M6) from the saved replications.
#
# Fits that did not converge (max Rhat > 1.05) are excluded before computing
# metrics, for both models alike, and the number excluded is reported. Medians
# are reported next to means because a single diverged fit can dominate a mean.
#
# Run from the package root:  Rscript data-raw/validation_summary.R

path <- file.path("inst", "extdata", "validation_results.rds")
out  <- readRDS(path)
reps <- out$reps
rhat_max <- 1.05

reps$converged <- reps$max_rhat <= rhat_max
fit_status <- unique(reps[, c("scenario", "model", "rep", "converged")])
n_excl <- stats::aggregate(converged ~ scenario + model, fit_status,
                           function(x) sum(!x))
names(n_excl)[3] <- "n_excluded"

ok <- reps[reps$converged, ]
ok$covered <- as.numeric(ok$covered)
ok$sq_error <- ok$error^2
grp <- ok[, c("scenario", "model", "parameter")]

agg <- stats::aggregate(
  cbind(bias = error, rmse = sq_error, coverage90 = covered,
        sold_out_share = sold_out_share) ~ scenario + model + parameter,
  data = ok, FUN = mean
)
agg$rmse <- sqrt(agg$rmse)
agg$median_bias <- stats::aggregate(error ~ scenario + model + parameter, ok,
                                    stats::median)$error
agg$n_fits <- stats::aggregate(rep ~ scenario + model + parameter, ok,
                               length)$rep
agg <- merge(agg, n_excl, by = c("scenario", "model"))
agg$truth <- unname(out$settings$truth[agg$parameter])
agg <- agg[order(agg$scenario, agg$parameter, agg$model),
           c("scenario", "model", "parameter", "truth", "n_fits", "n_excluded",
             "bias", "median_bias", "rmse", "coverage90", "sold_out_share")]
rownames(agg) <- NULL

out$summary <- agg
out$settings$rhat_max <- rhat_max
saveRDS(out, path, compress = "xz")
options(width = 140)
print(agg, digits = 3)
