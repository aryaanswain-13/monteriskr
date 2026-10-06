## Submission

This is a first submission.

## Test environments

* local macOS (aarch64), R 4.5.0
* GitHub Actions: see `.github/workflows/R-CMD-check.yaml`
* win-builder and R-hub: not yet run (to do before submission)

## R CMD check results

Local `R CMD check --as-cran`: 0 errors | 0 warnings | 0 notes.

## Notes for the reviewer

* The package fits models with brms, which requires a Stan toolchain. Tests that
  fit a model are wrapped in `skip_on_cran()`; examples and the vignette use a
  pre-computed fit shipped in `inst/extdata/`.
* The `inst/extdata/validation_results.rds` file is the stored output of a
  simulation study (`data-raw/validation.R`, not shipped in the built package).
