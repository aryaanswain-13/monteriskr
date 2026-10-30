## Submission

This is a first submission.

## Test environments

* local macOS (aarch64), R 4.5.0
* win-builder, R-devel (Windows, ucrt)
* GitHub Actions: macOS, Ubuntu and Windows, R release (see `.github/workflows/R-CMD-check.yaml`)

## R CMD check results

0 errors | 0 warnings | 1 note

* "New submission": this is the first submission of the package.

## Notes for the reviewer

* The package fits models with 'brms', which requires a Stan toolchain. Tests that
  fit a model are wrapped in `skip_on_cran()`; examples and the vignette use a
  pre-computed fit shipped in `inst/extdata/`.
* `inst/extdata/validation_results.rds` is the stored output of a simulation study
  (`data-raw/validation.R`, which is not shipped in the built package).
