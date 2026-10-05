# Project: monteriskr (R package, targeting CRAN)

## Stack
R, roxygen2 for docs, testthat (3rd edition) for tests, brms for modeling.
No Shiny, no cmdstanr, no golem in v0.1.

## Rules
- Write a short plan before editing. Bundle related fixes before running checks.
- Run commands from the package root: devtools::document(), devtools::test(), devtools::check().
- Wait for every terminal process to fully finish before reading its output.
- If a check or compile fails twice in a row, stop and ask me.
- Never run install.packages(); tell me which package is missing.
- Do not edit DESCRIPTION dependencies without telling me.
- Never write outside this folder. Use tempdir() in examples and tests.
- Examples must run in under 5 seconds; wrap slow ones in \donttest{}.
- Tests that fit brms models need skip_on_cran().
- Show me the full R CMD check output; don't just say it passed.
- Do not modify statistical model code without explaining the change.
- Do not call options() or set.seed() globally inside package functions.
- Commit to git after each working step.
- Read PLAN.md at the start of every session. Update a milestone's
  status only when it is finished, and never mark one done until its
  check output has been shown to me.
- After each committed working step, push to origin main (https://github.com/aryaanswain-13/monteriskr). Never force-push or rewrite pushed history without asking.