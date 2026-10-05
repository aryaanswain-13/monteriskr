# monteriskr build plan

Target: CRAN submission. Scope v0.1: count outcome, covariates, optional
capacity censoring, Monte Carlo risk metrics, scenario comparison.
Out of scope: Shiny, cmdstanr, golem, time series, multiple outcomes.

## Status
- [x] M1 Skeleton, simulate_events(), example_events
- [x] M2 check_data()
- [x] M3 fit_demand() with right-censoring (outcome - 1 shift for
      censored rows), normal(0, 1) prior on coefficients, fixture saved
- [x] M3 follow-ups: shared helper for the shift, store original data in
      its own slot (not in brmsfit$data), commit everything
- [x] M4 simulate_risk()
- [x] M5 risk_summary(), compare_scenarios()
- [x] M6 Validation (parameter recovery, with vs. without censoring)
- [ ] M7 Docs, vignette, README, NEWS, cran-comments, GitHub Actions
- [ ] Stretch: render_report()

## Key decisions
- Censoring: sold out means demand >= capacity. brms uses
  neg_binomial_2_lccdf = P(D > y), so censored rows get y = capacity - 1.
- Revenue is derived from attendance, never modelled separately.
- Each simulation run draws a parameter set from the posterior.
- Price changes are user assumptions unless price is a model covariate.
- Tests use the saved fixture; live brms fits only under skip_on_cran().
- VaR/CVaR convention: loss = -profit, VaR is the level-quantile of loss,
  CVaR is the mean loss at or beyond VaR.

## Rules
- Push to origin main after each committed working step; never force-push without asking.
- posterior goes back to Imports if any posterior:: call is added.

## Before CRAN submission
- [ ] Full check with network checks on (win-builder, R-hub)
- [ ] Maintainer name and email confirmed
- [ ] Repo public, URL and BugReports in DESCRIPTION
- [ ] Package name availability confirmed
