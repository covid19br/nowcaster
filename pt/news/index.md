# Changelog

## nowcaster (development version)

- Model-fitting functions are now named after the workhorse function
  that uses them:
  [`nowcasting_no_age()`](https://covid19br.github.io/nowcaster/pt/reference/nowcaster-deprecated.md)
  is now
  [`nowcasting_no_age_inla()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_no_age_inla.md)
  and
  [`nowcasting_age()`](https://covid19br.github.io/nowcaster/pt/reference/nowcaster-deprecated.md)
  is now
  [`nowcasting_age_inla()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_age_inla.md),
  matching
  [`nowcasting_no_age_mgcv()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_no_age_mgcv.md)
  and
  [`nowcasting_age_mgcv()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_age_mgcv.md).
  The old names still work but are deprecated. Functions shared by both
  workhorse functions
  ([`nowcasting.summary()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting.summary.md),
  [`slope.estimate.quant()`](https://covid19br.github.io/nowcaster/pt/reference/slope.estimate.quant.md))
  keep names without suffix.
- The ‘INLA’ functions now stop with an informative message when ‘INLA’
  is not installed.
- New article comparing
  [`nowcasting_inla()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_inla.md)
  and
  [`nowcasting_mgcv()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_mgcv.md)
  on the same examples, and a Portuguese version of the website.

## nowcaster 1.0.1

Lançamento CRAN: 2026-11-13

First release on CRAN.

### Nowcasting

- [`nowcasting_mgcv()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_mgcv.md)
  fits the nowcasting model as a negative binomial GAM with ‘mgcv’, for
  non-structured data and for data structured by age (`age_col`,
  `bins_age`, `method.group.gam`).
- [`nowcasting_inla()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_inla.md)
  fits the Bayesian version of the model with ‘INLA’ (suggested,
  installed from the INLA repository), with options for zero-inflated
  data (`zero_inflated`), the latent time model (`timeREmodel`) and
  model-fit output (`INLAoutput`, `WAIC`, `DIC`).
- Both functions can forecast `K` weeks ahead and return the predictive
  trajectories (`trajectories = TRUE`).

### Other functions and data

- [`nowcasting.summary()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting.summary.md)
  summarises predictive trajectories into medians and 50% / 95%
  intervals.
- [`slope.estimate.quant()`](https://covid19br.github.io/nowcaster/pt/reference/slope.estimate.quant.md)
  estimates the slope of the nowcasted curve.
- `sragBH`: anonymised SARI records from Belo Horizonte, Brazil.
