# nowcaster (development version)

* `nowcasting_mgcv()` and `nowcasting_inla()` are much faster at building the
  predictive trajectories: the 1,000 sampled delay triangles are now aggregated
  at once, as a matrix, instead of one at a time. On the `sragBH` example,
  `nowcasting_mgcv()` went from about 13 seconds to under 1 second. Results are
  identical for the same random seed. For the age-structured model the run time
  is now dominated by the model fit itself.
* Model-fitting functions are now named after the workhorse function that
  uses them: `nowcasting_no_age()` is now `nowcasting_no_age_inla()` and
  `nowcasting_age()` is now `nowcasting_age_inla()`, matching
  `nowcasting_no_age_mgcv()` and `nowcasting_age_mgcv()`. The old names still
  work but are deprecated. Functions shared by both workhorse functions
  (`nowcasting.summary()`, `slope.estimate.quant()`) keep names without suffix.
* The 'INLA' functions now stop with an informative message when 'INLA' is not
  installed.
* New article comparing `nowcasting_inla()` and `nowcasting_mgcv()` on the same
  examples, and a Portuguese version of the website.

# nowcaster 1.0.1

First release on CRAN.

## Nowcasting

* `nowcasting_mgcv()` fits the nowcasting model as a negative binomial GAM with
  'mgcv', for non-structured data and for data structured by age
  (`age_col`, `bins_age`, `method.group.gam`).
* `nowcasting_inla()` fits the Bayesian version of the model with 'INLA'
  (suggested, installed from the INLA repository), with options for
  zero-inflated data (`zero_inflated`), the latent time model (`timeREmodel`) and
  model-fit output (`INLAoutput`, `WAIC`, `DIC`).
* Both functions can forecast `K` weeks ahead and return the predictive
  trajectories (`trajectories = TRUE`).

## Other functions and data

* `nowcasting.summary()` summarises predictive trajectories into medians and
  50% / 95% intervals.
* `slope.estimate.quant()` estimates the slope of the nowcasted curve.
* `sragBH`: anonymised SARI records from Belo Horizonte, Brazil.
