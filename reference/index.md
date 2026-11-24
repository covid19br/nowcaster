# Package index

## Nowcasting

Workhorse functions. Both fit a negative binomial model to the delay
triangle;
[`nowcasting_inla()`](https://covid19br.github.io/nowcaster/reference/nowcasting_inla.md)
uses ‘INLA’ and
[`nowcasting_mgcv()`](https://covid19br.github.io/nowcaster/reference/nowcasting_mgcv.md)
uses ‘mgcv’.

- [`nowcasting_inla()`](https://covid19br.github.io/nowcaster/reference/nowcasting_inla.md)
  : Nowcasting with a Bayesian negative binomial model fitted by 'INLA'
- [`nowcasting_mgcv()`](https://covid19br.github.io/nowcaster/reference/nowcasting_mgcv.md)
  : Nowcasting with a negative binomial GAM fitted by 'mgcv'

## Model fitting

Lower-level functions called by the workhorse functions.

- [`nowcasting_no_age_inla()`](https://covid19br.github.io/nowcaster/reference/nowcasting_no_age_inla.md)
  : Fit the non-structured nowcasting model with 'INLA'
- [`nowcasting_age_inla()`](https://covid19br.github.io/nowcaster/reference/nowcasting_age_inla.md)
  : Fit the structured (age) nowcasting model with 'INLA'
- [`nowcasting_no_age_mgcv()`](https://covid19br.github.io/nowcaster/reference/nowcasting_no_age_mgcv.md)
  : Fit the non-structured nowcasting GAM
- [`nowcasting_age_mgcv()`](https://covid19br.github.io/nowcaster/reference/nowcasting_age_mgcv.md)
  : Fit the structured (age) nowcasting GAM

## Post-processing

- [`nowcasting.summary()`](https://covid19br.github.io/nowcaster/reference/nowcasting.summary.md)
  : Summarise nowcasting trajectories
- [`slope.estimate.quant()`](https://covid19br.github.io/nowcaster/reference/slope.estimate.quant.md)
  : Estimate the slope of the nowcasted curve

## Data

- [`sragBH`](https://covid19br.github.io/nowcaster/reference/sragBH.md)
  : SARI data from Belo Horizonte
