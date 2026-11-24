# Fit the structured (age) nowcasting model with 'INLA'

Fits the Bayesian negative binomial model with 'INLA' on data structured
by age class and samples the posterior predictive trajectories. Data has
to be in the delay-triangle format. Usually called internally by
[`nowcasting_inla()`](https://covid19br.github.io/nowcaster/reference/nowcasting_inla.md).

## Usage

``` r
nowcasting_age_inla(
  dataset,
  zero_inflated = FALSE,
  timeREmodel = "rw2",
  INLAoutput = FALSE,
  INLAoutputOnly = FALSE,
  WAIC = FALSE,
  DIC = FALSE
)
```

## Arguments

- dataset:

  data pre formatted in to age classes and delays by week for each
  cases, delay triangle format

- zero_inflated:

  Experimental! It deals with zero-inflated data by using a
  zeroinflatednbinomial2 model.

- timeREmodel:

  Latent model for time random effects. The default is a second-order
  random walk model.

- INLAoutput:

  return the INLA output. Default is FALSE.

- INLAoutputOnly:

  return only the INLA output. Default is FALSE.

- WAIC:

  return the WAIC. Default is FALSE.

- DIC:

  return the DIC. Default is FALSE.

## Value

A list with element 'sample', the posterior predictive trajectories by
age class, and optionally the 'INLA' output.
