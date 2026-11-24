# Fit the non-structured nowcasting model with 'INLA'

Fits the Bayesian negative binomial model with 'INLA' on non-structured
data and samples the posterior predictive trajectories. Data has to be
in the delay-triangle format. Usually called internally by
[`nowcasting_inla()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_inla.md).

## Uso

``` r
nowcasting_no_age_inla(
  dataset,
  zero_inflated = FALSE,
  timeREmodel = "rw2",
  INLAoutput = FALSE,
  INLAoutputOnly = FALSE,
  WAIC = FALSE,
  DIC = FALSE
)
```

## Argumentos

- dataset:

  data pre-formatted into delays by week for each case, delay-triangle
  format

- zero_inflated:

  zero-inflated model. Default is FALSE.

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

## Valor

A list with element 'sample', the posterior predictive trajectories, and
optionally the 'INLA' output.
