# nowcasting_no_age

Run INLA model on non-structured data, data has to be in the format of
delay-triangle

## Usage

``` r
nowcasting_no_age(
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

  zero-inflated model. Default is FALSE.

- timeREmodel:

  Latent model for time random effects. The default is a second-order
  random walk model.

- INLAoutput:

  return the INLA output. Default is FALSE.

- INLAoutputOnly:

  return the only the INLA output. Default is FALSE.

- WAIC:

  return the WAIC. Default is FALSE.

- DIC:

  return the DIC. Default is FALSE.

## Value

Trajectories from the inner 'INLA' model
