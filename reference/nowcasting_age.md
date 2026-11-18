# nowcasting_age

Run INLA model on structured data by age-class data has to be in the
format of delay-triangle

## Usage

``` r
nowcasting_age(
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

  return the only the INLA output. Default is FALSE.

- WAIC:

  return the WAIC. Default is FALSE.

- DIC:

  return the DIC. Default is FALSE.

## Value

Trajectories from the inner 'INLA' model
