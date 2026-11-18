# nowcasting_no_age_mgcv

Run INLA model on non-structured data, data has to be in the format of
delay-triangle

## Usage

``` r
nowcasting_no_age_mgcv(dataset)
```

## Arguments

- dataset:

  data pre formatted in to age classes and delays by week for each
  cases, delay triangle format

## Value

Trajectories from the Negative Binomial GAM
