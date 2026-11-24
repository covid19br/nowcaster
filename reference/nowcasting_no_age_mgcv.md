# Fit the non-structured nowcasting GAM

Fits a negative binomial GAM with 'mgcv' on non-structured data and
samples the predictive trajectories for the unreported cases. Data has
to be in the delay-triangle format. Usually called internally by
[`nowcasting_mgcv()`](https://covid19br.github.io/nowcaster/reference/nowcasting_mgcv.md).

## Usage

``` r
nowcasting_no_age_mgcv(dataset)
```

## Arguments

- dataset:

  data pre-formatted into delays by week for each case, delay-triangle
  format

## Value

A list with element 'sample', the predictive trajectories from the
negative binomial GAM.
