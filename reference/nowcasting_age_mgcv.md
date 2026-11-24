# Fit the structured (age) nowcasting GAM

Fits a negative binomial GAM with 'mgcv' on data structured by age class
and samples the predictive trajectories for the unreported cases. Data
has to be in the delay-triangle format. Usually called internally by
[`nowcasting_mgcv()`](https://covid19br.github.io/nowcaster/reference/nowcasting_mgcv.md).

## Usage

``` r
nowcasting_age_mgcv(dataset, method = "fs")
```

## Arguments

- dataset:

  data pre formatted in to age classes and delays by week for each
  cases, delay triangle format

- method:

  Type of smooth by age class: "fs" (factor smooth,
  `s(., age_class, bs = "fs")`, default) or "by"
  (`s(., by = age_class)`).

## Value

A list with element 'sample', the predictive trajectories by age class.
