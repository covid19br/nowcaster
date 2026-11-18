# nowcasting_age_mgcv

Run INLA model on structured data by age-class data has to be in the
format of delay-triangle

## Usage

``` r
nowcasting_age_mgcv(dataset, method = "fs")
```

## Arguments

- dataset:

  data pre formatted in to age classes and delays by week for each
  cases, delay triangle format

- method:

  method used grouped gam, "by" where the smooth function will be s(.,
  by= agegroup) or "fs" (factor smooth) where the smooth function uses
  s(., age_group, ,bs="fs")

## Value

Trajectories from the inner 'INLA' model
