# Slope.estimate.quant

Fits a linear model to trajectories returned from 'nowcasting_inla()'
within a given window. The default is 3 weeks. If 'end.week' is missing
uses the maximum date in 'trajectories'.

## Usage

``` r
slope.estimate.quant(end.week, trajectories, window = 3)
```

## Arguments

- end.week:

  (in weeks) The end of the week wanted to the slope estimate. Default:
  max. date in 'trajectories'.

- trajectories:

  Data.frame with the predicted or nowcasted estimate

- window:

  (in weeks) Window of how much time will be used to calculate the slope
  estimate. The default is 3 weeks.

## Value

The numerical value of the slope of the estimate

## Examples

``` r
# \donttest{
# Loading Belo Horizonte SARI dataset
data(sragBH)
now <- nowcasting_mgcv(dataset = sragBH,
                date_onset = DT_SIN_PRI,
                date_report = DT_DIGITA,
                trajectories = TRUE,
                silent = TRUE)
#> Joining with `by = join_by(date_onset)`
#> Joining with `by = join_by(date_onset, delay, Time)`
slope.estimate.quant(trajectories = now$trajectories)
#> Warning: Using max. date in 'trajectories' as 'end.week'
#> [1] -0.5
# }
```
