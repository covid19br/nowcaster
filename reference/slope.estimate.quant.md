# Estimate the slope of the nowcasted curve

Fits a linear model to each trajectory returned by
[`nowcasting_inla()`](https://covid19br.github.io/nowcaster/reference/nowcasting_inla.md)
or
[`nowcasting_mgcv()`](https://covid19br.github.io/nowcaster/reference/nowcasting_mgcv.md)
(with `trajectories = TRUE`) within a given window. Requires the 'lme4'
package. The default is 3 weeks. If 'end.week' is missing uses the
maximum date in 'trajectories'.

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

A trend category based on the distribution of the slopes of the
trajectories: `1` increasing (5% quantile \> 0), `0.5` probably
increasing (25% quantile \>= 0), `0` stable or uncertain, `-0.5`
probably decreasing (75% quantile \<= 0) and `-1` decreasing (95%
quantile \< 0).

## Examples

``` r
# \donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  # Loading Belo Horizonte SARI dataset
  data(sragBH)
  now <- nowcasting_mgcv(dataset = sragBH,
                         date_onset = DT_SIN_PRI,
                         date_report = DT_DIGITA,
                         trajectories = TRUE)
  slope.estimate.quant(trajectories = now$trajectories)
}
#> Warning: Using default, trimming out 0 days of data
#> Joining with `by = join_by(date_onset)`
#> Joining with `by = join_by(date_onset, delay, Time)`
#> Warning: Using max. date in 'trajectories' as 'end.week'
#> [1] -0.5
# }
```
