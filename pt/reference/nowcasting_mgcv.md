# Nowcasting with a negative binomial GAM fitted by 'mgcv'

Estimates the number of events that have already started but are not yet
notified. The main use is to estimate how many cases in an outbreak have
already started their symptoms but has not yet notified.
nowcasting_mgcv, fits a statistical distribution to the empirical
distribution of time of delay between onset date and report date.

## Uso

``` r
nowcasting_mgcv(
  dataset,
  bins_age = "SI-PNI",
  trim.data = 0,
  Dmax = 15,
  wdw = 30,
  use.epiweek = TRUE,
  age_col,
  date_onset,
  date_report,
  data.by.week = FALSE,
  return.age = NULL,
  silent = FALSE,
  method.group.gam = "fs",
  K = 0,
  trajectories = FALSE,
  ...
)
```

## Argumentos

- dataset:

  A data.frame with at least 2 columns, date of onset and date of
  report. It can have a third column, a stratum column by which the data
  will be stratified, usually age.

- bins_age:

  Age bins to do the nowcasting, it receive a vector of age bins, or
  options between, "SI-PNI", "10 years", "5 years". The default is
  "SI-PNI".

- trim.data:

  (in weeks) Number of most recent weeks to trim out of the data before
  estimation. Default is 0 weeks.

- Dmax:

  (in weeks) Maximum reporting delay considered by the model. Cases
  reported with a longer delay are discarded. Default is 15 weeks.

- wdw:

  (in weeks) Length of the time window, counted back from the last onset
  date, used to fit the model. Default is 30 weeks.

- use.epiweek:

  If TRUE, it uses the CDC epiweek definition where the week starts on
  Sunday, if FALSE, the week ends at the weekday of the last record
  date. Default is TRUE

- age_col:

  Column with the age (in years) of each case. If given, the structured
  (age-stratified) model is fitted.

- date_onset:

  Column of dates of onset of the events, normally date of onset of
  first symptoms of cases.

- date_report:

  Column of dates of report of the events, normally the date the
  notification was recorded in the system.

- data.by.week:

  If TRUE, also returns the delay-triangle data used to fit the model
  (element 'data'). Default is FALSE.

- return.age:

  Deprecated. If the estimate by Age should be returned. Default is
  TRUE.

- silent:

  Deprecated. Should the warnings be turned off? The default is FALSE.

- method.group.gam:

  Type of smooth used by age class in the structured model: "fs" (factor
  smooth, default) or "by" (one smooth per age class).

- K:

  (in weeks) How many weeks to forecast ahead. The default is K = 0, no
  forecasting ahead.

- trajectories:

  Returns the predictive trajectories. The default is FALSE.

- ...:

  list parameters to other functions

## Valor

A list. Element 'total' is a data.frame with the nowcasting estimate:
predictive 'Median', 95% interval ('LI', 'LS') and 50% interval ('LIb',
'LSb') for each onset week. If 'data.by.week' = TRUE, element 'data'
holds the delay-triangle data used to fit the model. If 'age_col' is
given, element 'age' holds the estimate by age class. If 'trajectories'
= TRUE, element 'trajectories' holds the predictive trajectories sampled
from the GAM.

## Veja também

[`nowcasting_inla()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_inla.md)
for the Bayesian version of the model fitted with 'INLA'.

## Exemplos

``` r
# \donttest{
# Loading Belo Horizonte SARI dataset
data(sragBH)
output <- nowcasting_mgcv(dataset = sragBH,
                          date_onset = DT_SIN_PRI,
                          date_report = DT_DIGITA)
#> Warning: Using default, trimming out 0 days of data
#> Joining with `by = join_by(date_onset)`
#> Joining with `by = join_by(date_onset, delay, Time)`
head(output$total)
#> # A tibble: 6 × 7
#>    Time dt_event   Median    LI    LS   LIb   LSb
#>   <int> <date>      <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1    17 2021-12-19    625   621  633    623  627 
#> 2    18 2021-12-26    694   687  707    691  698 
#> 3    19 2022-01-02    811   800  828.   807  817 
#> 4    20 2022-01-09    886   871  910.   880  893.
#> 5    21 2022-01-16    817   799  845    810  826 
#> 6    22 2022-01-23    630   609  662    622  639 
# }
```
