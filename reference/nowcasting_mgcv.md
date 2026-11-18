# nowcasting_mgcv

function to estimate amount of events already started by not yet
notified. The main use is to estimate how many cases in a outbreak has
already started their onset date of symptons but has not yet notified.
nowcasting_mgcv, fits a statistical distribution to the empirical
distribution of time of delay between onset date and report date.

## Usage

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

## Arguments

- dataset:

  Dataset with at least 2 columns, date of onset, date of report. It can
  be a dataset with 3 columns, two dates columns as before said and a
  another one being an stratum column, in which data will stratified,
  usually being age.

- bins_age:

  Age bins to do the nowcasting, it receive a vector of age bins, or
  options between, "SI-PNI", "10 years", "5 years". The default is
  "SI-PNI".

- trim.data:

  (in weeks) Date to be trimmed out from the data base, in days. Default
  is 0 days.

- Dmax:

  (in weeks) Window of dates the estimation will act, i.e., till how
  many past weeks the nowcasting will estimate. Default is 15 weeks.

- wdw:

  (in weeks) Until which maximum amount of weeks the Nowcasting will use
  to the estimation. Default is 30 weeks.

- use.epiweek:

  If TRUE, it uses the CDC epiweek definition where the week starts on
  Sunday, if FALSE it the week ends at the weekday of the last record
  date. Default is TRUE

- age_col:

  Column for ages

- date_onset:

  Column of dates of onset of the events, normally date of onset of
  first symptoms of cases

- date_report:

  Column of dates of report of the event, normally date of digitation of
  the notification of cases

- data.by.week:

  If it has to be returned the whole time-series data. Default is FALSE.

- return.age:

  Deprecated. If the estimate by Age should be returned. Default is
  TRUE.

- silent:

  Deprecated. Should be the warnings turned off? . The default is FALSE

- method.group.gam:

  Method for aggregated smooth function in gam. Default is "fs" factor
  smooth, another alternative is "by".

- K:

  (in weeks) How much weeks to forecast ahead? . The default is K = 0,
  no forecasting ahead

- trajectories:

  Returns the predictive trajectories. The default is FALSE.

- ...:

  list parameters to other functions

## Value

a list of 2 elements, each element with a data.frame with nowcasting
estimation, 'Total', 'data' with the time-series out of wdw . If
'age_col' is parsed, add a third element with by age estimation 'age' .
If 'trajectories' = TRUE, add a forth element with the returned
trajectories from 'inla'.

## Examples

``` r
# \donttest{
# Loading Belo Horizonte SARI dataset
data(sragBH)
output <- nowcasting_mgcv(dataset = sragBH,
                date_onset = DT_SIN_PRI,
                date_report = DT_DIGITA)
#> Nowcasting only
#> Warning: Using default to maximum delay, 'Dmax = 15'
#> Warning: Using default to window of action, 'wdw = 30'
#> Warning: Using default to returning option for the data, 'data.by.week = FALSE'
#> Warning: Not returning trajectories
#> Warning: 'age_col' missing, nowcasting with unstructured model
#> Warning: Using default, trimming out 0 days of data
#> Joining with `by = join_by(date_onset)`
#> Joining with `by = join_by(date_onset, delay, Time)`
# }
```
