# nowcasting_inla

function to estimate amount of events already started by not yet
notified. The main use is to estimate how many cases in a outbreak has
already started their onset date of symptons but has not yet notified.
nowcasting_inla, fits a statistical distribution to the empirical
distribution of time of delay between onset date and report date.

## Usage

``` r
nowcasting_inla(
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
  K = 0,
  trajectories = FALSE,
  zero_inflated = FALSE,
  timeREmodel = "rw2",
  INLAoutput = FALSE,
  INLAoutputOnly = FALSE,
  WAIC = FALSE,
  DIC = FALSE,
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

- K:

  (in weeks) How much weeks to forecast ahead? . The default is K = 0,
  no forecasting ahead

- trajectories:

  Returns the trajectories estimated from the inner 'INLA' model . The
  default is FALSE.

- zero_inflated:

  Experimental! In non-structured models, fit a model that deals with
  zero-inflated data. The default is FALSE. If the age_col is not
  missing this flag is ignored.

- timeREmodel:

  Latent model for time random effects. . The default is a second-order
  random walk model.

- INLAoutput:

  return the INLA output. Default is FALSE.

- INLAoutputOnly:

  return the only the INLA output. Default is FALSE.

- WAIC:

  return the WAIC. The default is FALSE.

- DIC:

  return the DIC.The default is FALSE.

- ...:

  list parameters to other functions

## Value

a list of 2 elements, each element with a data.frame with nowcasting
estimation, 'Total', 'data' with the time-series out of wdw . If
'age_col' is parsed, add a third element with by age estimation 'age' .
If 'trajectories' = TRUE, add a forth element with the returned
trajectories from 'inla'. If 'INLAoutput' = TRUE, the INLA output is
returned as a list object named 'output'. If 'INLAoutputOnly' = TRUE,
just the INLA output is returned in a list object named 'output'. If
'WAIC' = TRUE or 'DIC' = TRUE, then 'INLAoutput' is forced to be TRUE
returning the INLA output and a list object named waic or dic are also
returned.

## Examples

``` r
if (FALSE) { # \dontrun{
# Loading Belo Horizonte SARI dataset
data(sragBH)
output <- nowcasting_inla(dataset = sragBH,
                date_onset = DT_SIN_PRI,
                date_report = DT_DIGITA,
                silent = T)
} # }
```
