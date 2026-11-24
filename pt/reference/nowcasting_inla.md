# Nowcasting with a Bayesian negative binomial model fitted by 'INLA'

Estimates the number of events that have already started but are not yet
notified. The main use is to estimate how many cases in an outbreak have
already started their symptoms but has not yet notified.
nowcasting_inla, fits a statistical distribution to the empirical
distribution of time of delay between onset date and report date.

## Uso

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

- K:

  (in weeks) How many weeks to forecast ahead. The default is K = 0, no
  forecasting ahead.

- trajectories:

  Returns the posterior predictive trajectories estimated from the inner
  'INLA' model. The default is FALSE.

- zero_inflated:

  Experimental! In non-structured models, fit a model that deals with
  zero-inflated data. The default is FALSE. If the age_col is not
  missing this flag is ignored.

- timeREmodel:

  Latent model for time random effects. The default is a second-order
  random walk model, "rw2".

- INLAoutput:

  return the INLA output. Default is FALSE.

- INLAoutputOnly:

  return only the INLA output. Default is FALSE.

- WAIC:

  return the WAIC. The default is FALSE.

- DIC:

  return the DIC. The default is FALSE.

- ...:

  list parameters to other functions

## Valor

A list. Element 'total' is a data.frame with the nowcasting estimate:
posterior 'Median', 95% credible interval ('LI', 'LS') and 50% credible
interval ('LIb', 'LSb') for each onset week. If 'data.by.week' = TRUE,
element 'data' holds the delay-triangle data used to fit the model. If
'age_col' is given, element 'age' holds the estimate by age class. If
'trajectories' = TRUE, element 'trajectories' holds the posterior
predictive trajectories from 'inla'. If 'INLAoutput' = TRUE, the INLA
output is returned as a list object named 'output'. If 'INLAoutputOnly'
= TRUE, just the INLA output is returned in a list object named
'output'. If 'WAIC' = TRUE or 'DIC' = TRUE, then 'INLAoutput' is forced
to be TRUE returning the INLA output and a list object named waic or dic
are also returned.

## Veja também

[`nowcasting_mgcv()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_mgcv.md)
for the same model fitted as a GAM with 'mgcv', which does not need
'INLA'.

## Exemplos

``` r
if (FALSE) { # \dontrun{
# Needs the 'INLA' package, installed from https://www.r-inla.org/download-install
# Loading Belo Horizonte SARI dataset
data(sragBH)
output <- nowcasting_inla(dataset = sragBH,
                          date_onset = DT_SIN_PRI,
                          date_report = DT_DIGITA)
head(output$total)
} # }
```
