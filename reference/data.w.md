# data.w

Function to put the data into the format proper to the nowcasting
estimation, has to use the maximum of data present on the last week
parsed to the estimation. Makes the account of maximum days per week to
be used.

## Usage

``` r
data.w(
  dataset,
  trim.data,
  bins_age = c("SI-PNI", "10 years", "5 years", bins_age),
  date_onset,
  date_report,
  age_col,
  use.epiweek = FALSE,
  K = 0,
  silent = FALSE
)
```

## Arguments

- dataset:

  dataset to be formatted as data by week

- trim.data:

  How much to trim of the data?

- bins_age:

  Bins of age to cu the data, parsing from nowcasting_inla

- date_onset:

  Column of dates of onset of the events, normally date of onset of
  first symptoms of cases

- date_report:

  Column of dates of report of the event, normally date of digitation of
  the notification of cases

- age_col:

  Age column to be where to cut the data into age classes

- use.epiweek:

  If TRUE, it uses the CDC epiweek definition where the week starts on
  Sunday, if FALSE it the week ends at the weekday of the last record
  date.

- K:

  How much weeks to forecast ahead? The default for K is 0, no
  forecasting ahead

- silent:

  Deprecated. To be removed.

## Value

Data in weeks format, with the maximum dates for the last week used
