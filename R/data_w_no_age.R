#' @title data.w_no_age
#'
#' @description Function to put the data into the format proper to the nowcasting estimation,
#' has to use the maximum of data present on the last week parsed to the estimation.
#' Makes the account of maximum days per week to be used. With no age structure.
#'
#' @param dataset dataset to be formatted as data by week
#' @param trim.data How much to trim of the data?
#' @param date_onset Column of dates of onset of the events, normally date of onset of first symptoms of cases
#' @param date_report Column of dates of report of the event, normally date of digitation of the notification of cases
#' @param use.epiweek If TRUE, it uses the CDC epiweek definition where the week starts on Sunday, if FALSE it the week ends at the weekday of the last record date.
#' @param K How much weeks to forecast ahead?
#' [Default] K is 0, no forecasting ahead
#'
#' @return Data in weeks format, with the maximum dates for the last week used
#' @export
#'
#' @examples If the last data is at a Sunday, so the week starts at Monday before.
#' If ends at Thursday, so it starts on the Friday before
data.w_no_age<-function(dataset,
                        trim.data,
                        date_onset,
                        date_report,
                        use.epiweek = FALSE,
                        K=0,
                        silent=F){
  if(!silent){
    ## Last digitation date considered
    if(missing(trim.data)){
      trim.data <-  0
      warning("Using default, no trimming out of the data")
    } else {
      warning("Using default, trimming out ",
              trim.data ,
              " days of data",
              call. = T)
    }
  }else{
    ## Trim.data
    trim.data <-  0
  }

  ## Transforming trim.data into weeks
  trim.data.w <- 7*trim.data

  ## K parameter of forecasting
  K.w<-7*K

  ## Maximum date to be considered on the estimation
  DT_max <- max(dataset |>
                  dplyr::pull(var = {{date_report}}),
                na.rm = T) - trim.data.w + K.w


  ## Last day of the week for the digitation date calculation
  DT_max_diadasemana <- as.integer(format(DT_max, "%w"))

  #  Notice that if max recording date is Saturday (DT_max_diadasemana = 6) then the week is complete,
  # and the epiweek is on course. Otherwise some data must be ignored
  aux.trimming.date = ifelse( use.epiweek, 6-DT_max_diadasemana, 0)

  ## Accounting for the maximum of days on the last week to be used
  data_w <- dataset |>
    dplyr::rename(date_report = {{date_report}},
                  date_onset = {{date_onset}}) |>
    dplyr::filter(date_report <= DT_max - aux.trimming.date) |>
    dplyr::mutate(
      ## Altering the date for the first day of the week
      date_onset = date_onset -
        as.integer(format(date_onset, "%w")) -
        (6-DT_max_diadasemana),
      date_report = date_report -
        as.integer(format(date_report, "%w")) -
        (6-DT_max_diadasemana),
      Delay = as.numeric(date_report - date_onset) / 7) |>
    dplyr::filter(Delay >= 0)

  # Returning the data
  return(data_w)
}



