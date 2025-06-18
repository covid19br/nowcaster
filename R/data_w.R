#' @title data.w
#'
#' @description Function to put the data into the format proper to the nowcasting estimation,
#' has to use the maximum of data present on the last week parsed to the estimation.
#' Makes the account of maximum days per week to be used.
#'
#' @param dataset dataset to be formatted as data by week
#' @param trim.data How much to trim of the data?
#' @param bins_age Bins of age to cu the data, parsing from nowcasting_inla
#' @param date_onset Column of dates of onset of the events, normally date of onset of first symptoms of cases
#' @param date_report Column of dates of report of the event, normally date of digitation of the notification of cases
#' @param age_col Age column to be where to  cut the data into age classes
#' @param use.epiweek If TRUE, it uses the CDC epiweek definition where the week starts on Sunday, if FALSE it the week ends at the weekday of the last record date.
#' @param K How much weeks to forecast ahead?
#' The default for K is 0, no forecasting ahead
#' @param silent Deprecated. To be removed.
#'
#' @return Data in weeks format, with the maximum dates for the last week used
#' @export
#'
data.w <- function(dataset,
                   trim.data,
                   bins_age = c("SI-PNI", "10 years", "5 years", bins_age),
                   date_onset,
                   date_report,
                   age_col,
                   use.epiweek = FALSE,
                   K = 0,
                   silent = F){
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

  ## Ignore data after the last Sunday of recording (Sunday as too)
  aux.trimming.date = ifelse( use.epiweek | DT_max_diadasemana == 6, DT_max_diadasemana + 1, 0)


  # ## Test for age bins
  if(!is.numeric(bins_age)){
    if(missing(bins_age) | is.null(bins_age)){
      bins_age<-"SI-PNI"
      warning("Using SI-PNI age bins")
    } else if(bins_age == "SI-PNI"){
      bins_age<-c(0,5,12,18,30,seq(40,90,by=10),130)
      labels_age<-1:(length(bins_age)-1)
      warning("Bins age as in SI-PNI: ",
              stringr::str_c(bins_age[bins_age < 90], " "), "90+",
              call. = T)
    } else if(bins_age == "10 years"){
      bins_age<-c(seq(0,90, by = 10),130)
      labels_age<-1:(length(bins_age)-1)
      warning("Bins age in 10 years: ",
              stringr::str_c(bins_age[bins_age < 90], " "), "90+",
              call. = T)
    } else if(bins_age == "5 years"){
      bins_age<-c(seq(0,90, by = 5),130)
      labels_age<-1:(length(bins_age)-1)
      warning("Bins age in 5 years: ",
              stringr::str_c(bins_age[bins_age < 90], " "), "90+",
              call. = T)
    }
    else {
      stop("Age bins are not options of 'SI-PNI', '10 years', '5 years' or numeric vector!")
    }
  } else {
    bins_age<-bins_age
    labels_age<-1:(length(bins_age)-1)
    warning("Using bins ages given: ",
            stringr::str_c(bins_age[bins_age < bins_age[length(bins_age) - 1]], " "),
            bins_age[length(bins_age) - 1], "+",
            call. = T)
  }

  # Workaround check
  DT.sun.aux <- dt.aux <- fx_etaria <- Delay <- NULL

  ## Accounting for the maximum of days on the last week to be used
  data_w <- dataset |>
    dplyr::rename(date_report = {{date_report}},
                  date_onset = {{date_onset}},
                  age_col = {{age_col}}) |>
    dplyr::filter(date_report <= DT_max - aux.trimming.date,
                  age_col <= max(bins_age)) |>
    tidyr::drop_na(age_col) |>
    dplyr::mutate(
      ## Onset date
      # Moving the date to sunday
      DT.sun.aux = as.integer(format(date_onset, "%w")),
      ## Altering the date for the first day of the week
      dt.aux = date_onset -
        # Last recording date (DT_max_diadasemana) is the last day of the new week format
        DT.sun.aux +
        ifelse( use.epiweek, 0, DT_max_diadasemana+1 -
                  ifelse(DT_max_diadasemana+1>DT.sun.aux,7, 0)
        ),
      date_onset = dt.aux - ifelse( date_onset < dt.aux, 7, 0),
      # Recording date
      DT.sun.aux = as.integer(format(date_report, "%w")),
      ## Altering the date for the first day of the week
      dt.aux = date_report -
        # Last recording date (DT_max_diadasemana) is the last day of the new week format
        DT.sun.aux +
        ifelse( use.epiweek, 0, DT_max_diadasemana+1 -
                  ifelse(DT_max_diadasemana+1 > DT.sun.aux, 7, 0)
        ),
      date_report = dt.aux - ifelse( date_report < dt.aux, 7, 0),
      Delay = as.numeric(date_report - date_onset) / 7,
      fx_etaria = cut(age_col,
                      breaks = bins_age,
                      labels = labels_age,
                      right = F)
    ) |>
    dplyr::select(-dt.aux, -DT.sun.aux) |>
    tidyr::drop_na(fx_etaria) |>
    dplyr::filter(Delay >= 0)

  # Returning
  return(data_w)
}
