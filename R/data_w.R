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
#' [Default] K is 0, no forecasting ahead
#'
#' @return Data in weeks format, with the maximum dates for the last week used
#' @export
#'
#' @examples If the last data is at a Sunday, so the weel starts at Monday before.
#' If ends at Thursday, so it starts on the Friday before
data.w <- function(dataset,
                   trim.data,
                   bins_age = c("SI-PNI", "10 years", "5 years", bins_age),
                   date_onset,
                   date_report,
                   age_col,
                   use.epiweek = TRUE,
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
  aux.trimming.date = ifelse( use.epiweek, 6-DT_max_diadasemana, 0)

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

  ## Accounting for the maximum of days on the last week to be used
  data_w <- dataset |>
    dplyr::rename(date_report = {{date_report}},
                  date_onset = {{date_onset}},
                  age_col = {{age_col}}) |>
    dplyr::filter(date_report <= DT_max - aux.trimming.date,
                  age_col <= max(bins_age)) |>
    tidyr::drop_na(age_col) |>
    dplyr::mutate(
      # Alterando a data para o primeiro dia da semana
      # Ex. se ultimo dado for de um domingo, entao a semana
      # comeca na 2a anterior, se termina 5a, entao começará 6a
      date_onset = date_onset -
        as.integer(format(date_onset, "%w")) -
        (6-DT_max_diadasemana),
      date_report = date_report -
        as.integer(format(date_report, "%w")) -
        (6-DT_max_diadasemana),
      Delay = as.numeric(date_report - date_onset) / 7,
      fx_etaria = cut(age_col,
                      breaks = bins_age,
                      labels = labels_age,
                      right = F)
    ) |>
    tidyr::drop_na(fx_etaria) |>
    dplyr::filter(Delay >= 0)

  # Returning
  return(data_w)
}
