#' @title Nowcasting with a Bayesian negative binomial model fitted by 'INLA'
#'
#' @description Estimates the number of events that have already started but are not yet notified.
#' The main use is to estimate how many cases in an outbreak have already started their symptoms
#' but has not yet notified.
#' nowcasting_inla, fits a statistical distribution to the empirical distribution of time of delay between
#' onset date and report date.
#'
#' @param dataset A data.frame with at least 2 columns, date of onset and date of report.
#' It can have a third column, a stratum column by which the data will be stratified, usually age.
#' @param trim.data (in weeks) Number of most recent weeks to trim out of the data before estimation. Default is 0 weeks.
#' @param Dmax (in weeks) Maximum reporting delay considered by the model. Cases reported with a longer delay are discarded. Default is 15 weeks.
#' @param wdw (in weeks) Length of the time window, counted back from the last onset date, used to fit the model. Default is 30 weeks.
#' @param use.epiweek If TRUE, it uses the CDC epiweek definition where the week starts on Sunday, if FALSE, the week ends at the weekday of the last record date. Default is TRUE
#' @param data.by.week If TRUE, also returns the delay-triangle data used to fit the model (element 'data'). Default is FALSE.
#' @param return.age Deprecated. If the estimate by Age should be returned. Default is TRUE.
#' @param bins_age Age bins to do the nowcasting, it receive a vector of age bins,
#' or options between, "SI-PNI", "10 years", "5 years". The default is "SI-PNI".
#' @param silent If `TRUE`, the function prints no messages. By default (`FALSE`) it reports the age classes used and how many weeks were trimmed, when it trims data.
#' @param K (in weeks) How many weeks to forecast ahead. The default is K = 0, no forecasting ahead.
#' @param age_col Column with the age (in years) of each case. If given, the structured (age-stratified) model is fitted.
#' @param date_onset Column of dates of onset of the events, normally date of onset of first symptoms of cases.
#' @param date_report Column of dates of report of the events, normally the date the notification was recorded in the system.
#' @param trajectories Returns the posterior predictive trajectories estimated from the inner 'INLA' model. The default is FALSE.
#' @param zero_inflated Experimental! In non-structured models, fit a model that deals with zero-inflated data. The default is FALSE. If the age_col is not missing this flag is ignored.
#' @param timeREmodel Latent model for time random effects. The default is a second-order random walk model, "rw2".
#' @param INLAoutput return the INLA output. Default is FALSE.
#' @param INLAoutputOnly return only the INLA output. Default is FALSE.
#' @param WAIC return the WAIC. The default is FALSE.
#' @param DIC return the DIC. The default is FALSE.
#' @param ... list parameters to other functions
#'
#' @return A list. Element 'total' is a data.frame with the nowcasting estimate: posterior 'Median',
#' 95% credible interval ('LI', 'LS') and 50% credible interval ('LIb', 'LSb') for each onset week.
#' If 'data.by.week' = TRUE, element 'data' holds the delay-triangle data used to fit the model.
#' If 'age_col' is given, element 'age' holds the estimate by age class.
#' If 'trajectories' = TRUE, element 'trajectories' holds the posterior predictive trajectories from 'inla'.
#' If 'INLAoutput' = TRUE, the INLA output is returned as a list object named 'output'.
#' If 'INLAoutputOnly' = TRUE, just the INLA output is returned in a list object named 'output'.
#' If 'WAIC' = TRUE or 'DIC' = TRUE, then 'INLAoutput' is forced to be TRUE returning the INLA output and a list object named waic or dic are also returned.
#' @export
#'
#' @seealso [nowcasting_mgcv()] for the same model fitted as a GAM with 'mgcv', which does not need 'INLA'.
#'
#' @examples
#' \dontrun{
#' # Needs the 'INLA' package, installed from https://www.r-inla.org/download-install
#' # Loading Belo Horizonte SARI dataset
#' data(sragBH)
#' output <- nowcasting_inla(dataset = sragBH,
#'                           date_onset = DT_SIN_PRI,
#'                           date_report = DT_DIGITA)
#' head(output$total)
#' }

nowcasting_inla <- function(dataset,
                            bins_age="SI-PNI",
                            trim.data=0,
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
                            WAIC = FALSE, DIC = FALSE,
                            ...){

  dots<-list(...)

  # Workaround check
  Y <- Time <- delay <- dt.aux <- fx_etaria <- Delay <- NULL

  ## Safe tests
  if(missing(dataset)){
    stop("Dataset is missing!")
  }
  if(ncol(dataset) < 2){
    if(!missing(age_col) & ncol(dataset) < 3){
      stop("Missing 'age_col'! \n
           Dataset does not have 3 columns!")
    }else{
      stop("Dataset does not have 2 columns!")
    }

  }
  if(missing(date_onset) | missing(date_report)){
    stop("'date_onset' or 'date_report' missing! Please give a column name for each of this parameters")
  }
  if(K < 0 ){
    stop("K less than 0, we cannot produce backcasting! \n
         Please set the K to anything greater than 0 to Forecasting")
  }
  if (!isTRUE(silent) && !isFALSE(silent)) {
    stop("'silent' must be TRUE or FALSE.")
  }

  # Dealing with INLA output

  INLAoutput.aux = INLAoutput

  ## Forcing INLA output TRUE when INLAoutput is FALSE
  if(INLAoutputOnly == TRUE & INLAoutput == FALSE) INLAoutput.aux = TRUE

  ## Forcing INLA output TRUE when either WAIC or DIC are TRUE
  if((WAIC == TRUE | DIC == TRUE) & INLAoutput.aux == FALSE) INLAoutput.aux = TRUE


  ## Objects for keep the nowcasting
  ## Filtering out cases without report date
  if(missing(age_col)){
    data.clean <- dataset |>
      dplyr::select({{date_report}}, {{date_onset}})  |>
      tidyr::drop_na({{date_report}})
  } else {
    data.clean <- dataset  |>
      dplyr::select({{date_report}}, {{date_onset}}, {{age_col}})  |>
      tidyr::drop_na({{date_report}})
  }

  ## Filtering data to the parameters setted above
  if(missing(age_col)){
    data_w <- data.w_no_age(dataset = data.clean,
                            trim.data = trim.data,
                            date_onset = {{date_onset}},
                            date_report = {{date_report}},
                            use.epiweek = use.epiweek,
                            K = K,
                            silent = silent)
  }else {
    data_w <- data.w(dataset = data.clean,
                     bins_age = bins_age,
                     trim.data = trim.data,
                     age_col = {{age_col}},
                     date_onset = {{date_onset}},
                     date_report = {{date_report}},
                     use.epiweek = use.epiweek,
                     K = K,
                     silent = silent)
  }

  ## Parameters of Nowcasting estimate
  Tmax <- max(data_w |>
                dplyr::pull(var = date_onset))


  ## Data to be entered in Nowcasting function
  ##
  if(missing(age_col)){
    data.inla <- data_w  |>
      ## Filter for dates
      dplyr::filter(date_onset >= Tmax - 7 * wdw,
                    Delay <= Dmax)  |>
      ## Group by on Onset dates, Amounts of delays and Stratum
      dplyr::group_by(date_onset, delay = Delay)  |>
      ## Counting
      dplyr::tally(name = "Y")  |>
      dplyr::ungroup()
  } else {
    data.inla <- data_w  |>
      ## Filter for dates
      dplyr::filter(date_onset >= Tmax - 7 * wdw,
                    Delay <= Dmax)  |>
      ## Group by on Onset dates, Amounts of delays and Stratum
      dplyr::group_by(date_onset, delay = Delay, fx_etaria)  |>
      ## Counting
      dplyr::tally(name = "Y")  |>
      dplyr::ungroup()
  }


  ## Auxiliary date table

  ## Range of onset weeks in the model data. The data were already trimmed by
  ## 'trim.data' weeks in data.w() / data.w_no_age().
  dates <- range(data.inla$date_onset)

  # Adding forecast
  dates[2] <- dates[2] + 7*K


  ## To make an auxiliary date table with each date plus an amount of dates  to forecast
  tbl.date.aux <- tibble::tibble(
    date_onset = seq(dates[1], dates[2], by = 7)
  )  |>
    tibble::rowid_to_column(var = "Time")

  ## Joining auxiliary date tables
  data.inla <- data.inla  |>
    dplyr::left_join(tbl.date.aux, by = "date_onset")

  ## Time maximum to be considered
  Tmax.id <- max(data.inla$Time)

  # Auxiliary date table on each stratum, By age
  if(missing(age_col)){
    tbl.NA <-
      expand.grid(Time = 1:(Tmax.id+K),
                  delay = 0:Dmax)  |>
      dplyr::left_join(tbl.date.aux, by = "Time")
  } else{
    tbl.NA <-
      expand.grid(Time = 1:(Tmax.id+K),
                  delay = 0:Dmax,
                  fx_etaria = unique(data.inla$fx_etaria)
      ) |>
      dplyr::left_join(tbl.date.aux, by = "Time")
  }

  ## Joining the auxiliary date table by Stratum
  if(missing(age_col)){
    data.inla <- data.inla  |>
      dplyr::full_join(tbl.NA, by = c("date_onset", "delay", "Time")) |>
      dplyr::mutate(
        Y = ifelse(Time + delay > Tmax.id, as.numeric(NA), Y),
        ## If Time + Delay is greater than Tmax, fill with NA
        Y = ifelse(is.na(Y) & Time + delay <= Tmax.id, 0, Y ),
        ## If Time + Delay is smaller than Tmax AND Y is NA, fill 0
      )  |>
      dplyr::arrange(Time, delay) |>
      dplyr::rename(dt_event = date_onset) |>
      tidyr::drop_na(delay)
  }else {
    data.inla <- data.inla  |>
      dplyr::full_join(tbl.NA, by = c("date_onset", "delay", "fx_etaria", "Time")) |>
      dplyr::mutate(
        Y = ifelse(Time + delay > Tmax.id, as.numeric(NA), Y),
        ## If Time + Delay is greater than Tmax, fill with NA
        Y = ifelse(is.na(Y) & Time + delay <= Tmax.id, 0, Y ),
        ## If Time + Delay is smaller than Tmax AND Y is NA, fill 0
      )  |>
      dplyr::arrange(Time, delay, fx_etaria) |>
      dplyr::rename(dt_event = date_onset) |>
      tidyr::drop_na(delay)
  }
  ## Precisamos transformar essa datas de volta no valor que é correspondente delas,
  ## a ultima data de primeiro sintomas foi jogada pra até uma semana atrás


  if(missing(age_col)){
    # Nowcasting no age

    if(zero_inflated){
      ## Nowcasting estimate
      sample.now <- nowcasting_no_age_inla(dataset = data.inla,
                                      zero_inflated = TRUE,
                                      timeREmodel = timeREmodel,
                                      INLAoutput = INLAoutput.aux,
                                      INLAoutputOnly = INLAoutputOnly,
                                      WAIC = WAIC, DIC = DIC)
    }else{
      ## Nowcasting estimate
      sample.now <- nowcasting_no_age_inla(dataset = data.inla,
                                      zero_inflated = FALSE,
                                      timeREmodel = timeREmodel,
                                      INLAoutput = INLAoutput.aux,
                                      INLAoutputOnly = INLAoutputOnly,
                                      WAIC = WAIC, DIC = DIC)
    }

    ## Summary on the posteriors of nowcasting
    if(!INLAoutputOnly){
      now_summary<-nowcasting.summary(trajectory = sample.now$sample,
                                      age = FALSE)
    }else{
      now_summary <- list()
    }

  } else {
    # Nowcasting by age groups

    if(zero_inflated){
      ## Zero inflated by age
      sample.now <- nowcasting_age_inla(dataset = data.inla,
                                   zero_inflated = TRUE,
                                   timeREmodel = timeREmodel,
                                   INLAoutput = INLAoutput.aux,
                                   INLAoutputOnly = INLAoutputOnly,
                                   WAIC = WAIC, DIC = DIC)
    }else{
      ## Negative binomial by age
      sample.now <- nowcasting_age_inla(dataset = data.inla,
                                   zero_inflated = FALSE,
                                   timeREmodel = timeREmodel,
                                   INLAoutput = INLAoutput.aux,
                                   INLAoutputOnly = INLAoutputOnly,
                                   WAIC = WAIC, DIC = DIC)
    }

    ## Summary on the posteriors of nowcasting
    if(!INLAoutputOnly){
      now_summary<-nowcasting.summary(trajectory = sample.now$sample,
                                    age = TRUE)
    } else {
      now_summary <- list()
    }
  }

  ## Objects to be returned

  if(!INLAoutputOnly){
    if(data.by.week){
      now_summary$data <- data.inla

      if(trajectories){
        now_summary$trajectories <- sample.now$sample
      }
    } else {
      if(trajectories){
        now_summary$trajectories <- sample.now$sample
      }
    }

  }

  if(INLAoutput.aux) now_summary$output <- sample.now$INLAoutput
  if(WAIC) now_summary$waic <- sample.now$INLAoutput$waic$waic
  if(DIC) now_summary$dic <- sample.now$INLAoutput$dic$dic


  ## Final object returned
  return(now_summary)

}

