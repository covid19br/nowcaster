#' @title nowcasting_inla
#'
#' @description function to estimate amount of events already started by not yet notified.
#' The main use is to estimate how many cases in a outbreak has already started their onset date of symptons
#' but has not yet notified.
#' nowcasting_inla, fits a statistical distribution to the empirical distribution of time of delay between
#' onset date and report date.
#'
#' @param dataset Dataset with at least 2 columns, date of onset, date of report.
#' It can be a dataset with 3 columns, two dates columns as before said and a another one being an stratum column,
#' in which data will stratified, usually being age.
#' @param trim.data (in weeks) Date to be trimmed out from the data base, in days.
#' [Default] 0 days.
#' @param Dmax (in weeks) Window of dates the estimation will act, i.e., till how many past weeks the nowcasting will estimate.
#' [Default] 15 weeks.
#' @param wdw (in weeks) Until which maximum amount of weeks the Nowcasting will use to the estimation.
#' [Default] 30 weeks.
#' @param use.epiweek If TRUE, it uses the CDC epiweek definition where the week starts on Sunday, if FALSE it the week ends at the weekday of the last record date.
#' [Default] FALSE
#' @param data.by.week If it has to be returned the whole time-series data.
#' [Default] FALSE.
#' @param return.age [Deprecated] If the estimate by Age should be returned.
#' [Default] TRUE.
#' @param bins_age Age bins to do the nowcasting, it receive a vector of age bins,
#' or options between, "SI-PNI", "10 years", "5 years".
#' [Default] "SI-PNI".
#' @param silent [Deprecated] Should be the warnings turned off?
#' [Default] is TRUE.
#' @param K (in weeks) How much weeks to forecast ahead?
#' [Default] K is 0, no forecasting ahead
#' @param age_col Column for ages
#' @param date_onset Column of dates of onset of the events, normally date of onset of first symptoms of cases
#' @param date_report Column of dates of report of the event, normally date of digitation of the notification of cases
#' @param trajectories Returns the trajectories estimated from the inner 'INLA' model
#' [Default] FALSE.
#' @param zero_inflated [Experimental] In non-structured models, fit a model that deals with zero-inflated data.
#' [Default] FALSE. If the [age_col] is not missing this flag is ignored.
#' @param ... list parameters to other functions
#'
#' @return a list of 2 elements, each element with a data.frame with nowcasting estimation, 'Total',
#' 'data' with the time-series out of wdw .
#' If 'age_col' is parsed, add a thrid element with by age estimation 'age' .
#' If 'trajectories' = TRUE, add a forth element with the returned trajectories from 'inla'.
#' @export
#'
#' @examples
#' # Loading Belo Horizonte SARI dataset
#' data(sragBH)
#' nowcasting_inla(dataset = sragBH,
#'                 date_onset = DT_SIN_PRI,
#'                 date_report = DT_DIGITA,
#'                 silent = T)
nowcasting_inla <- function(dataset,
                            bins_age="SI-PNI",
                            trim.data=0,
                            Dmax = 15,
                            wdw = 30,
                            use.epiweek = FALSE,
                            age_col,
                            date_onset,
                            date_report,
                            data.by.week = FALSE,
                            # return.age = NULL,
                            silent = F,
                            K = 0,
                            trajectories = F,
                            zero_inflated = F,
                            ...){

  dots<-list(...)

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

  ## Warnings
  if(missing(silent) | silent == FALSE){

    ## K parameter and trim.data warnings
    if (K > 0 & trim.data != 0){
      warning(paste0("Using K = ", K, " and trim.data = ", trim.data, ", is that right?"))
    }else{
      if(K > 0){
        message(paste0("Forecasting with K ", K, " ahead"))
      }else{
        message("Nowcasting only")
      }
    }
    ## Missing age column warning
    if(missing(bins_age)){
      bins_age <- "SI-PNI"
      warning("Using 'SI-PNI' age bins!")
    }else{
      bins_age<-bins_age
      message("Using age bins inputed")
    }
    ## Missing trim.data warning
    if(missing(trim.data)){
      trim.data <- 0
      warning("Using default to trim dates, 'trim.data = 0'")
    }else{
      trim.data<-trim.data
      message("Using trim.data inputed")
    }
    ## Missing Dmax warning
    if(missing(Dmax)){
      Dmax <- 15
      warning("Using default to maximum delay, 'Dmax = 15'")
    }else{
      Dmax<-Dmax
      message("Using Dmax inputed")
    }
    ## Missing wdw warning
    if(missing(wdw)){
      wdw <- 30
      warning("Using default to window of action, 'wdw = 30'")
    }else{
      wdw<-wdw
      message("Using wdw inputed")
    }
    ## Missing data.by.week warning
    if(missing(data.by.week)){
      data.by.week <- FALSE
      warning("Using default to returning option for the data, 'data.by.week = FALSE'")
    }else{
      data.by.week<-data.by.week
      message("Returning 'data.by.week'")
    }
    # ## Missing return.age warning
    # if(missing(return.age)){
    #   return.age <- TRUE
    #   warning("Using default to returning estimate by age, return.age = TRUE")
    # }
    ## Missing age_col warning
    if(missing(age_col)){
      warning("'age_col' missing, nowcasting with unstructured model")
    }else{
      message("'age_col' inputed, nowcasting with structured model")
    }

    if(missing(trajectories) | trajectories == FALSE){
      warning("Not returning trajectories")
    }else{
      trajectories = TRUE
      message("Trajectories returned")

      if(!missing(age_col) & !missing(zero_inflated)){
        zero_inflated<-FALSE
        warning("'age_col' parsed, 'zero_inflated' ignored!")
      }
    }
  }
  # else{
  #     bins_age<-bins_age;
  #     trim.data<-trim.data;
  #     Dmax<-Dmax;
  #     wdw<-wdw;
  #     data.by.week<-data.by.week;
  #     zero_inflated<-zero_inflated
  #   }



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
    data_w<-data.w_no_age(dataset = data.clean,
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



  # ## Maximum recording date
  # max.date_report = data.clean %>%
  #   dplyr::pull(var = {{date_report}}) %>%
  #   max(na.rm = T)



  ## Auxiliary date table
  if(K==0){
    dates <- range(data.inla |>
                    dplyr::pull(var = date_onset - 7*trim.data))
  } else {
    ## This is done to explicitly say for the forecast part that its date of onset is the present date
    date_k <- max(data.inla$date_onset) + 7*K - 7*trim.data
    dates <- range(data.inla$date_onset, date_k)
  }

  ## To make an auxiliary date table with each date plus an amount of dates  to forecast
  tbl.date.aux <- tibble::tibble(
    date_onset = seq(dates[1], dates[2], by = 7)
  )  |>
    tibble::rowid_to_column(var = "Time")

  ## Joining auxiliary date tables
  data.inla <- data.inla  |>
    dplyr::left_join(tbl.date.aux)

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
      dplyr::full_join(tbl.NA) |>  #View()
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
      dplyr::full_join(tbl.NA)  |>   #View()
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

    if(zero_inflated){
      ## Nowcasting estimate
      sample.now <- nowcasting_no_age(dataset = data.inla,
                                      zero_inflated = T)
    }else{
      ## Nowcasting estimate
      sample.now <- nowcasting_no_age(dataset = data.inla,
                                      zero_inflated = F)
    }

    ## Summary on the posteriors of nowcasting
    now_summary<-nowcasting.summary(trajetory = sample.now,
                                    age = F)
    l<-1
  } else {

    if(zero_inflated){
      ## Nowcasting estimate
      sample.now <- nowcasting_age(dataset = data.inla,
                                   zero_inflated = T)
    }else{
      ## Nowcasting estimate
      sample.now <- nowcasting_age(dataset = data.inla,
                                   zero_inflated = F)
    }

    ## Summary on the posteriors of nowcasting
    now_summary<-nowcasting.summary(trajetory = sample.now,
                                    age = T)
    l<-0
  }

  ## Objects to be returned

  if(data.by.week){

    # if(missing(age_col)){
      now_summary[[3-l]]<- data.inla
    # }

    # now_summary[[3-l]]<-data_w |>
    #   dplyr::group_by(date_onset) |>
    #   dplyr::summarise(observed = dplyr::n(),
    #                    Delay = Delay)

    names(now_summary)[3-l]<-"data"

    if(trajectories){
      now_summary[[4-l]]<-sample.now
      names(now_summary)[4-l]<-"trajectories"
    }
  } else {
    if(trajectories){
      now_summary[[3-l]]<-sample.now
      names(now_summary)[3-l]<-"trajectories"
    }
  }

  ## Final object returned
  return(now_summary)

}

