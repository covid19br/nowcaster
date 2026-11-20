#' @title nowcasting_diff_inla
#'
#' @description Function to estimate the number of events that have already occurred but have not yet been reported, 
#' in situations where the notification date is unavailable. The function calculates delayed notifications by comparing 
#' case counts for the same event date across successive database versions.
#' nowcasting_diff_inla, calculates reporting delays based on differences between database versions and fits a statistical distribution to the empirical delay.
#'
#' @param dataset Dataset with at least three columns: event start date, database release date, and number of cases. Optionally,
#' a fourth column with age strata can be included. Data must must be agregated by week
#' @param Dmax (in weeks) Window of dates the estimation will act, i.e., till how many past weeks the nowcasting will estimate. Default is 10 weeks.
#' @param wdw (in weeks) Until which maximum amount of weeks the Nowcasting will use to the estimation. Default is 10 weeks.
#' @param use.epiweek If TRUE, it uses the CDC epiweek definition where the week starts on Sunday, if FALSE it the week ends at the weekday of the last record date. Default is FALSE
#' @param silent Deprecated. Should be the warnings turned off? . The default is TRUE.
#' @param K (in weeks) How much weeks to forecast ahead? . The default is K = 0, no forecasting ahead
#' @param age_col Column for ages
#' @param date_start Column containing the dates when the events ocurred. Data must must be agregated by week 
#' @param date_release Column containing the dates when the databases were released. Data must must be agregated by week
#' @param trajectories Returns the trajectories estimated from the inner 'INLA' model . The default is FALSE.
#' @param zero_inflated Experimental! In non-structured models, fit a model that deals with zero-inflated data. The default is FALSE. If the age_col is not missing this flag is ignored.
#' @param timeREmodel Latent model for time random effects. . The default is a second-order random walk model.
#' @param INLAoutput return the INLA output. Default is FALSE.
#' @param INLAoutputOnly return the only the INLA output. Default is FALSE.
#' @param WAIC return the WAIC. The default is FALSE.
#' @param DIC return the DIC.The default is FALSE
#' @param diff_data Nowcasting based on database differences to estimate reporting delays.
#' @param cases Number of reported cases. 
#' @param ... list parameters to other functions
#'
#' @return a list of 2 elements, each element with a data.frame with nowcasting estimation, 'Total',
#' 'data' with the time-series out of wdw .
#' If 'age_col' is parsed, add a third element with by age estimation 'age' .
#' If 'trajectories' = TRUE, add a forth element with the returned trajectories from 'inla'.
#' If 'INLAoutput' = TRUE, the INLA output is returned as a list object named 'output'.
#' If 'INLAoutputOnly' = TRUE, just the INLA output is returned in a list object named 'output'.
#' If 'WAIC' = TRUE or 'DIC' = TRUE, then 'INLAoutput' is forced to be TRUE returning the INLA output and a list object named waic or dic are also returned.
#' @export
#'
#' @examples
#' # Loading Belo Horizonte SARI dataset
#'
#' data(flu_mg)
#'
#' now_diff<- nowcasting_diff_inla(dataset= flu_MG,
#' date_start = dt_start,
#' date_release = dt_release,
#' Dmax = 10,
#' wdw=10,
#' cases = cases,
#' silent=F
#' )

#'
#' tail(now_diff$total)

nowcasting_diff_inla <- function(dataset,
                            Dmax = 10,
                            wdw = 10,
                            date_start,
                            date_release,
                            cases,
                            age_col,
                            silent = F,
                            K = 0,
                            trajectories = F,
                            zero_inflated = F,
                            timeREmodel = "rw2",
                            INLAoutput = F,
                            INLAoutputOnly = F,
                            WAIC = F, DIC = F,
                            ...){

  dots<-list(...)

  # Workaround check
  Y <- Time <- delay <- dt.aux <- fx_etaria <- Delay <- NULL

  ## Safe tests
  if(missing(dataset)){
    stop("Dataset is missing!")
  }

  if(missing(date_start) | missing(date_release)){
    stop("'date_start' or 'date_release' missing! Please give a column name for each of this parameters")
  }
  if(K < 0 ){
    stop("K less than 0, we cannot produce backcasting! \n
         Please set the K to anything greater than 0 to Forecasting")
  }

if(missing(cases)){
stop("'cases' missing! Please give a column name for this parameters") 
}
  
  ####diff_base warning
  
  if(missing(cases)){
    stop("'cases' is missing! Please provide this parameter")
  }
  
  # Dealing with INLA output

  INLAoutput.aux = INLAoutput

  ## Forcing INLA output TRUE when INLAoutput is FALSE
  if(INLAoutputOnly == T & INLAoutput == F) INLAoutput.aux = T

  ## Forcing INLA output TRUE when either WAIC or DIC are TRUE
  if((WAIC == T | DIC == T) & INLAoutput.aux == F) INLAoutput.aux = T

  
  # checking n_base
  n_base<-length(unique(dplyr::pull(dataset, {{date_release}})))

  ## Warnings
  if(missing(silent) | silent == FALSE){

    ## K parameter and trim.data warnings
    if (K > 0 ){
      warning(paste0("Using K = ", ", is that right?"))
    }else{
      if(K > 0){
        message(paste0("Forecasting with K ", K, " ahead"))
      }else{
        message("Nowcasting only")
      }
    }
    
    ## Missing Dmax warning
    if(missing(Dmax)){
      Dmax <- 10
      warning("Using default to maximum delay, 'Dmax = 10'")
    }else if (Dmax>n_base) {
      message("The number of databases used is lower than the specified Dmax. Using Dmax=", n_base ," as maximum delay")
      Dmax<-n_base
    }else{
      Dmax<-Dmax
      message("Using Dmax inputed")
    }
    
    ## Missing wdw warning
    if(missing(wdw)){
      wdw <- 10
      warning("Using default to window of action, 'wdw = 10'")
    }else if(wdw>n_base) {
      message("The number of databases used is lower than specified wdw. Using wdw=", n_base )
      wdw<-n_base
       }else{
      wdw<-wdw
      message("Using wdw inputed")
    }
  
  
    if(missing(trajectories) | trajectories == FALSE){
      warning("Not returning trajectories")
    }else{
      trajectories = TRUE
      message("Trajectories returned")

      # Nao entendi esse warning (Leo)
      if(!missing(age_col) & !missing(zero_inflated)){
        zero_inflated<-FALSE
        warning("'age_col' parsed, 'zero_inflated' ignored!")
      }
    }


    # ## Missing return.age warning
    # if(missing(return.age)){
    #   return.age <- TRUE
    #   warning("Using default to returning estimate by age, return.age = TRUE")
    # }
    ## Missing age_col warning
    if(missing(age_col)){
      warning("'age_col' missing, nowcasting with unstructured model")
      ## Missing bins_age column warning
    }else{
      message("'age_col' inputed, nowcasting with structured model")
    }
    
  }

  ## Objects for keep the nowcasting
  ## Filtering out cases without report date
  if(missing(age_col)){
    
Tmax <- max(dataset |>
            dplyr::pull(var = {{date_start}}))
    
data.inla <- dataset |> 
      dplyr::rename(date_release = {{date_release}},
                    date_onset = {{date_start}},
                    cases = {{cases}})|>
  dplyr::select( date_onset,date_release, cases) |>
  dplyr::filter(date_onset>=min(date_release)) |>
  dplyr:: arrange(date_onset, date_release) |>
  dplyr::group_by(date_onset) |>
  dplyr:: mutate(delay = row_number() - 1) |>  
  dplyr::arrange(delay, .by_group = TRUE) |>
  dplyr::mutate(
        Y = cases - lag(cases, default = 0),
        Y = pmax(Y, 0)
      ) |>
  dplyr::ungroup() |>
  ## Filter for dates
  dplyr::filter(date_onset >= Tmax - 7 * wdw,
                delay <= Dmax)   
    
  }else{
    
Tmax <- max(dataset |>
                  dplyr::pull(var = {{date_start}}))

data.inla <- dataset |> 
      dplyr::rename(date_release = {{date_release}},
                    date_onset = {{date_start}},
                    cases = {{cases}},
                    fx_etaria={{age_col}}) |>
  dplyr::select( date_onset,date_release, fx_etaria, cases) |>
  dplyr::filter(date_onset>=min(date_release)) |>
  dplyr::arrange(fx_etaria,date_onset, date_release) |>
  dplyr::group_by(fx_etaria, date_onset) |>
  dplyr::mutate(delay = row_number() - 1) |>  
  dplyr::arrange(delay, .by_group = TRUE) |>
  dplyr::mutate(
    Y = cases - lag(cases, default = 0),
    Y = pmax(Y, 0)
  ) |>
  dplyr::ungroup() |>
  ## Filter for dates
  dplyr::filter(date_onset >= Tmax - 7 * wdw,
                delay <= Dmax) 

  }

  ## Parameters of Nowcasting estimate

  ## Auxiliary date table
  if(K==0){
    dates <- range(data.inla |>
                    dplyr::pull(var = date_onset))
  } else {
    ## This is done to explicitly say for the forecast part that its date of onset is the present date
    date_k <- max(data.inla$date_onset) + 7*K
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
    # Nowcasting no age

    if(zero_inflated){
      ## Nowcasting estimate
      sample.now <- nowcasting_no_age(dataset = data.inla,
                                      zero_inflated = T,
                                      timeREmodel = timeREmodel,
                                      INLAoutput = INLAoutput.aux,
                                      INLAoutputOnly = INLAoutputOnly,
                                      WAIC = WAIC, DIC = DIC)
    }else{
      ## Nowcasting estimate
      sample.now <- nowcasting_no_age(dataset = data.inla,
                                      zero_inflated = F,
                                      timeREmodel = timeREmodel,
                                      INLAoutput = INLAoutput.aux,
                                      INLAoutputOnly = INLAoutputOnly,
                                      WAIC = WAIC, DIC = DIC)
    }

    ## Summary on the posteriors of nowcasting
    if(!INLAoutputOnly){
      now_summary<-nowcasting.summary(trajectory = sample.now$sample,
                                      age = F)
      # l<-1
    }else{
      now_summary <- list()
    }

  }else{
    # Nowcasting by age groups

    if(zero_inflated){
      ## Zero inflated by age
      sample.now <- nowcasting_age(dataset = data.inla,
                                   zero_inflated = T,
                                   timeREmodel = timeREmodel,
                                   INLAoutput = INLAoutput.aux,
                                   INLAoutputOnly = INLAoutputOnly,
                                   WAIC = WAIC, DIC = DIC)
    }else{
      ## Negative binomial by age
      sample.now <- nowcasting_age(dataset = data.inla,
                                   zero_inflated = F,
                                   timeREmodel = timeREmodel,
                                   INLAoutput = INLAoutput.aux,
                                   INLAoutputOnly = INLAoutputOnly,
                                   WAIC = WAIC, DIC = DIC)
    }

    ## Summary on the posteriors of nowcasting
    if(!INLAoutputOnly){
      now_summary<-nowcasting.summary(trajectory = sample.now$sample,
                                    age = T)
    } else {
      now_summary <- list()
    }
    # l<-0
  }

  ## Objects to be returned

  if(!INLAoutputOnly){
 
      # # if(missing(age_col)){
      # now_summary[[3-l]]<- data.inla
      # # }
      #
      # # now_summary[[3-l]]<-data_w |>
      # #   dplyr::group_by(date_onset) |>
      # #   dplyr::summarise(observed = dplyr::n(),
      # #                    Delay = Delay)
      #
      # names(now_summary)[3-l]<-"data"

      now_summary$data <- data.inla
    
    if(trajectories){
      # now_summary[[4-l]]<-sample.now
      # names(now_summary)[4-l]<-"trajectories"
      now_summary$trajectories <- sample.now$sample
    }else {
      if(trajectories){
        # now_summary[[3-l]]<-sample.now
        # names(now_summary)[3-l]<-"trajectories"
        now_summary$trajectories <- sample.now$sample
      }
    }

  }

  if(INLAoutput.aux) now_summary$output <- sample.now$INLAoutput
  if(WAIC) now_summary$waic <- sample.now$INLAoutput$waic$waic
  if(DIC) now_summary$dic <- sample.now$INLAoutput$dic$dic 
  
  now_summary$data <- data.inla

  ## Final object returned
  return(now_summary)

}

