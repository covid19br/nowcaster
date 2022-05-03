#' nowcasting_inla
#'
#' @description function to estimate amount of events already started by not yet notified.
#' The main use is to estimate how many cases in a outbreak has already started their onset date of symptons
#' but has not yet notified.
#' nowcasting_inla, fits a statistical distribution to the empirical distribution of time of delay between
#' onset date and report date.
#'
#' @param dataset Dataset with at least 3 columns, date of onset, date of report and stratum.
#' @param trim.data # (in days) Date to be trimmed out from the data base, in days.
#' [Default] 0 days.
#' @param Dmax (in weeks) Until which maximum amount of weeks the Nowcasting will use for the estimation.
#' [Default] 15 weeks.
#' @param wdw (in weeks) Window of dates the estimation will act, i.e., how many past weeks the nowcasting will estimates.
#' [Default] 30 weeks.
#' @param data.by.week If it has to be returned the whole time-series data.
#' [Default] FALSE.
#' @param return.age [Depracted] If the estimate by Age should be returned.
#' [Default] TRUE.
#' @param bins_age Age bins to do the nowcasting, it receive a vector of age bins,
#' or options between, "SI-PNI", "10 years", "5 years".
#' [Default] "SI-PNI".
#' @param silent [Depracted] Should be the warnings turned off?
#' [Default] is TRUE.
#' @param K How much weeks to forecast ahead?
#' [Default] K is 0, no forecasting ahead
#' @param age_col Column for ages
#' @param ...
#'
#' @return a list of 2 elements, each element with a data.frame with nowcasting estimation, $[1] 'Total', $[2] by 'Age'.
#' If data.by.week = TRUE, add a $[3] 'dados' with the time-series out of wdw.
#' @export
#'
#' @examples
nowcasting_inla <- function(dataset,
                            bins_age="SI-PNI",
                            trim.data,
                            Dmax = 15,
                            wdw = 30,
                            age_col,
                            # date_onset,
                            # date_report,
                            data.by.week = FALSE,
                            # return.age = NULL,
                            silent = F,
                            K = 0,
                            trajectories = F,
                            ...){

  # ## Loading required packages
  require(tidyverse)
  # require(lubridate)
  # require(vroom)
  # require(INLA)
  #
  ## Safe tests
  if(missing(dataset)){
    stop("Dataset is missing!")
  }
  if(ncol(dataset) < 3){
    stop("Dataset does not have 3 columns!")
  }

  ## Warnings
  if(!silent){
    if(missing(bins_age)){
      bins_age <- "SI-PNI"
      warning("Using 'SI-PNI' age bins!")
    }

    if(missing(trim.data)){
      trim.data <- 0
      warning("Using default to trim dates, trim.data = 0")
    }

    if(missing(Dmax)){
      Dmax <- 15
      warning("Using default to maximum delay, Dmax = 15")
    }

    if(missing(wdw)){
      wdw <- 30
      warning("Using default to window of action, wdw = 30")
    }

    if(missing(data.by.week)){
      data.by.week <- FALSE
      warning("Using default to returning option for the data, data.by.week = FALSE")
    }

    if(missing(return.age)){
      return.age <- TRUE
      warning("Using default to returning estimate by age, return.age = TRUE")
    }
    if(missing(age_col)){
      warning("Age_col missing, nowcasting with unstructured data")
    }
  }

  ## Objects for keep the nowcasting
  ## Filtering out cases without report date
  if(missing(age_col)){
    dados<-dataset %>%
      dplyr::select(DT_DIGITA, DT_SIN_PRI) %>%
      tidyr::drop_na(DT_DIGITA)
  } else {
    dados <- dataset %>%
      # dplyr::mutate(IDADE = NU_IDADE_N) %>%
      dplyr::select(DT_DIGITA, DT_SIN_PRI, {{age_col}}) %>%
      tidyr::drop_na(DT_DIGITA)
  }

  ## Filtering data to the parameters setted above
  if(missing(age_col)){
    dados_w<-nowcaster::dados.w_no_age(dataset = dados,
                            trim.data = trim.data)
  }else {
    dados_w <- nowcaster::dados.w(dataset = dados,
                       bins_age = bins_age,
                       trim.data = trim.data,
                       age_col = {{age_col}})
  }

  ## Parameters of Nowcasting estimate
  Tmax <- max(dados_w$DT_SIN_PRI)

  ## Parameter of stratum

  ## Data to be entered in Nowcasting function
  ##
  if(missing(age_col)){
    dados.inla <- dados_w %>%
      ## Filter for dates
      dplyr::filter(DT_SIN_PRI >= Tmax - 7 * wdw,
                    Delay <= Dmax) %>%
      ## Group by on Onset dates, Amounts of delays and Stratum
      group_by(DT_SIN_PRI, delay = Delay) %>%
      ## Counting
      dplyr::tally(name = "Y") %>%
      dplyr::ungroup()
  } else {
    dados.inla <- dados_w %>%
      ## Filter for dates
      dplyr::filter(DT_SIN_PRI >= Tmax - 7 * wdw,
                    Delay <= Dmax) %>%
      ## Group by on Onset dates, Amounts of delays and Stratum
      dplyr::group_by(DT_SIN_PRI, delay = Delay, fx_etaria) %>%
      ## Counting
      dplyr::tally(name = "Y") %>%
      dplyr::ungroup()
  }


  ## Auxiliary date table
  #if(K==0){
  dates<-unique(dados.inla$DT_SIN_PRI)
  #} else {
  # dates<-c(unique(dados.inla$DT_SIN_PRI),(max(dados.inla$DT_SIN_PRI) + 7*K))
  #}
  ## Talvez isso não precise se a gente voltar as datas de primeiros sintomas para a data dela correspondente
  ## To make an auxiliary date table with each date plus an amount of dates  to forecast
  tbl.date.aux <- tibble::tibble(
    DT_SIN_PRI = dates
  ) %>%
    tibble::rowid_to_column(var = "Time")

  ## Joining auxiliary date tables
  dados.inla <- dados.inla %>%
    dplyr::left_join(tbl.date.aux)

  ## Time maximum to be considered
  Tmax.id <- max(dados.inla$Time)

  # Auxiliary date table on each stratum, By age
  if(missing(age_col)){
    tbl.NA <-
      expand.grid(Time = 1:(Tmax.id+K),
                  delay = 0:Dmax) %>%
      dplyr::left_join(tbl.date.aux, by = "Time")
  } else{
    tbl.NA <-
      expand.grid(Time = 1:(Tmax.id+K),
                  delay = 0:Dmax,
                  fx_etaria = unique(dados.inla$fx_etaria)
      )%>%
      dplyr::left_join(tbl.date.aux, by = "Time")
  }

  ## Joining the auxiliary date table by Stratum
  if(missing(age_col)){
    dados.inla <- dados.inla %>%
      dplyr::full_join(tbl.NA) %>%  #View()
      dplyr::mutate(
        Y = ifelse(Time + delay > Tmax.id, as.numeric(NA), Y),
        ## If Time + Delay is greater than Tmax, fill with NA
        Y = ifelse(is.na(Y) & Time + delay <= Tmax.id, 0, Y ),
        ## If Time + Delay is smaller than Tmax AND Y is NA, fill 0
      ) %>%
      dplyr::arrange(Time, delay)%>%
      dplyr::rename(dt_event = DT_SIN_PRI)
  }else {
    dados.inla <- dados.inla %>%
      dplyr::full_join(tbl.NA) %>%  #View()
      dplyr::mutate(
        Y = ifelse(Time + delay > Tmax.id, as.numeric(NA), Y),
        ## If Time + Delay is greater than Tmax, fill with NA
        Y = ifelse(is.na(Y) & Time + delay <= Tmax.id, 0, Y ),
        ## If Time + Delay is smaller than Tmax AND Y is NA, fill 0
      ) %>%
      dplyr::arrange(Time, delay, fx_etaria)%>%
      dplyr::rename(dt_event = DT_SIN_PRI)
  }
  ## Precisamos transformar essa datas de volta no valor que é correspondente delas,
  ## a ultima data de primeiro sintomas foi jogada pra até uma semana atrás


  if(missing(age_col)){
    ## Nowcasting estimate
    sample.now <- nowcasting_no_age(dados.age = dados.inla)

    ## Summary on the posteriors of nowcasting
    now_summary<-nowcasting.summary(sample.now,
                                    age = F)
    l<-1
  } else {
    ## Nowcasting estimate
    sample.now <- nowcasting_age(dados.age = dados.inla)

    ## Summary on the posteriors of nowcasting
    now_summary<-nowcasting.summary(sample.now,
                                    age = T)
    l<-0
  }

  ## Objects to be returned

  if(data.by.week){

    now_summary[[3-l]]<-dados_w
    names(now_summary)[3-l]<-"dados"

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

  ## Returning Age
  # if(!return.age){
  #   if(trajectories){
  #     now_summary<-now_summary$total
  #
  #     now_summary[[4]]<-sample.now
  #     names(now_summary)[4]<-"trajectories"
  #   } else {
  #     now_summary <- now_summary$total
  #   }
  # }

  return(now_summary)

}

