#' @title Nowcasting with a negative binomial GAM fitted by 'mgcv'
#'
#' @description Estimates the number of events that have already started but are not yet notified.
#' The main use is to estimate how many cases in an outbreak have already started their symptoms
#' but has not yet notified.
#' nowcasting_mgcv, fits a statistical distribution to the empirical distribution of time of delay between
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
#' @param method.group.gam Type of smooth used by age class in the structured model: "fs" (factor smooth, default) or "by" (one smooth per age class).
#' @param fit How the negative binomial GAM is fitted:
#'   * `"gam"` (default): [mgcv::gam()], the original fit.
#'   * `"bam"`: [mgcv::bam()] with `method = "fREML"`, a faster fit for large
#'     data. On the age-structured model it was about 7 times faster than
#'     `"gam"` on the example data, with nowcasts within about 1%.
#'   * `"bam_discrete"`: [mgcv::bam()] with `discrete = TRUE`, which
#'     discretises the covariates. It was over 40 times faster than `"gam"` on
#'     the age-structured model. With `method.group.gam = "by"` the nowcasts
#'     matched `"gam"` closely; with `"fs"` the estimates for the most recent
#'     weeks were up to about 8% lower on the example data.
#' @param K (in weeks) How many weeks to forecast ahead. The default is K = 0, no forecasting ahead.
#' @param age_col Column with the age (in years) of each case. If given, the structured (age-stratified) model is fitted.
#' @param date_onset Column of dates of onset of the events, normally date of onset of first symptoms of cases.
#' @param date_report Column of dates of report of the events, normally the date the notification was recorded in the system.
#' @param trajectories Returns the predictive trajectories. The default is FALSE.
#' @param silent Deprecated. Should the warnings be turned off? The default is FALSE.
#' @param ... list parameters to other functions
#'
#' @return A list. Element 'total' is a data.frame with the nowcasting estimate: predictive 'Median',
#' 95% interval ('LI', 'LS') and 50% interval ('LIb', 'LSb') for each onset week.
#' If 'data.by.week' = TRUE, element 'data' holds the delay-triangle data used to fit the model.
#' If 'age_col' is given, element 'age' holds the estimate by age class.
#' If 'trajectories' = TRUE, element 'trajectories' holds the predictive trajectories sampled from the GAM.
#' @seealso [nowcasting_inla()] for the Bayesian version of the model fitted with 'INLA'.
#' @export
#'
#' @examples
#' \donttest{
#' # Loading Belo Horizonte SARI dataset
#' data(sragBH)
#' output <- nowcasting_mgcv(dataset = sragBH,
#'                           date_onset = DT_SIN_PRI,
#'                           date_report = DT_DIGITA)
#' head(output$total)
#' }
nowcasting_mgcv <- function(dataset,
                            bins_age="SI-PNI",
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
                            method.group.gam = "fs",
                            K = 0,
                            trajectories = FALSE,
                            fit = c("gam", "bam", "bam_discrete"),
                            ...){

  fit <- match.arg(fit)

  dots<-list(...)

  # Workaround check
  Y <- Time <- delay <- dt.aux <- fx_etaria <- Delay <- NULL

  ## Input checks
  if(missing(dataset)){
    stop("Dataset is missing!")
  }
  if(ncol(dataset) < 2){
    stop("Dataset does not have 2 columns!")
  }
  if(missing(date_onset) | missing(date_report)){
    stop("'date_onset' or 'date_report' missing! Please give a column name for each of this parameters")
  }
  if(K < 0 ){
    stop("K less than 0, we cannot produce backcasting! \n
         Please set the K to anything greater than 0 to Forecasting")
  }
  method.group.gam <- match.arg(method.group.gam, c("fs", "by"))

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
    data2model <- data_w  |>
      ## Filter for dates
      dplyr::filter(date_onset >= Tmax - 7 * wdw,
                    Delay <= Dmax)  |>
      ## Group by on Onset dates, Amounts of delays and Stratum
      dplyr::group_by(date_onset, delay = Delay)  |>
      ## Counting
      dplyr::tally(name = "Y")  |>
      dplyr::ungroup()
  } else {
    data2model <- data_w  |>
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
  dates <- range(data2model$date_onset)

  # Adding forecast
  dates[2] <- dates[2] + 7*K


  ## To make an auxiliary date table with each date plus an amount of dates  to forecast
  tbl.date.aux <- tibble::tibble(
    date_onset = seq(dates[1], dates[2], by = 7)
  )  |>
    tibble::rowid_to_column(var = "Time")

  ## Joining auxiliary date tables
  data2model <- data2model  |>
    dplyr::left_join(tbl.date.aux, by = "date_onset")

  ## Time maximum to be considered
  Tmax.id <- max(data2model$Time)

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
                  fx_etaria = unique(data2model$fx_etaria)
      ) |>
      dplyr::left_join(tbl.date.aux, by = "Time")
  }

  ## Joining the auxiliary date table by Stratum
  if(missing(age_col)){
    data2model <- data2model  |>
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
    data2model <- data2model  |>
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

      ## Nowcasting estimate
      sample.now <- nowcasting_no_age_mgcv(dataset = data2model, fit = fit)

      now_summary<-nowcasting.summary(trajectory = sample.now$sample,
                                      age = F)
  } else {
    # Nowcasting by age groups

    ## Negative binomial by age
    sample.now <- nowcasting_age_mgcv(dataset = data2model,
                                      method = method.group.gam,
                                      fit = fit)

    now_summary<-nowcasting.summary(trajectory = sample.now$sample,
                                    age = T)
  }

  ## Objects to be returned

  if(trajectories) now_summary$trajectories <- sample.now$sample

  if(data.by.week) now_summary$data <- data2model


  ## Final object returned
  return(now_summary)

}

