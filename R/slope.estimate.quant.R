#' @title Slope.estimate.quant
#'
#' @description
#' Fits a linear model to trajectories returned from 'nowcasting_inla()' within a given window. The default is 3 weeks. If 'end.week' is missing uses the maximum date in 'trajectories'.
#'
#' @param trajectories Data.frame with the predicted or nowcasted estimate
#' @param end.week (in weeks) The end of the week wanted to the slope estimate. Default: max. date in 'trajectories'.
#' @param window (in weeks) Window of how much time will be used to calculate the slope estimate. The default is 3 weeks.
#'
#' @return The numerical value of the slope of the estimate
#' @export
#'
#' @examples
#' # Loading Belo Horizonte SARI dataset
#' data(sragBH)
#' now <- nowcasting_inla(dataset = sragBH,
#'                 date_onset = DT_SIN_PRI,
#'                 date_report = DT_DIGITA,
#'                 trajectories = TRUE,
#'                 silent = T)
#' slope.estimate.quant(trajectories = now$trajectories)
slope.estimate.quant <- function(end.week, trajectories, window=3){

  if(missing(trajectories)){
    stop("'trajectories' is missing!")
  }

  ## If 'end.week' is missing uses the max. date from trajectories
  if(missing(end.week)){
    end.week<-max(trajectories$dt_event)
    warning("Using max. date in 'trajectories' as 'end.week'")
  }

  ## Handling trajectories object
  trajectories<-trajectories |>
    dplyr::rename(Date = dt_event,
                  Cases = Y)

  ## Testing if the amount of weeks in the data encompass the size of the window
  base.week <- end.week - (window*7 - 7)
  if (!base.week %in% trajectories$Date){
    base.week <- min(trajectories$Date)
    warning("'base.week' wasn't present in 'trajectories', using min. of date.")
  }

  ## Normalizing the cases column name
  norm.cases <- trajectories |>
    dplyr::filter(Date == base.week) |>
    dplyr::rename(database_value = Cases) |>
    dplyr::group_by(sample, database_value) |>
    dplyr::select(-Time, -Date) |>
    dplyr::right_join(trajectories |>
                        dplyr::filter(Date >= base.week & Date <= end.week),
                      by='sample') |>
    dplyr::mutate(Cases = dplyr::case_when(
      database_value > 0 ~ Cases/database_value,
      TRUE ~ Cases))

  ## Testing the amount of samples in the trajectories, if it unique, fit the line and get the slope
  ## if it is more than one, fit for each sample the line and does statistical estimates for the slope
  if (length(norm.cases$sample |>  unique()) == 1){

    ## The line model fitting
    tmp <- stats::lm(Cases ~ Date, data = norm.cases)

    ## Confidence Intervals
    l <- stats::confint(tmp, parm = 'Date', level = .9)
    q <- stats::confint(tmp, parm = 'Date', level = .5)

    ## Slope estimate
    slope <- dplyr::case_when(
      l[1] > 0 ~ 1,
      l[1] < 0 & q[1] >= 0 ~ .5,
      q[1] < 0 & q[2] > 0 ~ 0,
      q[2] <= 0 & l[2] > 0 ~ -.5,
      l[2] < 0 ~ -1
    ) |>
      as.numeric()

  } else {

    ## More than one sample
    tmp <- lme4::lmList(Cases ~ Date | sample, data = norm.cases)

    slope <- stats::coefficients(tmp) |>
      dplyr::select(Date) |>
      ## Quantiles calculations
      dplyr::summarise(LI = stats::quantile(Date, probs=.05, na.rm=T),
                       Q1 = stats::quantile(Date, probs=.25, na.rm=T),
                       Q3 = stats::quantile(Date, probs=.75, na.rm=T),
                       LS = stats::quantile(Date, probs=.95, na.rm=T)) |>
      dplyr::ungroup() |>
      dplyr::transmute(slope = dplyr::case_when(
        LI > 0 ~ 1,
        LI < 0 & Q1 >= 0 ~ .5,
        Q1 < 0 & Q3 > 0 ~ 0,
        Q3 <= 0 & LS > 0 ~ -.5,
        LS < 0 ~ -1
      ) ) |>
      as.numeric()

  }

  ## Return the slope
  return(slope)
}
