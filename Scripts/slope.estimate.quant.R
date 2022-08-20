suppressWarnings(suppressPackageStartupMessages(library("tidyverse")))
suppressWarnings(suppressPackageStartupMessages(library(lme4)))

#' @title Slope.estimate
#'
#' @param end.week [in weeks] The end of the week wanted to the slope estimate
#' @param trajectories Data.frame with the predicted or nowcasted estimate
#' @param window [in weeks] Window of how much time will be used to calculate the slope estimate
#'
#' @return The slope of the estimate
#' @export
#'
#' @examples
slope.estimate.quant <- function(end.week, trajectories, window=3){

  if(missing(end.week)){
    end.week<-max(trajectories$dt_event)
  }

  trajectories<-trajectories |>
    dplyr::rename(Date = dt_event,
                  Casos = Y)

  ## Testing if the amount of weeks in the data encompasse the size of the window
  base.week <- end.week - (window*7 - 7)
  if (!base.week %in% trajectories$Date){
    return(NA)
  }

  ## Normalizing the cases column name
  norm.casos <- trajectories |>
    dplyr::filter(Date == base.week) |>
    dplyr::rename(valorbase = Casos) |>
    dplyr::group_by(sample, valorbase) |>
    dplyr::select(-Time, -Date)

  norm.casos <- norm.casos |>
    dplyr::right_join(trajectories |>
                        dplyr::filter(Date >= base.week & Date <= end.week),
                      by='sample') |>
    dplyr::mutate(Casos = case_when(
      valorbase > 0 ~ Casos/valorbase,
      TRUE ~ Casos))

  ## Testing the amount of samples in the trajectories, if it unique, fit the line and get the slope
  ## if it is more than one, fit for each sample the line and does statistical estimates for the slope
  if (length(norm.casos$sample |>  unique()) == 1){

    ## The line model fitting
    tmp <- stats::lm(Casos ~ Date, data = norm.casos)
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
    tmp <- lme4::lmList(Casos ~ Date | sample, data = norm.casos)
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
