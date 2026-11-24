#' @title Fit the non-structured nowcasting GAM
#'
#' @description Fits a negative binomial GAM with 'mgcv' on non-structured data and
#' samples the predictive trajectories for the unreported cases. Data has to be in the delay-triangle format.
#' Usually called internally by [nowcasting_mgcv()].
#'
#' @param dataset data pre-formatted into delays by week for each case, delay-triangle format
#'
#' @return A list with element 'sample', the predictive trajectories from the negative binomial GAM.
#' @export
nowcasting_no_age_mgcv <- function(dataset){
  ## Safe test
  if(missing(dataset)){
    stop("'dataset' is missing in 'nowcasting_no_age()'.")
  }


  index.missing <- which(is.na(dataset$Y))

  ## Model equation: intercept + s(time random effect) + s(Delay random effect)
  ## Y(t) ~ 1 + s(t) + s(delay),

  ## Running the Negative Binomial model in mgcv
  output0 <- mgcv::gam(Y ~ 1 + s(Time) + s(delay),
                       family = "nb", data = dataset )

  output <- list()


  ## Predictive distribution of the missing cells: sample the coefficients
  ## from their approximate posterior, then the counts from the negative
  ## binomial with the estimated dispersion
  draws <- sample_missing_gam(output0, newdata = dataset[index.missing, ])

  ## Trajectories: observed plus sampled counts, by week of onset
  output$sample <- aggregate_trajectories(dataset, idx = index.missing,
                                          draws = draws)

return(output)

}
