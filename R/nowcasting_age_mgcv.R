#' @title Fit the structured (age) nowcasting GAM
#'
#' @description Fits a negative binomial GAM with 'mgcv' on data structured by age class and
#' samples the predictive trajectories for the unreported cases. Data has to be in the delay-triangle format.
#' Usually called internally by [nowcasting_mgcv()].
#'
#' @param dataset data pre formatted in to age classes and delays by week for each cases, delay triangle format
#' @param method Type of smooth by age class: "fs" (factor smooth, `s(., age_class, bs = "fs")`, default) or "by" (`s(., by = age_class)`).
#' @inheritParams nowcasting_mgcv
#'
#' @return A list with element 'sample', the predictive trajectories by age class.
#' @export
nowcasting_age_mgcv <- function(dataset, method = "fs",
                                fit = c("gam", "bam", "bam_discrete")){

  # Workaround check
  fx_etaria <- NULL

  # ## [Not in use] Check for zero-inflated
  # if (zero_inflated ){
  #   family <- "zeroinflatednbinomial2"
  #   control.family <- list(
  #     hyper = list("theta1" = list(prior = "loggamma",
  #                                  param = c(0.001, 0.001)),
  #                  # INLA default fro a
  #                  "theta2" = list(prior = "gaussian",
  #                                  param = c(2, 1)))
  #   )
  # } else {
  #   family <- 'nbinomial'
  #   control.family <- list(
  #     hyper = list("theta" = list(prior = "loggamma",
  #                                 param = c(0.001, 0.001)))
  #   )
  # }

  index.missing <- which(is.na(dataset$Y))

  dataset <- dataset |>
    dplyr::mutate(
      fx_etaria.num = as.numeric(fx_etaria))


  ## Model equation: intercept + s(time random effect) + s(Delay random effect)
  ## Y(t) ~ 1 + s(t) + s(delay),


  ## Running the Negative Binomial model in mgcv
  ## Basis dimensions: mgcv's default of 10, or fewer when the observed part
  ## of the delay triangle has fewer distinct weeks or delays
  observed <- dataset[!is.na(dataset$Y), ]
  k.time <- gam_basis_dim(observed$Time,
                          "weeks of onset (increase 'wdw')")
  k.delay <- gam_basis_dim(observed$delay,
                           "delays (increase 'Dmax')")

  if(method == "by")
    output0 <- fit_nb_gam(
      stats::as.formula(sprintf(
        "Y ~ 1 + fx_etaria + s(Time, by = fx_etaria, k = %d) + s(delay, by = fx_etaria, k = %d)",
        k.time, k.delay)),
      data = dataset, fit = fit)

  if(method == "fs")
    output0 <- fit_nb_gam(
      stats::as.formula(sprintf(
        "Y ~ 1 + s(Time, fx_etaria, bs = \"fs\", k = %d) + s(delay, fx_etaria, bs = \"fs\", k = %d)",
        k.time, k.delay)),
      data = dataset, fit = fit)

  output <- list()


  ## Predictive distribution of the missing cells: sample the coefficients
  ## from their approximate posterior, then the counts from the negative
  ## binomial with the estimated dispersion
  draws <- sample_missing_gam(output0, newdata = dataset[index.missing, ])

  ## Trajectories: observed plus sampled counts, by week of onset and age class
  output$sample <- aggregate_trajectories(dataset, idx = index.missing,
                                          draws = draws, age = TRUE)


  return(output)

}
