#' @title Fit the structured (age) nowcasting model with 'INLA'
#'
#' @description Fits the Bayesian negative binomial model with 'INLA' on data structured by age class and
#' samples the posterior predictive trajectories. Data has to be in the delay-triangle format.
#' Usually called internally by [nowcasting_inla()].
#'
#' @param dataset data pre formatted in to age classes and delays by week for each cases, delay triangle format
#' @param timeREmodel Latent model for time random effects. The default is a second-order random walk model.
#' @param zero_inflated Experimental! It deals with zero-inflated data by using a zeroinflatednbinomial2 model.
#' @param INLAoutput return the INLA output. Default is FALSE.
#' @param INLAoutputOnly return only the INLA output. Default is FALSE.
#' @param WAIC return the WAIC. Default is FALSE.
#' @param DIC return the DIC. Default is FALSE.
#'
#' @return A list with element 'sample', the posterior predictive trajectories by age class, and optionally the 'INLA' output.
#' @export
nowcasting_age_inla <- function(dataset,
                           zero_inflated=FALSE,
                           timeREmodel = "rw2",
                           INLAoutput = FALSE,
                           INLAoutputOnly = FALSE,
                           WAIC = FALSE, DIC = FALSE){

  # Workaround check
  fx_etaria <- NULL

  check_inla()

  ## [Not in use] Check for zero-inflated
  if (zero_inflated ){
    family <- "zeroinflatednbinomial2"
    control.family <- list(
      hyper = list("theta1" = list(prior = "loggamma",
                                   param = c(0.001, 0.001)),
                   # INLA default fro a
                   "theta2" = list(prior = "gaussian",
                                   param = c(2, 1)))
    )
  } else {
    family <- 'nbinomial'
    control.family <- list(
      hyper = list("theta" = list(prior = "loggamma",
                                  param = c(0.001, 0.001)))
    )
  }

  index.missing <- which(is.na(dataset$Y))

  dataset <- dataset |>
    dplyr::mutate(
      fx_etaria.num = as.numeric(fx_etaria))

  ## Model equation: intercept + age + f(time random effect | age) + f(Delay random effect | age)
  ## Y(t,Age) ~ 1 + Age + rw2(t,Age) + rw1(delay, Age),
  ## prec(rw2) ~ logGamma(10e-3, 10e-3), prec(rw1) ~ logGamma(10e-3, 10e-3)
  model <- stats::as.formula(paste0( "Y ~ 1 + fx_etaria +
    f(Time, model = \"", timeREmodel, "\",
      hyper = list(\"prec\" = list(prior = \"loggamma\",
                                 param = c(0.001, 0.001))
      ),
      group = fx_etaria.num, control.group = list(model = \"iid\")) +
    f(delay, model = \"rw1\",
      hyper = list(\"prec\" = list(prior = \"loggamma\",
                                 param = c(0.001, 0.001))),
      group = fx_etaria.num, control.group = list(model = \"iid\")
    )"))



  ## Running the Negative Binomial model in INLA
  output0 <- INLA::inla(model, family = family,
                        data = dataset,
                        control.predictor = list(link = 1, compute = TRUE),
                        control.compute = list( config = TRUE, waic=WAIC, dic=DIC),
                        control.family = control.family
  )

  output <- list()

  if(INLAoutput) output$INLAoutput <- output0

  if(!INLAoutputOnly){


    ## Step 1: Sampling from the approximate posterior distribution using INLA
    srag.samples0.list <- INLA::inla.posterior.sample(n = 1000, output0)

    ## Step 2: Sampling the missing triangle from the likelihood using INLA estimates
    draws <- sample_missing_inla(srag.samples0.list, idx = index.missing,
                                 zero_inflated = zero_inflated)

    ## Step 3: Trajectories: observed plus sampled counts, by week of onset and age class
    output$sample <- aggregate_trajectories(dataset, idx = index.missing,
                                            draws = draws, age = TRUE)
  }else{
    output$sample <- NA
  }

  return(output)

}
