#' @title Fit the non-structured nowcasting model with 'INLA'
#'
#' @description Fits the Bayesian negative binomial model with 'INLA' on non-structured data and
#' samples the posterior predictive trajectories. Data has to be in the delay-triangle format.
#' Usually called internally by [nowcasting_inla()].
#'
#' @param dataset data pre-formatted into delays by week for each case, delay-triangle format
#' @param zero_inflated zero-inflated model. Default is FALSE.
#' @param timeREmodel Latent model for time random effects. The default is a second-order random walk model.
#' @param INLAoutput return the INLA output. Default is FALSE.
#' @param INLAoutputOnly return only the INLA output. Default is FALSE.
#' @param WAIC return the WAIC. Default is FALSE.
#' @param DIC return the DIC. Default is FALSE.
#'
#' @return A list with element 'sample', the posterior predictive trajectories, and optionally the 'INLA' output.
#' @export
nowcasting_no_age_inla <- function(dataset,
                              zero_inflated=FALSE,
                              timeREmodel = "rw2",
                              INLAoutput = FALSE,
                              INLAoutputOnly = FALSE,
                              WAIC = FALSE, DIC = FALSE
                              ){
  ## Safe test
  if(missing(dataset)){
    stop("'dataset' is missing in 'nowcasting_no_age_inla()'.")
  }

  check_inla()

  ## Check for the zero-inflated
  if (zero_inflated){
    family <- "zeroinflatednbinomial2"

    control.family <- list(
      hyper = list("theta1" = list(prior = "loggamma",
                                   param = c(0.001, 0.001)),
                   # INLA default hyper parameters for a
                   "theta2" = list(prior = "gaussian",
                                   param = c(2, 1)))
      # hyper = list("theta1" = list(prior = "loggamma",
      #                              param = c(0.01, 0.01)),
      #              "theta2" = list(prior = "gaussian",
      #                              param = c(0, 0.4)))
    )
  } else {
    family <- 'nbinomial'
    control.family <- list(
      hyper = list("theta" = list(prior = "loggamma",
                                  param = c(0.001, 0.001)))
    )
  }

  index.missing <- which(is.na(dataset$Y))

  ## Model equation: intercept + f(time random effect) + f(Delay random effect)
  ## Y(t) ~ 1 + rw2(t) + rw1(delay),
  ## prec(rw2) ~ logGamma(10e-3, 10e-3), prec(rw1) ~ logGamma(10e-3, 10e-3)
  model <- stats::as.formula(paste0(
    "Y ~ 1 + f(Time, model = \"", timeREmodel,"\",
        hyper = list(\"prec\" = list(prior = \"loggamma\",
                                   param = c(0.001, 0.001))
        )) +
      f(delay, model = \"rw1\",
        hyper = list(\"prec\" = list(prior = \"loggamma\",
                                   param = c(0.001, 0.001)))
      )"))

  ## Running the Negative Binomial model in INLA
  output0 <- INLA::inla(model,
                        family = family,
                        data = dataset,
                        control.predictor = list(link = 1, compute = T),
                        control.compute = list( config = T, waic=WAIC, dic=DIC),
                        control.family = control.family
  )

  # }

  output <- list()

  if(INLAoutput) output$INLAoutput <- output0

  if(!INLAoutputOnly){

    ## Step 1: Sampling from the approximate posterior distribution using INLA
    srag.samples0.list <- INLA::inla.posterior.sample(n = 1000, output0)

    ## Step 2: Sampling the missing triangle from the likelihood using INLA estimates
    draws <- sample_missing_inla(srag.samples0.list, idx = index.missing,
                                 zero_inflated = zero_inflated)

    ## Step 3: Trajectories: observed plus sampled counts, by week of onset
    output$sample <- aggregate_trajectories(dataset, idx = index.missing,
                                            draws = draws)
  }else{
    output$sample <- NA
  }

  return(output)

}
