#' @title nowcasting_no_age
#'
#' @description Run INLA model on non-structured data,
#' data has to be in the format of delay-triangle
#'
#' @param dataset data pre formatted in to age classes and delays by week for each cases,
#' delay triangle format
#' @param zero_inflated zero-inflated model. Default is FALSE.
#' @param timeREmodel Latent model for time random effects. The default is a second-order random walk model.
#' @param INLAoutput return the INLA output. Default is FALSE.
#' @param INLAoutputOnly return the only the INLA output. Default is FALSE.
#' @param WAIC return the WAIC. Default is FALSE.
#' @param DIC return the DIC. Default is FALSE.

#'
#' @return Trajectories from the inner 'INLA' model
#' @export
nowcasting_no_age <- function(dataset,
                              zero_inflated=FALSE,
                              timeREmodel = "rw2",
                              INLAoutput = F,
                              INLAoutputOnly = F,
                              WAIC = F, DIC = F
                              ){
  ## Safe test
  if(missing(dataset)){
    stop("'dataset' is missing in 'nowcasting_no_age()'.")
  }

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

    ## Algorithm to get samples for the predictive distribution for the number of cases

    ## Step 1: Sampling from the approximate posterior distribution using INLA
    srag.samples0.list <- INLA::inla.posterior.sample(n = 1000, output0)

    ## Give a parameter to trajectories, TO-DO

    ## Step 2: Sampling the missing triangle from the likelihood using INLA estimates
    vector.samples0 <- lapply(X = srag.samples0.list,
                              FUN = function(x, idx = index.missing){
                                if(zero_inflated){
                                  # unif.log <- as.numeric(runif(idx,0,1) < x$hyperpar[2])
                                  # p = 1 - (exp(eta) / (1+exp(eta)))^a ; eta -> grande; p -> 0
                                  unif.log <- as.numeric(stats::runif(idx,0,1) > 1-(exp(x$latent[idx]) / (1+exp(x$latent[idx])))^x$hyperpar[2])
                                }else{
                                  unif.log = 1
                                }
                                stats::rnbinom(n = idx,
                                               mu = exp(x$latent[idx]),
                                               size = x$hyperpar[1]
                                ) * unif.log
                              } )

    ## Step 3: Calculate N_{a,t} for each triangle sample {N_{t,a} : t=Tactual-Dmax+1,...Tactual}

    gg.age <- function(x, dados.gg, idx){

      Y <- Time <- dt_event <- NULL

      data.aux <- dados.gg
      Tmin <- min(dados.gg$Time[idx])
      data.aux$Y[idx] <- x
      data.aggregated <- data.aux |>
        ## Selecionando apenas os dias faltantes a partir
        ## do domingo da respectiva ultima epiweek
        ## com dados faltantes
        dplyr::filter(Time >= Tmin  ) |>
        dplyr::group_by(Time, dt_event) |>
        dplyr::summarise(
          Y = sum(Y), .groups = "keep"
        )
      data.aggregated
    }

    ## Step 4: Applying the age aggregation on each posterior
    tibble.samples.0 <- lapply( X = vector.samples0,
                                FUN = gg.age,
                                dados = dataset,
                                idx = index.missing)

    srag.pred.0 <- dplyr::bind_rows(tibble.samples.0, .id = "sample")

    output$sample <- srag.pred.0
  }else{
    output$sample <- NA
  }

  return(output)

}
