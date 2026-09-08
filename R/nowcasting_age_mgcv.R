#' @title nowcasting_age_mgcv
#'
#' @description Run INLA model on structured data by age-class
#' data has to be in the format of delay-triangle
#'
#' @param dataset data pre formatted in to age classes and delays by week for each cases, delay triangle format
#' @param method method used grouped gam, "by" where the smooth function will be s(.,  by= agegroup) or "fs" (factor smooth) where the smooth function uses s(., age_group, ,bs="fs")
#'
#' @return Trajectories from the inner 'INLA' model
#' @export
nowcasting_age_mgcv <- function(dataset, method = "fs"){

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
  if(method == "by")
    output0 <- mgcv::gam(Y ~ 1 + fx_etaria + s(Time, by = fx_etaria) +
                           s(delay, by = fx_etaria),
                         family = "nb", data = dataset )

  if(method == "fs")
    output0 <- mgcv::gam(Y ~ 1 + s(Time, fx_etaria, bs = "fs") +
                           s(delay, fx_etaria, bs = "fs"),
                         family = "nb", data = dataset )

  output <- list()


  ## Algorithm to get samples for the predictive distribution for the number of cases

  ## Step 1: Sampling from the approximate posterior distribution of the coefficients

  betas.p <- mgcv::rmvn(n = 1000, stats::coef(output0), output0$Vp)


  # Step 2: Get the design matrix (for the predictive values)
  Xp <- stats::predict(output0, type = "lpmatrix", newdata = dataset[index.missing,])

  # Step 3: Get the samples from the linear terms
  eta.samples <- Xp %*% t(betas.p)


  n.missing = nrow(eta.samples)

  # Step 4: Sampling the missing triangle (fixing the negative binomial hyperparameter)
  theta.nb <- output0$family$getTheta(T) # NegBin hyper parameter

  # Step 5: Do the same as we did in INLA (sampling the missing triangle)
  vector.samples <- lapply(X = 1:1000,
                           FUN = function(x)
                             stats::rnbinom(n = n.missing, mu = exp(eta.samples[,x]), size = theta.nb))

  ## Step 6: Calculate N_{a,t} for each triangle sample {N_{t,a} : t=Tactual-Dmax+1,...Tactual}

  gg.age <- function(x, dados, idx){
    # Workaround check
    Y <- Time <- dt_event <- fx_etaria <- fx_etaria.num <- Delay <- NULL
    data.aux <- dados
    Tmin <- min(dados$Time[idx])
    data.aux$Y[idx] <- x
    data.aggregated <- data.aux |>
      ## Selecionando apenas os dias faltantes a partir
      ## do domingo da respectiva ultima epiweek
      ## com dados faltantes
      dplyr::filter(Time >= Tmin  ) |>
      dplyr::group_by(Time, dt_event, fx_etaria, fx_etaria.num) |>
      dplyr::summarise(
        Y = sum(Y), .groups = "keep"
      )
    data.aggregated
  }

  ## Step 4: Applying the age aggregation on each posterior
  tibble.samples.0 <- lapply( X = vector.samples,
                              FUN = gg.age,
                              dados = dataset,
                              idx = index.missing)

  srag.pred.0 <- dplyr::bind_rows(tibble.samples.0, .id = "sample")

  output$sample <- srag.pred.0


  return(output)

}
