#' @title nowcasting_no_age_mgcv
#'
#' @description Run INLA model on non-structured data,
#' data has to be in the format of delay-triangle
#'
#' @param dataset data pre formatted in to age classes and delays by week for each cases,
#' delay triangle format

#'
#' @return Trajectories from the Negative Binomial GAM
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


  ## Algorithm to get samples for the predictive distribution for the number of cases

  ## Step 1: Sampling from the approximate posterior distribution of the coefficients

  betas.p <- mgcv::rmvn(n = 1000, coef(output0), output0$Vp)


  # Step 2: Get the design matrix (for the predictive values)
  Xp <- predict(output0, type = "lpmatrix", newdata = dataset[index.missing,])

  # Step 3: Get the samples from the linear terms
  eta.samples <- Xp %*% t(betas.p)


  n.missing = nrow(eta.samples)

  # Step 4: Sampling the missing triangle (fixing the negative binomial hyperparameter)
  theta.nb <- output0$family$getTheta(T) # NegBin hyper parameter

  # Step 5: Do the same as we did in INLA (sampling the missing triangle)
  vector.samples <- lapply(X = 1:1000,
                           FUN = function(x)
                             rnbinom(n = n.missing, mu = exp(eta.samples[,x]), size = theta.nb))


  ## Step 6: Calculate N_{a,t} for each triangle sample {N_{t,a} : t=Tactual-Dmax+1,...Tactual}

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

  ## Step 7: Applying the age aggregation on each posterior
  tibble.samples <- lapply( X = vector.samples,
                              FUN = gg.age,
                              dados = dataset,
                              idx = index.missing)

  srag.pred <- dplyr::bind_rows(tibble.samples, .id = "sample")

  output$sample <- srag.pred

return(output)

}
