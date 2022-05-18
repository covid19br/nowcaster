#' nowcasting_age
#'
#' @param dataset data pre formatted in to age classes and delays by week for each cases, delay triangle format
#'
#' @return
#' @export
#'
#' @examples
nowcasting_age <- function(dados.age){

  # ## Loading packages
  # require(INLA)
  # require(tidyr)

  index.missing <- which(is.na(dados.age$Y))

  dados.age <- dados.age |>
    dplyr::mutate(
      fx_etaria.num = as.numeric(fx_etaria),
      # delay.grp = inla.group(delay, n = 20)
    )

  ## Model equation: intercept + age + f(time random effect | age) + f(Delay random effect | age)
  ## Y(t,Age) ~ 1 + Age + rw2(t,Age) + rw1(delay, Age),
  ## prec(rw2) ~ logGamma(10e-3, 10e-3), prec(rw1) ~ logGamma(10e-3, 10e-3)
  model <- Y ~ 1 + fx_etaria +
    f(Time,
      model = "rw2",
      hyper = list("prec" = list(prior = "loggamma",
                                 param = c(0.001, 0.001))
      ),
      group = fx_etaria.num, control.group = list(model = "iid")) +
    f(delay, model = "rw1",
      hyper = list("prec" = list(prior = "loggamma",
                                 param = c(0.001, 0.001))),
      group = fx_etaria.num, control.group = list(model = "iid")
    )

  ## Age-Delay effects
  ## f(delay, model = "rw1",
  ## hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001))),
  ## group = fx_etaria.num, control.group = list(model = "iid"))

  # if(zeroinflated){
  #   ## Running the Zero inflated Negative Binomial model in INLA
  #   output0 <- inla(model, family = "zeroinflatednbinomial0", data = dados.age,
  #                   control.predictor = list(link = 1, compute = T),
  #                   control.compute = list( config = T, waic=F, dic=F),
  #                   control.family = list(
  #                     hyper = list("theta1" = list(prior = "loggamma",
  #                                                  param = c(0.001, 0.001))
  #                                  ## "theta2" = list(prior = "gaussian",
  #                                  ##                 param = c(-1, .1))
  #                     )
  #                   )
  #   )
  # }else{
    ## Running the Negative Binomial model in INLA
  output0 <- INLA::inla(model, family = "nbinomial",
                  data = dados.age,
                  control.predictor = list(link = 1, compute = T),
                  control.compute = list( config = T, waic=F, dic=F),
                  control.family = list(
                    hyper = list("theta" = list(prior = "loggamma",
                                                param = c(0.001, 0.001))
                    )
                  )
  )

  # }

  ## Algorithm to get samples for the predictive distribution for the number of cases

  ## Step 1: Sampling from the approximate posterior distribution using INLA
  srag.samples0.list <- INLA::inla.posterior.sample(n = 1000, output0)

  ## Give a parameter to trajectories, TO-DO

  ## Step 2: Sampling the missing triangle from the likelihood using INLA estimates
  vector.samples0 <- lapply(X = srag.samples0.list,
                            FUN = function(x, idx = index.missing){
                              # if(zeroinflated){
                              #   unif.log <- as.numeric(runif(idx,0,1) < x$hyperpar[2])
                              # }else{
                                unif.log = 1
                              # }
                              stats::rnbinom(n = idx,
                                      mu = exp(x$latent[idx]),
                                      size = x$hyperpar[1]
                              ) * unif.log
                            } )

  ## Step 3: Calculate N_{a,t} for each triangle sample {N_{t,a} : t=Tactual-Dmax+1,...Tactual}

  gg.age <- function(x, dados.gg, idx){
    data.aux <- dados.gg
    Tmin <- min(dados.gg$Time[idx])
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
  tibble.samples.0 <- lapply( X = vector.samples0,
                              FUN = gg.age,
                              dados = dados.age,
                              idx = index.missing)

  srag.pred.0 <- dplyr::bind_rows(tibble.samples.0, .id = "sample")

  return(srag.pred.0)

}
