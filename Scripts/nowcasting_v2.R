# Auxiliar functions for nowcasting

# Auxiliar function, sampling from a negative binomial likelihood
# ff <- function(x, idx){
#   rnbinom(n = idx, mu = exp(x$latent[idx]), size = x$hyperpar[1])
# }

# Zero inflated:
ff <- function(x, idx = index.missing, zero.inflated = TRUE){
  if (zero.inflated){
    aux <- rnbinom(n = idx, mu = exp(x$latent[idx]), size = x$hyperpar[1])
    aux.unif <- rbinom(n = length(idx), 1, prob = 1-x$hyperpar[2])
    return(aux * aux.unif)
  } else {
    rnbinom(n = idx, mu = exp(x$latent[idx]), size = x$hyperpar[1])
  }
}

# Auxiliar function selecionando um pedaco do dataset
gg <- function(x, dados, idx, Fim.sat, Dmax){
  data.aux <- dados
  data.aux$Casos[idx] <- x
  data.aggregated <- data.aux %>%
    # Selecionando apenas os dias faltantes a partir
    # do domingo da respectiva ultima epiweek
    # com dados faltantes
    filter(Date >= Fim.sat - Dmax  ) %>%
    group_by(Date) %>% 
    dplyr::summarise( 
      Casos = sum(Casos) 
    )
  data.aggregated
}


# Algorithm to get samples for the predictive distribution for the number of cases

nowcasting <- function(output.day, dadosRio.ag, 
                       Fim = today.week, Dm = Dmax, zero.inflated = TRUE, n.samples = 1000){
  
  index.missing = which(is.na(dadosRio.ag$Casos))
  
  
  # Step 1: Sampling from the approximate posterior distribution using INLA
  srag.samples.list <- inla.posterior.sample(n = n.samples, output.day)
  
  # Step 2: Sampling the missing triangle (in vector form) from the likelihood using INLA estimates
  vector.samples <- lapply(X = srag.samples.list, 
                           FUN = ff,
                           idx = index.missing,
                           zero.inflated = zero.inflated
  )
  
  # Step 3: Calculate N_t for each triangle sample {N_t : t=Tactual-Dmax+1,...Tactual}
  tibble.samples <- lapply( X = vector.samples,
                            FUN = gg,
                            dados = dadosRio.ag, 
                            idx = index.missing,
                            Fim.sat = Fim, 
                            Dmax = Dm
  )
  
  # Nowcasting
  srag.pred <- bind_rows(tibble.samples, .id = "sample")
  
  srag.pred
}


#######################
# hess.min <- -1
# h.value <- 0.01
# trials <- 0
# while (hess.min <= 0 & trials < 50){
#   output <- inla(model, family = "nbinomial", data = delay.inla.trian,
#                  control.predictor = list(link = 1, compute = T),
#                  control.compute = list( config = T, waic=TRUE, dic=TRUE),
#                  control.family = list( 
#                    hyper = list("theta" = list(prior = "loggamma", param = c(0.1, 0.1)))
#                  ),
#                  control.inla = list(h = h.value)
#   )
#   hess.start <- which(output$logfile == 'Eigenvalues of the Hessian')
#   hess.min <- min(as.numeric(output$logfile[(hess.start+1):(hess.start+3)]))
#   h.value <- h.value + 0.01
#   trials <- trials + 1
# }
# print(paste('Hessian trials:',trials))
# 
######################


# Running INLA for the nowcasting model
nowcast.INLA <- function(dados.ag, model.day, zero.inflated = TRUE, ...){
  hess.min <- -1
  h.value <- 0.01
  h.trials <- 0.001
  trials <- 0
  
  if (zero.inflated){
    family = "zeroinflatednbinomial1"
    control.family = list( 
      hyper = list("theta1" = list(prior = "loggamma", param = c(0.01, 0.01)),
                   "theta2" = list(prior = "gaussian", param = c(0, 0.4)))
    )
  } else {
    family = 'nbinomial'
    control.family = list( 
                         hyper = list("theta" = list(prior = "loggamma", param = c(0.01, 0.01)))
                       )
  }
  while (hess.min <= 0 & trials < 50){
    output <- inla(formula = model.day, 
                 family = family, 
                 data = dados.ag,
                 num.threads = 6,
                 control.predictor = list(link = 1, compute = T),
                 control.compute = list(config = T, waic=F, dic=F, openmp.strategy='huge'),
                 control.family = control.family,
                 control.inla = list(h = h.value),
                 ...
                 # control.family = list( 
                 # hyper = list("theta" = list(
                 #   prior = "loggamma", param = c(1, 0.1)))
                 #   )
    )
    hess.start <- which(output$logfile == 'Eigenvalues of the Hessian')
    hess.min <- min(as.numeric(output$logfile[(hess.start+1):(hess.start+3)]))
    h.value <- h.trials + 0.001
    h.trials <- h.value
    trials <- trials + 1
  }
  output
}


# Plot nowcasting
plot.nowcast <- function(pred.summy, Fim, nowcast = T){
  
  if(!nowcast){
    # Time series
    p0.day <- pred.summy %>% 
      ggplot(aes(x =  Date, y = Casos, 
                 color = "Casos notificados", 
                 linetype = "Casos notificados")) + 
      geom_line(size = 1, na.rm = T) 
  } else {
    p0.day <- pred.summy %>% 
      ggplot(aes(x =  Date, y = Casos.cut, 
                 color = "Casos notificados", 
                 linetype = "Casos notificados")) + 
      geom_line(size = 1, na.rm = T) +
      geom_ribbon( aes( ymin=IC90I, ymax=IC90S), fill = 'gray', 
                   color = 'gray', alpha = 0.5, 
                   show.legend = F) + 
      geom_ribbon( aes( ymin=Q1, ymax=Q3), fill = 'gray', 
                   color = 'gray', alpha = 0.75, 
                   show.legend = F) + 
      geom_line(aes(x = Date, y = Median, 
                    colour = "Casos estimados", 
                    linetype = "Casos estimados"), 
                size = 1, na.rm = T) +
      geom_line(aes(x=Date, y=rolling_average,
                    colour='Média móvel',
                    linetype='Média móvel'), size=1) +
      scale_colour_manual(name = "", 
                          values = c("black", "black", 'blue'), 
                          guide = guide_legend(reverse=F)) +
      scale_linetype_manual(name = "", 
                            values = c("dotted", "solid", 'solid'), 
                            guide = guide_legend(reverse=F))
  }
  
  p0.day <- p0.day + 
    #ylab("Casos hospitalização de SRAG") + 
    #xlab("Tempo") +
    theme_bw( base_size = 14) +
    theme( legend.position = c(0.2, 0.8), legend.title = element_blank()) 
  
  p0.day
}

# Nowcasting_age ----------------
nowcasting_age <- function(dados.age, n.samples=1000, ...){
  
  index.missing <- which(is.na(dados.age$Y))
  
  dados.age <- dados.age %>% 
    mutate(fx_etaria.num = as.numeric(fx_etaria))
  
  # Model equation: intercept + (time random effect) + (Delay random effect)
  model <- Y ~ 1 + fx_etaria +  
    f(Time, 
      model = "rw2", 
      hyper = list("prec" = list(prior = "loggamma", 
                                 param = c(0.001, 0.001))
      ),
      group = fx_etaria.num, control.group = list(model = "iid")) + 
    # f(delay, model = "rw1", 
    #   hyper = list("prec" = list(prior = "loggamma", 
    #                              param = c(0.001, 0.001)))
    # )
  # Age-Delay effects
  f(delay, model = "rw1",
    hyper = list("prec" = list(prior = "loggamma",
                               param = c(0.001, 0.001))),
    group = fx_etaria.num, control.group = list(model = "iid"))
  
  hess.min <- -1
  h.value <- 0.01
  h.trials <- 0.001
  trials <- 0
  while (hess.min <= 0 & trials < 50){
    
    # Running the Negative Binomial model in INLA
    output0 <- inla(model, family = "nbinomial", data = dados.age,
                    control.predictor = list(link = 1, compute = T),
                    control.compute = list(config = T, waic=F, dic=F, openmp.strategy='huge'),
                    control.family = list(
                      hyper = list("theta" = list(prior = "loggamma", 
                                                  param = c(0.001, 0.001))
                      )
                    ),
                    num.threads = 6,
                    control.inla = list(h = h.value),
                    ...
                    
    )
    hess.start <- which(output0$logfile == 'Eigenvalues of the Hessian')
    hess.min <- min(as.numeric(output0$logfile[(hess.start+1):(hess.start+3)]))
    h.value <- h.trials + 0.001
    h.trials <- h.value
    trials <- trials + 1
    
  }  
  #plot(output0)
  
  ## Fixed effects 
  #output0$summary.fixed
  
  ## Hyperparameters (negative binomial parameter, random effects precisions)
  # output0$summary.hyperpar
  
  
  
  # Algorithm to get samples for the predictive distribution for the number of cases
  
  # Step 1: Sampling from the approximate posterior distribution using INLA
  srag.samples0.list <- inla.posterior.sample(n = n.samples, output0)
  
  
  # Step 2: Sampling the missing triangule from the likelihood using INLA estimates
  vector.samples0 <- lapply(X = srag.samples0.list, 
                            FUN = function(x, idx = index.missing){
                              rnbinom(n = idx, 
                                      mu = exp(x$latent[idx]), 
                                      size = x$hyperpar[1]
                              )
                            } ) 
  
  
  #####
  # Step 3: Calculate N_{a,t} for each triangle sample {N_{t,a} : t=Tactual-Dmax+1,...Tactual}
  
  # Auxiliar function selecionando um pedaco do dataset
  gg.age <- function(x, dados.gg, idx){
    data.aux <- dados.gg
    fx_etaria.zero.cases <- dados.gg %>%
      group_by(fx_etaria.num) %>%
      dplyr::summarise(n=sum(Y, na.rm=T)) %>%
      ungroup()
    fx_etaria.zero.cases <- fx_etaria.zero.cases$fx_etaria.num[fx_etaria.zero.cases$n==0]
    Tmin <- min(dados.gg$Time[idx])
    data.aux$Y[idx] <- x
    data.aggregated <- data.aux %>%
      # Selecionando apenas os dias faltantes a partir
      # do domingo da respectiva ultima epiweek
      # com dados faltantes
      filter(Time >= Tmin  ) %>%
      mutate(Y = ifelse(fx_etaria.num %in% fx_etaria.zero.cases, 0, Y)) %>%
      group_by(Time, dt_event, fx_etaria, fx_etaria.num) %>% 
      dplyr::summarise( 
        Y = sum(Y), .groups = "keep"
      )
    data.aggregated
  }
  
  tibble.samples.0 <- lapply( X = vector.samples0,
                              FUN = gg.age,
                              dados = dados.age, 
                              idx = index.missing)
  
  srag.pred.0 <- bind_rows(tibble.samples.0, .id = "sample")
  
  srag.pred.0
  
}

# nowcasting.summary -----------------------
nowcasting.summary <- function(trajetoria, age = F){
  # Trajetoria tem as colunas: sample, Time, dt_event, Y
  # Se age = T tb terá as colunas fx_etaria e fx_etaria.num
  
  total.samples <- trajetoria %>% 
    group_by(Time, dt_event, sample) %>%
    summarise(Y = sum(Y))
  total.summy <- total.samples %>%
    group_by(Time, dt_event) %>%
    summarise(      Median = replace_na(round(median(Y, na.rm=T)), 0), 
                    Q1 = round(quantile(Y, probs = 0.25, na.rm=T)),
                    Q3 = round(quantile(Y, probs = 0.75, na.rm=T)),
                    IC80I = round(quantile(Y, probs = 0.1, na.rm=T)),
                    IC80S = round(quantile(Y, probs = 0.9, na.rm=T)),
                    IC90I = round(quantile(Y, probs = 0.05, na.rm=T)),
                    IC90S = round(quantile(Y, probs = 0.95, na.rm=T)),
                    LI = round(quantile(Y, probs = 0.025, na.rm=T)),
                    LS = round(quantile(Y, probs = 0.975, na.rm=T))
                    ,
              .groups = "drop")
  if(age){
    age.summy <- trajetoria %>%
      group_by(Time, dt_event, fx_etaria, fx_etaria.num) %>%
      summarise(      Median = replace_na(round(median(Y, na.rm=T)), 0), 
                      Q1 = round(quantile(Y, probs = 0.25, na.rm=T)),
                      Q3 = round(quantile(Y, probs = 0.75, na.rm=T)),
                      IC80I = round(quantile(Y, probs = 0.1, na.rm=T)),
                      IC80S = round(quantile(Y, probs = 0.9, na.rm=T)),
                      IC90I = round(quantile(Y, probs = 0.05, na.rm=T)),
                      IC90S = round(quantile(Y, probs = 0.95, na.rm=T)),
                      LI = round(quantile(Y, probs = 0.025, na.rm=T)),
                      LS = round(quantile(Y, probs = 0.975, na.rm=T))
                      ,
                .groups = "drop")
    
    output <- list()
    output$total <- total.summy
    output$total.samples <- total.samples
    output$age <- age.summy
    
  }else{
    output = total.summy
  }
  
  
  output
}
