library(tidyverse)
library(lubridate)
library(INLA)

## Auxiliar functions for nowcasting 


dados.atuais <- function(dados, epiweek = T, 
                         obitos = T, Release = NULL,
                         age = F){
    if(!is.null(Release)){
        if(is.Date(Release) ){
            if(any(ymd(dados$data) == Release)){
                dados <- dados %>% 
                    filter(ymd(data) == Release)
            }else{
                stop("Release is not a release date. See unique(dados$data).")
            }
        }else{
            stop("Relase is not in a date format!")
        }
    }
    
                                        # Ultima atualizacao
    DATA_last_update <- max(ymd(dados$data)) 
    
                                        # Filtrando os dados mais atuais
    dados <- filter(dados, ymd(data) == DATA_last_update)
    
    
    
                                        # rename: dt_event
    if(obitos){
        dados <- rename(dados, dt_event = dt_evoluca)
    }else{
        dados <- rename(dados, dt_event = dt_sin_pri)
    }
    
    dados <- mutate(dados, dt_event = ymd(dt_event))
    
    if(age){
        dados <- dados %>% 
            rename( fx_etaria = age_class) %>% 
            mutate(
                fx_etaria = substr(fx_etaria, start = 5, stop = 5)
            )
        
        if(epiweek){
            dados <- dados %>% 
                mutate(
                    dt_event_ew = epiweek(dt_event),
                    dt_event_ey = epiyear(dt_event),
                    dt_event = dt_event - as.numeric(format(dt_event, "%w"))
                ) %>% group_by(dt_event, dt_event_ew, dt_event_ey, fx_etaria) %>% 
                summarise(n = sum(n), .groups = "drop")
        }
        
    }else{
        if(epiweek){
            dados <- dados %>% 
                mutate(
                    dt_event_ew = epiweek(dt_event),
                    dt_event_ey = epiyear(dt_event),
                    dt_event = dt_event - as.numeric(format(dt_event, "%w"))
                ) %>% group_by(dt_event, dt_event_ew, dt_event_ey) %>% 
                summarise(n = sum(n), .groups = "drop")
        }
    }
    
    dados
}


                                        # Agrega por semana epidemiologica (Nowcasting diario ainda nao)
                                        # Dmax - atraso maximo considerado no modelo
                                        # Window - janela de dados a ser utilizado
prepara.dados.INLA <- function( dados, Dmax,
                               Window = NULL, K = 0, 
                               obitos = T, Release = NULL ){
    if(!is.null(Release)){
        if(is.Date(Release) ){
            if(any(ymd(dados$data) == Release)){
                dados <- dados %>% 
                    filter(ymd(data) <= Release)
            }else{
                stop("Release is not a release date. See unique(dados$data).")
            }
        }else{
            stop("Relase is not in a date format!")
        }
    }
    
    
    
    ## rename: dt_event
    if(obitos){
        dados <- rename(dados, dt_event = dt_evoluca)
    }else{
        dados <- rename(dados, dt_event = dt_sin_pri)
    }
    
    dados <- dados %>% 
        ##rename(dt_release = data) %>% 
        mutate(
            dt_release = ymd(data),
            dt_event = ymd(dt_event),
            ##delay = dt_release - dt_event,
            dt_event = dt_event - as.numeric(format(dt_event, "%w")),
            ##dt_release = dt_release - as.numeric(format(dt_release, "%w"))
            ) %>% 
        group_by(dt_release, dt_event) %>% 
        summarise(n = sum(n), .groups = "drop") %>% 
        ## Esse passo é necessario pois houve um periodo de dados diarios
        mutate(
            dt_release = dt_release - as.numeric(format(dt_release, "%w"))
        ) %>%
        group_by(dt_release, dt_event) %>% 
        ## Minimo pois os dados sao lancados segunda
        summarise(n = min(n), .groups = "drop") %>% 
        ## Caluclando o atraso em semanas
        mutate(
            delay = as.integer(dt_release - dt_event)/7
        ) %>% #View()
        arrange(dt_event, delay) %>%
        group_by(dt_event) %>%
        mutate(
            Y = diff(c(0,n))
        ) 
    
    Tmax <- max(dados$dt_release)
    Tmin <- min(dados$dt_release)
    if(!is.null(Window)){
        Tmin <- max( Tmin, Tmax - Window * 7)
    }
    
    Tforecast = Tmax + 7 * K
    
    tbl.delay.NA <- 
        tibble(dt_event = seq(Tmin, Tforecast, by = "weeks")) %>% 
        rowid_to_column(var = "Time")
    
    dados <- dados %>%
        select(dt_event, delay, Y) %>% 
        filter(delay <= Dmax) %>% 
        ## Completando a coluna de delays
        full_join( tibble(delay = 0:Dmax), by = "delay" ) %>% 
        spread(key = delay, value = Y) %>% 
        full_join(tbl.delay.NA, by = c("dt_event")) %>% #View() 
        filter(dt_event >= Tmin)
    
    dados <- dados %>% 
        gather(
            key = "delay", 
            value = "Y", -Time, -dt_event) %>% 
        ##   Preenchendo 0 para os valores desconhecidos e 
        ## mantendo NA para onde sera feito a previsao
        mutate(
            delay = as.integer(delay),
            Y = ifelse(is.na(Y) & 
                       as.numeric(Tmax - dt_event)/7 >= delay,
                       0, Y),
            ## Caso tenha alguma diferenca negativa (é possivel)
            Y = ifelse( Y < 0, 0, Y)
        )
    
    ## Assumindo que todo caso tem pelo menos 1 semana de atraso
    dados <- dados %>% 
        mutate(
            delay = ifelse(delay == 0, 1, delay)
        ) %>% group_by(dt_event, Time, delay) %>% 
        summarise(Y = sum(Y), .groups = "drop") 
    
    ## ## Gambiarra para garantir estabilidade numerica
    ## ## Estou adicionando alguns casos qdo observamos zero em todo o periodo
    ## if(!is.null(Window)){
    ##   for(k in 1:3){
    ##     if(all(dados[dados$delay == k,]$Y[1:(Window-k+1)] == rep(0,Window-k+1) )){
    ##       dados[dados$delay == k,]$Y[sample(x = 1:(Window-k+1), size = 4-k, replace = F)] <- 1
    ##     }
    ##   }
    ## }
    
    dados
}



## Agrega por semana epidemiologica (Nowcasting diario ainda nao)
## Dmax - atraso maximo considerado no modelo
## Window - janela de dados a ser utilizado
prepara.dados.INLA.2 <- function( dados, Dmax,
                                 Window = NULL, K = 0, 
                                 obitos = T, Release = NULL,  Tmax){
    if(!is.null(Release)){
        if(is.Date(Release) ){
            if(any(ymd(dados$data) == Release)){
                dados <- dados %>% 
                    filter(ymd(data) <= Release)
            }else{
                stop("Release is not a release date. See unique(dados$data).")
            }
        }else{
            stop("Relase is not in a date format!")
        }
    }
    
    
    
    ## rename: dt_event
    if(obitos){
        dados <- rename(dados, dt_event = dt_evoluca)
    }else{
        dados <- rename(dados, dt_event = dt_sin_pri)
    }
    
    dados <- dados %>% 
        ##rename(dt_release = data) %>% 
        mutate(
            dt_release = ymd(data),
            dt_event = ymd(dt_event),
            ##delay = dt_release - dt_event,
            dt_event = dt_event - as.numeric(format(dt_event, "%w")),
            ##dt_release = dt_release - as.numeric(format(dt_release, "%w"))
            ) %>% 
        group_by(dt_release, dt_event) %>% 
        summarise(n = sum(n), .groups = "drop") %>% 
        ## ## Esse passo é necessario pois houve um periodo de dados diarios
        ## mutate(
        ##   dt_release = dt_release - as.numeric(format(dt_release, "%w"))
        ## ) %>%
        group_by(dt_release, dt_event) %>% 
        ## Minimo pois os dados sao lancados segunda
        summarise(n = min(n), .groups = "drop") %>% 
        ## Caluclando o atraso em semanas
        mutate(
            delay = as.numeric(dt_release - dt_event)/7
        ) %>% ##View()
        arrange(dt_event, delay) %>%
        group_by(dt_event) %>%
        mutate(
            Y = diff(c(0,n))
        ) 
    
    Tmax <- Tmax - as.numeric(format(Tmax, "%w")) - 7
    Tmin <- min( dados$dt_release - as.numeric(format(dados$dt_release, "%w")))
    if(!is.null(Window)){
        Tmin <- max( Tmin, Tmax - Window * 7)
    }
    
    Tforecast = Tmax + 7 * K
    
    tbl.delay.NA <- 
        tibble(dt_event = seq(Tmin, Tforecast, by = "weeks")) %>% 
        rowid_to_column(var = "Time")
    
    tbl.triangle.now <- expand_grid(dt_event = seq(Tmax-7*Dmax, Tforecast, by="weeks"), 
                                    delay = seq(1, Dmax)
                                    ) %>% 
        filter(dt_event + 7*delay > Tmax) %>% 
        bind_cols(Y = NA) 

    tbl.fill.zeroes <- expand_grid(dt_event = seq(Tmin,Tmax-8*Dmax, by="weeks"), 
                                   delay = seq(1, Dmax)
                                   ) %>% bind_cols(Y = 0) 
    
    
    dados <- dados %>%
        select(dt_event, delay, Y) %>% 
        filter(delay <= Dmax+2.5, dt_event >= Tmin, dt_event <= Tmax) %>% 
        bind_rows(tbl.triangle.now) %>% 
        bind_rows(tbl.fill.zeroes) %>% 
        mutate(delay.f = floor(delay)) %>% 
        left_join(tbl.delay.NA, by = c("dt_event")) %>% 
        group_by(dt_event, delay.f) %>%
        summarise(
            delay = delay[1],
            Y = Y[1],
            Time = Time[1],
            .groups = "drop"
        ) %>% ungroup() %>% 
        mutate(
            ## Caso tenha alguma diferenca negativa (é possivel)
            Y = ifelse( Y < 0, 0, Y)
        ) ##%>% View()
    
    ## ## Assumindo que todo caso tem pelo menos 1 semana de atraso
    dados <- dados %>%
        mutate(
            delay = ifelse(delay < 1, 1, delay)
        ) %>% group_by(dt_event, Time, delay) %>%
        summarise(Y = sum(Y), .groups = "drop")
    
    ## ## Gambiarra para garantir estabilidade numerica
    ## ## Estou adicionando alguns casos qdo observamos zero em todo o periodo
    ## if(!is.null(Window)){
    ##   for(k in 1:3){
    ##     if(all(dados[dados$delay == k,]$Y[1:(Window-k+1)] == rep(0,Window-k+1) )){
    ##       dados[dados$delay == k,]$Y[sample(x = 1:(Window-k+1), size = 4-k, replace = F)] <- 1
    ##     }
    ##   }
    ## }
    
    dados
}


## Auxiliar functions for nowcasting

## Auxiliar function, sampling from a negative binomial likelihood
ff <- function(x, idx){
    rnbinom(n = idx, mu = exp(x$latent[idx]), size = x$hyperpar[1])
}

## Auxiliar function selecionando um pedaco do dataset
gg <- function(x, dados.gg, idx){
    data.aux <- dados.gg
    Tmin <- min(dados.gg$Time[idx])
    data.aux$Y[idx] <- x
    data.aggregated <- data.aux %>%
        ## Selecionando apenas os dias faltantes a partir
        ## do domingo da respectiva ultima epiweek
        ## com dados faltantes
        filter(Time >= Tmin  ) %>%
        group_by(Time, dt_event) %>% 
        dplyr::summarise( 
                   Y = sum(Y), .groups = "keep"
               )
    data.aggregated
}


## Algorithm to get samples for the predictive distribution for the number of cases

nowcasting <- function(output, dados.ag, MC.samples = 1000){
    
    index.missing = which(is.na(dados.ag$Y))
    
    
    ## Step 1: Sampling from the approximate posterior distribution using INLA
    samples.list <- inla.posterior.sample(n = MC.samples, output)
    
    ## Step 2: Sampling the missing triangle (in vector form) from the likelihood using INLA estimates
    vector.samples <- lapply(X = samples.list, 
                             FUN = ff,
                             idx = index.missing
                             )
    
    
    ## Step 3: Calculate N_t for each triangle sample {N_t : t=Tactual-Dmax+1,...Tactual}
    tibble.samples <- lapply( X = vector.samples,
                             FUN = gg,
                             dados.gg = dados.ag, 
                             idx = index.missing
                             )
    
    ## Nowcasting
    pred <- bind_rows(tibble.samples, .id = "sample")
    
    pred
}


tbl.nowcasting.summy <- function(dadosINLA, ...){
    
    model.eq <- Y ~ 1 + 
        f( Time, model = "rw1", constr = T,
          ##f( Time, model = "rw2", constr = T,
          ##hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001) ))
          hyper = list("prec" = list(prior = "pc.prec", param = c(2, 0.001) ))
          ) +
        f(delay, model = "rw1", constr = T,
          ## hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001) ))
          hyper = list("prec" = list(prior = "pc.prec", param = c(2, 0.001) ))
          ) ##+
    ## f(TimeDelay, model = "iid",
    ##   ## Checar priori tau ~ gamma(1, 1) sigma in (0.5; 6) 
    ##   ## sqrt(1/qgamma(c(0.025, 0.975),1,1))
    ##   hyper = list("prec" = list(prior = "loggamma", param = c(.1, .01) ))
    ## )
    
    
    
    output <- inla(formula = model.eq, 
                   family = "nbinomial", 
                   data = dadosINLA %>% 
                       mutate(
                           TimeDelay = paste(Time,delay)
                       ),
                   num.threads = 4,
                   control.predictor = list(link = 1, compute = T),
                   control.compute = list( config = T),
                   ...
                   ## control.family = list( 
                   ## hyper = list("theta" = list(
                   ##   prior = "loggamma", param = c(1, 0.1)))
                   ##   )
                   )
    
    
    sample.now <- nowcasting(output = output, dados.ag = dadosINLA )
    
    pred.summy <- sample.now %>% group_by(dt_event) %>% 
        summarise(Median = median(Y),
                  LI = quantile(Y, probs = 0.025),
                  LS = quantile(Y, probs = 0.975),
                  LIb = quantile(Y, probs = 0.25),
                  LSb = quantile(Y, probs = 0.75), 
                  .groups = "drop")
    
    
    total.summy <- sample.now %>% group_by(sample) %>% 
        summarise(Total = sum(Y), .groups = "drop") %>% 
        summarise(Median = median(Total),
                  LI = quantile(Total, probs = 0.025),
                  LS = quantile(Total, probs = 0.975),
                  .groups = "drop")
    
    list(pred = pred.summy, total = total.summy)
}



ggplot.nowcasting <- function(dados.summy, Dmax = NULL){
    
    if(is.null(Dmax)){
        Dmax <- sum(!is.na(dados.summy$Median))
    }
    
    n <- nrow(dados.summy)
    
    dados.summy$Median[n-Dmax] <- dados.summy$n[n-Dmax]
    
    Tmax.plot <- max(dados.summy$dt_event[!is.na(dados.summy$Median)])
    dados.summy <- dados.summy %>% 
        filter(dt_event <= Tmax.plot)
    
    p <- dados.summy %>% 
        ggplot( aes(x = dt_event, y = n)) + 
        geom_line() + theme_bw()
    
    p +
        geom_ribbon(data = dados.summy %>% drop_na(LI), 
                    mapping = aes(x = dt_event, y = Median, ymin = LI, ymax = LS), 
                    fill = "red", alpha = 0.2) + 
        geom_ribbon(data = dados.summy %>% drop_na(LI),
                    mapping = aes(x = dt_event, y = Median, ymin = LIb, ymax = LSb), 
                    fill = "red", alpha = 0.3) + 
        geom_line(data = dados.summy %>% drop_na(Median),
                  mapping = aes(x = dt_event, y = Median), 
                  color = "red") 
    ##geom_line(data = dados.cur %>% filter(dt_event <= Date_Release), color = "brown") +
}

cria.tweet.1 <- function(totalBR){
    totalBR[,-1] <- round(totalBR[,-1])
    cat("SRAG-COVID \n \n")
    
    l1 <- paste0("Óbitos: ", 
                 totalBR[[4,4]], " [", totalBR[[4,5]],"; ",totalBR[[4,6]],"]")
    l2 <- paste0("Hospitalizações: ", 
                 totalBR[[2,4]], " [", totalBR[[2,5]],"; ",totalBR[[2,6]],"]")
    
    cat(l1,"\n")
    cat(l2,"\n \n")
    
    cat("SRAG (contém COVID-19) \n \n")
    
    l3 <- paste0("Óbitos: ", 
                 totalBR[[3,4]], " [", totalBR[[3,5]],"; ",totalBR[[3,6]],"]")
    l4 <- paste0("Hospitalizações: ", 
                 totalBR[[1,4]], " [", totalBR[[1,5]],"; ",totalBR[[1,6]],"]")
    
    cat(l3,"\n")
    cat(l4,"\n \n")
    
    cat("Linha preta: valores observados \n")
    cat("Em vermelho: Nowcasting")
}


leia_dadosobs_age <- function(str, temp = "~/Git/nowcast_covid/Data/Age/"){
    url0 <- "https://raw.githubusercontent.com/covid19br/central_covid/master/dados_processados/integridade_SIVEP/"
    
    url1 <- paste0(url0,str)
    
    file1 <- paste0(temp,str)
    
    download.file(url = url1, destfile = file1)
    
    a <- read.csv(file = file1)
    
    a <- a %>%
        ## Removendo NAs
        filter(age_class != "age_NA") %>%
        mutate(
            age_class = as.character(age_class),
            ## assumindo age_class = 0 sao as criancas < 1 ano
            age_class = ifelse(age_class == "age_0", "age_1", age_class)
        )
    
    a
}



prepara.dados.age.INLA <- function( dados, agegroup, Dmax,
                                   Window = NULL, K = 0, 
                                   obitos = T, Release = NULL ){
    TMAX = max(ymd(dados$data))
    
    dados.list <- map(agegroup, .f = function(x){
        dados %>% filter(age_class == x) %>% 
            ##prepara.dados.INLA(Dmax, Window, K, obitos)
            prepara.dados.INLA.2(Dmax, Window, K, obitos, Tmax = TMAX)
    })
    
    dados <- bind_rows(dados.list, .id = "fx_etaria")
    
    dados
}


nowcasting_age <- function(dados.age, zeroinflated = F){
    
    index.missing <- which(is.na(dados.age$Y))
    
    dados.age <- dados.age %>% 
        mutate(
            fx_etaria.num = as.numeric(fx_etaria),
            delay.grp = inla.group(delay, n = 20)
        )
    
    ## Model equation: intercept + age + (time random effect | age) + f(Delay random effect | age)
    model <- Y ~ 1 + fx_etaria +  
        f(Time, 
          model = "rw2", 
          hyper = list("prec" = list(prior = "loggamma", 
                                     param = c(0.001, 0.001))
                       ),
          group = fx_etaria.num, control.group = list(model = "iid")) + 
        f(delay.grp, model = "rw2", 
          hyper = list("prec" = list(prior = "loggamma", 
                                     param = c(0.001, 0.001))),
          group = fx_etaria.num, control.group = list(model = "iid")
          )
    
    ## Age-Delay effects
    ## f(delay, model = "rw1", hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001))),
    ##   group = fx_etaria.num, control.group = list(model = "iid"))
    
    if(zeroinflated){
        ## Running the Zero inflated Negative Binomial model in INLA
        output0 <- inla(model, family = "zeroinflatednbinomial0", data = dados.age,
                        control.predictor = list(link = 1, compute = T),
                        control.compute = list( config = T, waic=F, dic=F),
                        control.family = list(
                            hyper = list("theta1" = list(prior = "loggamma", 
                                                         param = c(0.001, 0.001))
                                         ## "theta2" = list(prior = "gaussian", 
                                         ##                 param = c(-1, .1))
                                         )
                        )
                        )
    }else{
        ## Running the Negative Binomial model in INLA
        output0 <- inla(model, family = "nbinomial", data = dados.age,
                        control.predictor = list(link = 1, compute = T),
                        control.compute = list( config = T, waic=F, dic=F),
                        control.family = list(
                            hyper = list("theta" = list(prior = "loggamma", 
                                                        param = c(0.001, 0.001))
                                         )
                        )
                        )
        
    }
    
    ##plot(output0)
    
#### Fixed effects 
    ##output0$summary.fixed
    
#### Hyperparameters (negative binomial parameter, random effects precisions)
    ## output0$summary.hyperpar
    
    
    
    ## Algorithm to get samples for the predictive distribution for the number of cases
    
    ## Step 1: Sampling from the approximate posterior distribution using INLA
    srag.samples0.list <- inla.posterior.sample(n = 1000, output0)
    
    
    ## Step 2: Sampling the missing triangle from the likelihood using INLA estimates

    vector.samples0 <- lapply(X = srag.samples0.list, 
                              FUN = function(x, idx = index.missing){
                                  if(zeroinflated){
                                      unif.log <- as.numeric(runif(idx,0,1) < x$hyperpar[2])
                                  }else{
                                      unif.log = 1
                                  }
                                  rnbinom(n = idx, 
                                          mu = exp(x$latent[idx]), 
                                          size = x$hyperpar[1]
                                          ) * unif.log
                              } ) 
    
    
    
    
    
##########
    ## Step 3: Calculate N_{a,t} for each triangle sample {N_{t,a} : t=Tactual-Dmax+1,...Tactual}
    
    ## Auxiliar function selecionando um pedaco do dataset
    gg.age <- function(x, dados.gg, idx){
        data.aux <- dados.gg
        Tmin <- min(dados.gg$Time[idx])
        data.aux$Y[idx] <- x
        data.aggregated <- data.aux %>%
            ## Selecionando apenas os dias faltantes a partir
            ## do domingo da respectiva ultima epiweek
            ## com dados faltantes
            filter(Time >= Tmin  ) %>%
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


nowcasting.summary <- function(trajetoria, age = F){
    ## Trajetoria tem as colunas: sample, Time, dt_event, Y
    ## Se age = T tb terá as colunas fx_etaria e fx_etaria.num
    
    total.summy <- trajetoria %>% 
        group_by(Time, dt_event, sample) %>%
        summarise(Y = sum(Y, na.rm = T)) %>%
        group_by(Time, dt_event) %>%
        summarise(Median = median(Y, na.rm = T),
                  LI = quantile(Y, probs = 0.025, na.rm = T),
                  LS = quantile(Y, probs = 0.975, na.rm = T),
                  LIb = quantile(Y, probs = 0.25, na.rm = T),
                  LSb = quantile(Y, probs = 0.75, na.rm = T),
                  .groups = "drop")
    if(age){
        age.summy <- trajetoria %>%
            group_by(Time, dt_event, fx_etaria, fx_etaria.num) %>%
            summarise(Median = median(Y, na.rm = T),
                      LI = quantile(Y, probs = 0.025, na.rm = T),
                      LS = quantile(Y, probs = 0.975, na.rm = T),
                      LIb = quantile(Y, probs = 0.25, na.rm = T),
                      LSb = quantile(Y, probs = 0.75, na.rm = T),
                      .groups = "drop")
        
        output <- list()
        output$total <- total.summy
        output$age <- age.summy
        
    }else{
        output = total.summy
    }


    output
}

calcula_total = function(output_nowcasting_age, serie_atual, Dmax){

    ##serie_atual <- tbl.UF.age.h.covid
    
    ## Aux
    dt.without.nowcast <- max(serie_atual$dt_event) - Dmax*7
    fx.txt <- c("Total", paste0((0:7)*10, " - ",(0:7)*10+9), "80+")
    
    ## Total
    
    total.without.nowcast <- serie_atual %>% 
        filter(dt_event <= dt.without.nowcast) %>% 
        summarise(n = sum(n))

    total.with.nowcast <- output_nowcasting_age %>% 
        group_by(sample) %>% 
        summarise(Y = sum(Y)) %>% 
        summarise(
            Mean = mean(Y),
            LI = quantile(Y, probs = 0.025),
            LS = quantile(Y, probs = 0.975)
        )  %>% bind_cols(total.without.nowcast) %>% 
        transmute(
            fx_etaria = "99",
            Mean = Mean + n,
            LI = LI + n,
            LS = LS + n
        )
    
    
    total.age.without.nowcast <- serie_atual %>% 
        filter(dt_event <= dt.without.nowcast) %>% 
        group_by(fx_etaria) %>% 
        summarise(n = sum(n))
    
    total.age.with.nowcast <- output_nowcasting_age %>% 
        group_by(sample, fx_etaria) %>% 
        summarise(Y = sum(Y)) %>% 
        group_by(fx_etaria) %>% 
        summarise(
            Mean = mean(Y),
            LI = quantile(Y, probs = 0.025),
            LS = quantile(Y, probs = 0.975)
        ) %>% 
        left_join( total.age.without.nowcast, by = "fx_etaria") %>% 
        transmute(
            fx_etaria = fx_etaria,
            Mean = Mean + n,
            LI = LI + n,
            LS = LS + n
        )
    
    total.with.nowcast %>% bind_rows(total.age.with.nowcast) %>% 
        mutate(fx_etaria = factor(x = fx_etaria, 
                                  levels = as.character(c(99,1:9)), 
                                  labels = fx.txt)
               )
    
}
