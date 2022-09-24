source("nowcasting_v2.R")
source("slope.estimate.quant.R")

# Função para gerar as estimativas:
generate.estimate <- function(dadosBR,
                              Inicio=Inicio,
                              today.week=today.week,
                              Dmax=10,
                              wdw=40,
                              zero.inflated=TRUE,
                              n.samples = 1000,
                              ...){
  dadosBR <- dadosBR %>% 
    filter(DT_SIN_PRI_epiweek >= 1, DT_SIN_PRI_epiweek <= today.week) %>% 
    mutate(
      DelayWeeks = SinPri2Digita_DelayWeeks,
      DelayWeeks = ifelse(DelayWeeks < 0, NA, DelayWeeks)
    ) 
  
  # Atraso em semanas 
  # Dmax = 10
  trials <- 0
  consistent <- F
  while (trials < 3 & !consistent & Dmax >= 4){
    dados.srag.ag <- dadosBR %>%
      filter(DT_SIN_PRI_epiweek >= today.week - (wdw-1)) %>%
      mutate(
        DelayWeeks = ifelse(DelayWeeks > Dmax, NA, DelayWeeks)
      ) %>% 
      drop_na(DelayWeeks) %>% 
      group_by(DT_SIN_PRI_epiweek, DelayWeeks) %>% 
      dplyr::summarise(
        Casos = n()
      ) %>%  # View()
      # Passando para o formato wide
      spread(key = DelayWeeks, value = Casos) %>% #  View()
      # Adicianoando todas as data, alguns dias nao houveram casos com primeiros sintomas
      # e dias apos "Hoje" serão incluídos para previsão 
      full_join( 
        y = tibble(DT_SIN_PRI_epiweek = seq(epiweek(Inicio), today.week)), 
        by = "DT_SIN_PRI_epiweek" ) %>% # View() 
      # Voltando para o formato longo
      gather(key = DelayWeeks, value = Casos, -DT_SIN_PRI_epiweek) %>% 
      mutate(
        DelayWeeks = as.numeric(DelayWeeks),
        # Preparing the run-off triangle
        Casos = ifelse( 
          test = (DT_SIN_PRI_epiweek + DelayWeeks) <= today.week, 
          yes = replace_na(Casos, 0), 
          no = NA)
      ) %>% dplyr::rename( Date = DT_SIN_PRI_epiweek) %>%  ungroup() %>% 
      # Sorting by date
      dplyr::arrange(Date) %>% 
      # Creating Time and Delay indexes
      mutate( 
        Time = as.numeric(Date - min(Date) + 1)
      ) %>% 
      dplyr::rename( Delay = DelayWeeks)
    
    # Model equation
    model.srag <- Casos ~ 1 + 
      f(Time, model = "rw2", constr = T,
        hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001)))
        #hyper = list("prec" = list(prior = half_normal_sd(.1) ))
      ) +
      f(Delay, model = "rw1", constr = T,
        hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001)))
        #hyper = list("prec" = list(prior = half_normal_sd(.1) ))
      ) #+ 
    # Efeito tempo-atraso
    #f(TimeDelay, model = "iid", constr = T)
    
    output.srag <- nowcast.INLA(
      model.day = model.srag,
      dados.ag = dados.srag.ag %>%
        mutate(TimeDelay = paste(Time, Delay)),
      zero.inflated = zero.inflated,
      ...
    )
    
    dados.srag.ag.day.plot <- dadosBR %>%
      group_by(DT_SIN_PRI_epiweek) %>%
      dplyr::summarise( Casos = n()) %>%
      rename(Date = DT_SIN_PRI_epiweek) %>%
      ungroup() %>%
      right_join(epiweek.table %>% transmute(Date = DT_SIN_PRI_epiweek), by='Date') %>%
      replace_na(list(Casos = 0))
    
    pred.srag <- nowcasting(output.srag, dados.srag.ag, Fim=today.week, Dm=Dmax, zero.inflated=zero.inflated, n.samples=n.samples)
    pred.srag.summy <- pred.srag %>% group_by(Date) %>% 
      dplyr::summarise( #Mean = mean(Casos),
        Median = replace_na(round(median(Casos, na.rm=T)), 0), 
        Q1 = round(quantile(Casos, probs = 0.25, na.rm=T)),
        Q3 = round(quantile(Casos, probs = 0.75, na.rm=T)),
        IC80I = round(quantile(Casos, probs = 0.1, na.rm=T)),
        IC80S = round(quantile(Casos, probs = 0.9, na.rm=T)),
        IC90I = round(quantile(Casos, probs = 0.05, na.rm=T)),
        IC90S = round(quantile(Casos, probs = 0.95, na.rm=T)),
        LI = round(quantile(Casos, probs = 0.025, na.rm=T)),
        LS = round(quantile(Casos, probs = 0.975, na.rm=T))
      ) %>%
      right_join(dados.srag.ag.day.plot, by='Date') %>%
      arrange(Date) %>%
      mutate(full_estimate = case_when(
        is.na(Median) ~ as.numeric(Casos),
        TRUE ~ as.numeric(Median)),
        Casos.cut = case_when(
          Date <= today.week - 2 ~ as.numeric(Casos),
          TRUE ~ NA_real_),
        rolling_average = round(zoo::rollmean(full_estimate, k=3, fill=NA)))
    
    consistent <- pred.srag.summy %>%
      filter(Date == today.week) %>%
      transmute(dispersion = LS/(Median+1) < 20) %>%
      all()
    trials <- trials + 1
    Dmax <- Dmax - 2
    wdw <- round(2.25*Dmax) - 1
  }
  
  # Add previous weeks
  pred.srag.var <-dados.srag.ag.day.plot %>%
    filter(Date < today.week - Dmax)
  slist <- sort(rep(seq(1, n.samples), nrow(pred.srag.var)))
  pred.srag.var <- tibble(sample=slist, Date=rep(as.integer(pred.srag.var$Date), n.samples)) %>%
    left_join(pred.srag.var, by='Date') %>%
    rbind(pred.srag) %>%
    arrange(sample, Date)
  
  # Tendência via modelo linear com janela móvel
  weeks.level <- pred.srag.var$Date %>%
    unique()
  variation.lvl.3s <- weeks.level[weeks.level >= max(pred.srag.var$Date) - 8] %>%
    map(slope.estimate.quant, pred.srag=pred.srag.var, w=3) %>%
    unlist() %>%
    as_tibble_col(column_name = 'tendencia.3s') %>%
    cbind(Date = weeks.level[weeks.level >= max(pred.srag.var$Date) - 8])
  variation.lvl <- weeks.level[weeks.level >= max(pred.srag.var$Date) - 16] %>%
    map(slope.estimate.quant, pred.srag=pred.srag.var, w=6) %>%
    unlist() %>%
    as_tibble_col(column_name = 'tendencia.6s') %>%
    cbind(Date = weeks.level[weeks.level >= max(pred.srag.var$Date) - 16]) %>%
    left_join(variation.lvl.3s, by='Date')
  rm(pred.srag.var)
  
  pred.srag.summy <- pred.srag.summy %>%  
    left_join(variation.lvl, by='Date')
  
  return(pred.srag.summy)
}

generate.slope <- function(dados.srag.ag.day.plot,
                           pred.srag,
                           n.samples=1000,
                           today.week=today.week,
                           Dmax=10){
  
  pred.srag.var <-dados.srag.ag.day.plot %>%
    filter(Date <= today.week - Dmax)
  slist <- sort(rep(seq(1, n.samples), nrow(pred.srag.var)))
  pred.srag.var <- tibble(sample=slist, Date=rep(as.integer(pred.srag.var$Date), n.samples)) %>%
    left_join(pred.srag.var, by='Date') %>%
    rbind(pred.srag) %>%
    arrange(sample, Date)
  
  # Tendência via modelo linear com janela móvel
  weeks.level <- pred.srag.var$Date %>%
    unique()
  variation.lvl.3s <- weeks.level[weeks.level >= max(pred.srag.var$Date) - 8] %>%
    map(slope.estimate.quant, pred.srag=pred.srag.var, w=3) %>%
    unlist() %>%
    as_tibble_col(column_name = 'tendencia.3s') %>%
    cbind(Date = weeks.level[weeks.level >= max(pred.srag.var$Date) - 8])
  variation.lvl <- weeks.level[weeks.level >= max(pred.srag.var$Date) - 16] %>%
    map(slope.estimate.quant, pred.srag=pred.srag.var, w=6) %>%
    unlist() %>%
    as_tibble_col(column_name = 'tendencia.6s') %>%
    cbind(Date = weeks.level[weeks.level >= max(pred.srag.var$Date) - 16]) %>%
    left_join(variation.lvl.3s, by='Date')
  rm(pred.srag.var)
  
  pred.srag.summy <- pred.srag %>% group_by(Date) %>% 
    dplyr::summarise( #Mean = mean(Casos),
      Median = replace_na(round(median(Casos, na.rm=T)), 0), 
      Q1 = round(quantile(Casos, probs = 0.25, na.rm=T)),
      Q3 = round(quantile(Casos, probs = 0.75, na.rm=T)),
      IC80I = round(quantile(Casos, probs = 0.1, na.rm=T)),
      IC80S = round(quantile(Casos, probs = 0.9, na.rm=T)),
      IC90I = round(quantile(Casos, probs = 0.05, na.rm=T)),
      IC90S = round(quantile(Casos, probs = 0.95, na.rm=T)),
      LI = round(quantile(Casos, probs = 0.025, na.rm=T)),
      LS = round(quantile(Casos, probs = 0.975, na.rm=T))
    ) %>%
    mutate(LS = case_when(
      LS > 30 ~ pmin(3*Median, LS),
      TRUE ~ LS
    ),
    IC90S = case_when(
      IC90S > 30 ~ pmin(3*Median, IC90S),
      TRUE ~ LS
    )) %>%
    right_join(dados.srag.ag.day.plot, by='Date') %>%
    arrange(Date) %>%
    mutate(full_estimate = case_when(
      is.na(Median) ~ as.numeric(Casos),
      TRUE ~ as.numeric(Median)),
      Casos.cut = case_when(
        Date <= today.week - 2 ~ Casos,
        TRUE ~ NA_integer_),
      rolling_average = round(zoo::rollmean(full_estimate, k=3, fill=NA))) %>%
    left_join(variation.lvl, by='Date')
  
  return(pred.srag.summy)
  
  
}