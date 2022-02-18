# Nowcasting SRAG by age group
# Leo Bastos


nowcasting_inla <- function(boletim, trim.data = 2, Dmax = 15, wdw = 30, data.by.week = FALSE){
  
  ## Loading required packages
  require(tidyverse)
  require(lubridate)
  require(vroom)
  require(INLA)
  
  ## Auxiliary functions
  source("Nowcasting/Scripts/nowcasting_fun.r")
  source("Nowcasting/Scripts/dados_w.R")
  
  ## Objects for keep the nowcasting
  # if(missing(stratum)){
  #   strata.list <- vector(mode = "list", length = 2)
  #   names(strata.list) <- c("total", "idade")
  # }else{
  #   strata.list<-vector("list", 1)
  #   names(strata.list)<-"total"
  # }
  
  ## 
  dados <- boletim %>% 
    mutate(IDADE = NU_IDADE_N) %>% 
    select(DT_DIGITA, DT_SIN_PRI, IDADE) %>% 
    drop_na(DT_DIGITA) 
  
  ## Last reporting date
  trim.data <-  2 ## Discarding last 2 weeks
  ## Maximum report date o consider
  DT_max <- max(dados$DT_DIGITA  - trim.data, na.rm = T)
  
  ## Weekday of last reporting date
  DT_max_diadasemana <- as.integer(format(DT_max, "%w"))
  weekdays(DT_max)
  
  ## Filtering data to the parameters setted above
  dados_w<-dados.w(boletim)
  
  ## Parameters of Nowcasting estimate
  Dmax <- 15
  wdw <- 30
  Tmax <- max(dados_w$DT_SIN_PRI)
  
  ## Parameter of stratum
  fx.txt <- c("0 - 4", "5 - 11", "12 - 17", "18 - 29",
              "30 - 49", "50 - 59", "60 - 69", 
              "70 - 79", "80 +")
  
  ## Data to be entered in Nowcasting function
  ## 
  dados.inla <- dados_w %>% 
    ## Filter for dates
    filter(DT_SIN_PRI >= Tmax - 7 * wdw, Delay <= Dmax) %>% 
    ## Group by on Onset dates, Amounts of delays and Stratum
    group_by(DT_SIN_PRI, delay = Delay, fx_etaria) %>% 
    ## Counting
    tally(name = "Y") 
  
  ## Auxiliary date table
  tbl.date.aux <- tibble(
    DT_SIN_PRI = unique(dados.inla$DT_SIN_PRI)
  ) %>% 
    rowid_to_column(var = "Time")
  
  ## Joining auxiliary date tables
  dados.inla <- dados.inla %>% left_join(tbl.date.aux) 
  
  ## Time maximum to be considered
  Tmax.id <- max(dados.inla$Time)
  
  # Auxiliary date table on each stratum, By age
  tbl.NA <- expand.grid(Time = 1:Tmax.id,
                        delay = 0:Dmax,
                        fx_etaria = unique(dados.inla$fx_etaria)
  ) %>% left_join(tbl.date.aux, by = "Time")
  
  ## Joining the auxiliary date table by Stratum
  dados.inla <- dados.inla %>% full_join(tbl.NA) %>%  #View()
    mutate(
      Y = ifelse(Time + delay > Tmax.id, as.numeric(NA), Y),
      Y = ifelse(is.na(Y) & Time + delay <= Tmax.id, 0, Y ),
    ) %>% arrange(Time, delay, fx_etaria)
  
  
  ## Nowcasting estimate
  sample.now <- nowcasting_age(dados.age = dados.inla %>% 
                                 rename(dt_event = DT_SIN_PRI)
  )
  
  ## Summary on the posteriors of nowcasting
  now_summary<-nowcasting.summary(sample.now, age = T)
  
  ## Objects to be returned
  if(data.by.week){
    now_summary[[3]]<-dados_w
    names(now_summary)[3]<-"dados"
    return(now_summary)
  }
  
  return(now_summary)
  
}

