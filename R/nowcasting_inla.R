# Nowcasting SRAG by age group
# Leo Bastos


#' nowcasting_inla, function to estimate amount of events already started by not yet notified. 
#' The main use is to estimate how many cases in a outbreak has already started their onset date of symptons but has not yet notified.
#' nowcasting_inla, fits a statistical distribution to the empirical distribution of time of delay between onset date and report date.
#'
#' @param boletim # Data base to be used. [example] SIVEP-Gripe.
#' @param trim.data # (in weeks) Date to be trimmed out from the data base, in weeks. [Default] 2 weeks.
#' @param Dmax (in weeks) Until which maximum amount of weeks the Nowcasting will use for the estimation. [Default] 15 weeks.
#' @param wdw (in weeks) Window of dates the estimation will act, i.e., how many past weeks the nowcasting will estimates.[Default] 30 weeks. 
#' @param data.by.week [Optinal] If it has to be returned the whole time-series data. [Default] FALSE.
#' @param return.age [Optinal] If the estimate by Age should be returned. [Default] TRUE. 
#' @param bins_age [Optinal] Age bins to do the nowcasting, it receive a vector of age bins, or options between, "SI-PNI", "10 years", "5 years". [Default] "SI-PNI".
#' @param ... 
#' #' The estimation for the total is done by taking the age estimation all together.
#'
#' @return a list of 2 elements, each element with a data.frame with nowcasting estimation, $[1] 'Total', $[2] by 'Age'. 
#' If data.by.week = TRUE, add a $[3] 'dados' with the time-series out of wdw.
#' @export 'tidyverse'; 'lubridate'; 'vroom'; 'INLA'
#'
#' @examples 
nowcasting_inla <- function(dataset, 
                            bins_age="SI-PNI", 
                            trim.data = 2, 
                            Dmax = 15, 
                            wdw = 30, 
                            data.by.week = FALSE, 
                            return.age = T, 
                            silent = T,
                            ...){
  
  ## Loading required packages
  require(tidyverse)
  require(lubridate)
  require(vroom)
  require(INLA)
  
  ## Safe tests
  if(missing(dataset)){
    stop("Data base is missing!")
  }
  
  if(!silent){
    if(missing(bins_age)){
      warning("Using 'SI-PNI' age bins!")
    }
    
    if(missing(trim.data)){
      warning("Using default to trim dates, trim.data = 2")
    }
    
    if(missing(Dmax)){
      warning("Using default to maximum delay, Dmax = 15")
    }
    
    if(missing(wdw)){
      warning("Using default to window of action, wdw = 30")
    }
    
    if(missing(data.by.week)){
      warning("Using default to returning option for the data, data.by.week = FALSE")
    }
    
    if(missing(return.age)){
      warning("Using default to returning estimate by age, return.age = T")
    }
  }
  

  
  ## Objects for keep the nowcasting
  ## Filtering out cases without report date
  dados <- dataset %>% 
    mutate(IDADE = NU_IDADE_N) %>% 
    select(DT_DIGITA, DT_SIN_PRI, IDADE) %>% 
    drop_na(DT_DIGITA)
  
  ## Filtering data to the parameters setted above
  dados_w<-dados.w(dados, 
                   bins_age = bins_age, 
                   trim.data = trim.data)
  
  ## Parameters of Nowcasting estimate
  # Dmax <- 15
  # wdw <- 30
  Tmax <- max(dados_w$DT_SIN_PRI)
  
  ## Parameter of stratum
  # fx.txt <- c("0 - 4", "5 - 11", "12 - 17", "18 - 29",
  #             "30 - 49", "50 - 59", "60 - 69", 
  #             "70 - 79", "80 +")
  
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
  dados.inla <- dados.inla %>% 
    left_join(tbl.date.aux) 
  
  ## Time maximum to be considered
  Tmax.id <- max(dados.inla$Time)
  
  # Auxiliary date table on each stratum, By age
  tbl.NA <- expand.grid(Time = 1:Tmax.id,
                        delay = 0:Dmax,
                        fx_etaria = unique(dados.inla$fx_etaria)
  ) %>% 
    left_join(tbl.date.aux, by = "Time")
  
  ## Joining the auxiliary date table by Stratum
  dados.inla <- dados.inla %>% 
    full_join(tbl.NA) %>%  #View()
    mutate(
      Y = ifelse(Time + delay > Tmax.id, as.numeric(NA), Y),
      Y = ifelse(is.na(Y) & Time + delay <= Tmax.id, 0, Y ),
    ) %>% 
    arrange(Time, delay, fx_etaria) %>% 
    rename(dt_event = DT_SIN_PRI)
  
  ## Nowcasting estimate
  sample.now <- nowcasting_age(dados.age = dados.inla)
  
  ## Summary on the posteriors of nowcasting
  now_summary<-nowcasting.summary(sample.now, 
                                  age = T)
  
  ## Objects to be returned
  if(data.by.week){
    now_summary[[3]]<-dados_w
    names(now_summary)[3]<-"dados"
    return(now_summary)
  }
  
  ## Returning Age
  if(!return.age){
    now_summary<-now_summary$total
    return(now_summary)
  }
  
  return(now_summary)
  
}

