#' Actual data
#'
#' @param dados 
#' @param epiweek 
#' @param obitos 
#' @param Release 
#' @param age 
#'
#' @return
#' @export
#'
#' @examples
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
