#' Title
#'
#' @param dados 
#' @param trim.data 
#' @param bins_age 
#'
#' @return
#' @export
#'
#' @examples
dados.w<-function(dados, 
                  trim.data=2, 
                  bins_age = c("SI-PNI", "10 years", "5 years")){
  ## Data da ultima digitacão
  if(missing(trim.data)){
    trim.data <-  2
    warning("Using default, trimming 2 days of the data")
  }
  
  ## Data máxima de digitação a considerar
  DT_max <- max(dados$DT_DIGITA-trim.data, na.rm = T)
  
  # Dia da semana da ultima digitação
  DT_max_diadasemana <- as.integer(format(DT_max, "%w"))
  weekdays(DT_max)
  
  ## Test for age bins
  if(missing(bins_age) | is.null(bins_age) | bins_age == "SI-PNI"){
    bins_age<-c(0,4,11,18,30,seq(50,80,by=10),200)
    labels_age<-1:9
    warning("Bins age as in SI-PNI: ", 
            bins_age[bins_age <= 80], 
            call. = T)
  }else{
    if(bins_age == "10 years"){
      bins_age<-seq(0,200, by = 10)
      labels_age<-1:20
      warning("Bins age in 10 years: ", 
              bins_age[bins_age <= 80], 
              call. = T)
    }else if(bins_age){
      bins_age<-seq(0,200, by = 5)
      labels_age<-1:40
      warning("Bins age in 5 years: ", 
              bins_age[bins_age <= 80], 
              call. = T)
    } else {
      if(is.numeric(bins_age)){
        bins_age<-bins_age
        labels_age<-1:length(bins_age)-1
        warning("Using bins ages given: ", 
                bins_age, 
                call. = T)
      }
    }
  }
  
  
  dados.w <- dados %>% 
    filter(DT_DIGITA <= DT_max, epiyear(DT_SIN_PRI) >= 2021) %>%
    drop_na(IDADE) %>% 
    mutate(
      # Alterando a data para o primeiro dia da semana 
      # Ex. se ultimo dado for de um domingo, entao a semana 
      # comeca na 2a anterior, se termina 5a, entao começará 6a
      DT_SIN_PRI = DT_SIN_PRI - 
        as.integer(format(DT_SIN_PRI, "%w")) - 
        (6-DT_max_diadasemana),
      DT_DIGITA = DT_DIGITA - 
        as.integer(format(DT_DIGITA, "%w")) - 
        (6-DT_max_diadasemana),
      Delay = as.numeric(DT_DIGITA - DT_SIN_PRI) / 7,
      fx_etaria = cut(IDADE, 
                      breaks = bins_age, 
                      labels = labels_age,
                      right = F)
    ) %>%
    drop_na(fx_etaria) %>%
    filter(Delay >= 0)
  
  # Returning
  return(dados.w)
}