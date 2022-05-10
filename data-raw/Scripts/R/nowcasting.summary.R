#' Title
#'
#' @param trajetoria 
#' @param age 
#'
#' @return
#' @export
#'
#' @examples
nowcasting.summary <- function(trajetoria, age = F){
  ## Trajetoria tem as colunas: sample, Time, dt_event, Y
  ## Se age = T tb tera¡ as colunas fx_etaria e fx_etaria.num
  
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