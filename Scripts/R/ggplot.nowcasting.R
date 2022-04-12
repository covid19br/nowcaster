#' Title
#'
#' @param dados.summy 
#' @param Dmax 
#'
#' @return
#' @export
#'
#' @examples
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