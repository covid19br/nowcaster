#' Title
#'
#' @param totalBR 
#'
#' @return
#' @export
#'
#' @examples
cria.tweet.1 <- function(totalBR){
  totalBR[,-1] <- round(totalBR[,-1])
  cat("SRAG-COVID \n \n")
  
  l1 <- paste0("Ãbitos: ", 
               totalBR[[4,4]], " [", totalBR[[4,5]],"; ",totalBR[[4,6]],"]")
  l2 <- paste0("HospitalizaÃ§Ãµes: ", 
               totalBR[[2,4]], " [", totalBR[[2,5]],"; ",totalBR[[2,6]],"]")
  
  cat(l1,"\n")
  cat(l2,"\n \n")
  
  cat("SRAG (contÃ©m COVID-19) \n \n")
  
  l3 <- paste0("Ãbitos: ", 
               totalBR[[3,4]], " [", totalBR[[3,5]],"; ",totalBR[[3,6]],"]")
  l4 <- paste0("HospitalizaÃ§Ãµes: ", 
               totalBR[[1,4]], " [", totalBR[[1,5]],"; ",totalBR[[1,6]],"]")
  
  cat(l3,"\n")
  cat(l4,"\n \n")
  
  cat("Linha preta: valores observados \n")
  cat("Em vermelho: Nowcasting")
}