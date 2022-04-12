#' Title
#'
#' @param dados 
#' @param agegroup 
#' @param Dmax 
#' @param Window 
#' @param K 
#' @param obitos 
#' @param Release 
#'
#' @return
#' @export
#'
#' @examples
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