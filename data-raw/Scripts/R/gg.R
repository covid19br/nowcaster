## Auxiliar function selecionando um pedaco do dataset
#' Title
#'
#' @param x 
#' @param dados.gg 
#' @param idx 
#'
#' @return
#' @export
#'
#' @examples
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