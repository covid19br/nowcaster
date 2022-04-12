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
dados.w_no_age<-function(dados,
                  trim.data){
  dados_w <- dados %>%
    filter(DT_DIGITA <= DT_max, epiyear(DT_SIN_PRI) >= 2021) %>%
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
      Delay = as.numeric(DT_DIGITA - DT_SIN_PRI) / 7) %>%
    filter(Delay >= 0)

  # Returning
  return(dados_w)
}



