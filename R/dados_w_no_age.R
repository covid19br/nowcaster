#' Title
#'
#' @param dataset dataset to be formatted as data by week
#' @param trim.data How much to trim of the data?
#'
#' @return Data in weeks
#' @export
#'
#' @examples
dados.w_no_age<-function(dataset,
                         trim.data){

  # Loading packages
  require(dplyr)
  require(lubridate)

  ## Data máxima de digitação a considerar
  DT_max <- max(dataset$DT_DIGITA, na.rm = T) - trim.data

  # Dia da semana da ultima digitação
  DT_max_diadasemana <- as.integer(format(DT_max, "%w"))

  dados_w <- dataset %>%
    dplyr::filter(DT_DIGITA <= DT_max, lubridate::epiyear(DT_SIN_PRI) >= 2021) %>%
    dplyr::mutate(
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
    dplyr::filter(Delay >= 0)

  # Returning
  return(dados_w)
}



