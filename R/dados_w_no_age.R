#' Title
#'
#' @param dataset dataset to be formatted as data by week
#' @param trim.data How much to trim of the data?
#' @param K How much weeks to forecast ahead?
#' [Default] K is 0, no forecasting ahead
#'
#' @return Data in weeks
#' @export
#'
#' @examples
dados.w_no_age<-function(dataset,
                         trim.data,
                         date_onset,
                         date_report,
                         K){

  # Loading packages
  # require(dplyr)
  # require(lubridate)

  ## Data da ultima digitacão
  if(missing(trim.data)){
    trim.data <-  0
    warning("Using default, no trimming out of the data")
  } else {
    warning("Using default, trimming out ",
            trim.data ,
            " days of data",
            call. = T)
  }

  ## Trim.data
  trim.data.w<-7*trim.data
  ## K
  K.w<-7*K

  ## Data máxima de digitação a considerar
  DT_max <- max(dataset |>
                  dplyr::pull(var = {{date_report}}),
                na.rm = T) - trim.data.w + K.w

  # Dia da semana da ultima digitação
  DT_max_diadasemana <- as.integer(format(DT_max, "%w"))


  dados_w <- dataset |>
    dplyr::rename(date_report = {{date_report}},
           date_onset = {{date_onset}}) |>
    dplyr::filter(date_report <= DT_max,
                  # lubridate::epiyear(date_onset) >= 2021
                  ) |>
    dplyr::mutate(
      # Alterando a data para o primeiro dia da semana
      # Ex. se ultimo dado for de um domingo, entao a semana
      # comeca na 2a anterior, se termina 5a, entao começará 6a
      date_onset = date_onset -
        as.integer(format(date_onset, "%w")) -
        (6-DT_max_diadasemana),
      date_report = date_report -
        as.integer(format(date_report, "%w")) -
        (6-DT_max_diadasemana),
      Delay = as.numeric(date_report - date_onset) / 7) |>
    dplyr::filter(Delay >= 0)

  # Returning
  return(dados_w)
}



