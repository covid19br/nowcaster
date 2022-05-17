#' Dados_w
#'
#' @param dataset dataset to be formatted as data by week
#' @param trim.data How much to trim of the data?
#' @param bins_age Bins of age to cu the data, parsing from nowcasting_inla
#' @param age_col Age column to be where to  cut the data into age classes
#'
#' @return Data in weeks
#' @export
#'
#' @examples
dados.w<-function(dataset,
                  trim.data,
                  bins_age = c("SI-PNI", "10 years", "5 years", bins_age),
                  date_onset,
                  date_report,
                  age_col){
  # Loading packages
  require(tidyr)
  require(dplyr)
  require(lubridate)

  ## Data da ultima digitacão
  if(missing(trim.data)){
    trim.data <-  2
    warning("Using default, trimming out 2 days of the data")
  } else {
    warning("Using default, trimming out ",
                   trim.data ,
                   " days of data",
            call. = T)
  }

  ## Data máxima de digitação a considerar
  DT_max <- max(dataset |>
                  pull(var = {{date_report}}),
                na.rm = T) - trim.data

  # Dia da semana da ultima digitação
  DT_max_diadasemana <- as.integer(format(DT_max, "%w"))

  # ## Test for age bins
  if(!is.numeric(bins_age)){
    if(missing(bins_age) | is.null(bins_age)){
      bins_age<-"SI-PNI"
      warning("Using SI-PNI age bins")
    } else if(bins_age == "SI-PNI"){
        bins_age<-c(0,5,12,18,30,seq(40,90,by=10),130)
        labels_age<-1:(length(bins_age)-1)
        warning("Bins age as in SI-PNI: ",
                str_c(bins_age[bins_age < 90], " "), "90+",
                call. = T)
      } else if(bins_age == "10 years"){
        bins_age<-c(seq(0,90, by = 10),130)
        labels_age<-1:(length(bins_age)-1)
        warning("Bins age in 10 years: ",
                str_c(bins_age[bins_age < 90], " "), "90+",
                call. = T)
      } else if(bins_age == "5 years"){
        bins_age<-c(seq(0,90, by = 5),130)
        labels_age<-1:(length(bins_age)-1)
        warning("Bins age in 5 years: ",
                str_c(bins_age[bins_age < 90], " "), "90+",
                call. = T)
      }
    else {
      stop("Age bins are not options of 'SI-PNI', '10 years', '5 years' or numeric vector!")
    }
  } else {
    bins_age<-bins_age
    labels_age<-1:(length(bins_age)-1)
    warning("Using bins ages given: ",
            str_c(bins_age[bins_age < bins_age[length(bins_age) - 1]], " "),
            bins_age[length(bins_age) - 1], "+",
            call. = T)
  }


  dados_w <- dataset %>%
    rename(date_report = {{date_report}},
           date_onset = {{date_onset}}) %>%
    dplyr::filter(date_report <= DT_max,
                  #lubridate::epiyear(date_onset) >= 2021 &
             {{age_col}} <= max(bins_age)) %>%
    tidyr::drop_na({{age_col}}) %>%
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
      Delay = as.numeric(date_report - date_onset) / 7,
      fx_etaria = cut({{age_col}},
                      breaks = bins_age,
                      labels = labels_age,
                      right = F)
    ) %>%
    tidyr::drop_na(fx_etaria) %>%
    dplyr::filter(Delay >= 0)

  # Returning
  return(dados_w)
}
