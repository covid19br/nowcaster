#' SARI data from Belo Horizonte
#'
#' An anonymized dataset containing cases of severe acute respiratory illness
#' (SARI) from a Brazilian municipality, Belo Horizonte, with symptoms onset varying
#' from 2019-12-29 to 2022-03-27.
#'
#' @format A data frame with 65404 rows and 7 variables:
#' \describe{
#'   \item{DT_SIN_PRI}{Date of onset symptoms.}
#'   \item{DT_DIGITA}{Date of recording.}
#'   \item{CLASSI_FIN}{Final classification of the case. (Not used for nowcasting)}
#'   \item{EVOLUCAO}{Case evolution. (Not used for nowcasting)}
#'   \item{CO_MUN_RES}{IBGE municipality code. Belo Horizonte is 310620.}
#'   \item{Idade}{Age in years.}
#'   \item{fx_etaria}{Age brackets.}
#'   ...
#' }
#' @source \url{https://opendatasus.saude.gov.br/}
"sragBH"
