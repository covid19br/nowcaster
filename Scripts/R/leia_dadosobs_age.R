#' Title
#'
#' @param str 
#' @param temp 
#'
#' @return
#' @export
#'
#' @examples
leia_dadosobs_age <- function(str, temp = "~/Git/nowcast_covid/Data/Age/"){
  url0 <- "https://raw.githubusercontent.com/covid19br/central_covid/master/dados_processados/integridade_SIVEP/"
  
  url1 <- paste0(url0,str)
  
  file1 <- paste0(temp,str)
  
  download.file(url = url1, destfile = file1)
  
  a <- read.csv(file = file1)
  
  a <- a %>%
    ## Removendo NAs
    filter(age_class != "age_NA") %>%
    mutate(
      age_class = as.character(age_class),
      ## assumindo age_class = 0 sao as criancas < 1 ano
      age_class = ifelse(age_class == "age_0", "age_1", age_class)
    )
  
  a
}