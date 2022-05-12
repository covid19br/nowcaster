# Agrega por semana epidemiologica (Nowcasting diario ainda nao)
# Dmax - atraso maximo considerado no modelo
# Window - janela de dados a ser utilizado
#' Title
#'
#' @param dados 
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
prepara.dados.INLA <- function( dados, Dmax,
                                Window = NULL, K = 0, 
                                obitos = T, Release = NULL ){
  if(!is.null(Release)){
    if(is.Date(Release) ){
      if(any(ymd(dados$data) == Release)){
        dados <- dados %>% 
          filter(ymd(data) <= Release)
      }else{
        stop("Release is not a release date. See unique(dados$data).")
      }
    }else{
      stop("Relase is not in a date format!")
    }
  }
  
  
  
  ## rename: dt_event
  if(obitos){
    dados <- rename(dados, dt_event = dt_evoluca)
  }else{
    dados <- rename(dados, dt_event = dt_sin_pri)
  }
  
  dados <- dados %>% 
    ##rename(dt_release = data) %>% 
    mutate(
      dt_release = ymd(data),
      dt_event = ymd(dt_event),
      ##delay = dt_release - dt_event,
      dt_event = dt_event - as.numeric(format(dt_event, "%w")),
      ##dt_release = dt_release - as.numeric(format(dt_release, "%w"))
    ) %>% 
    group_by(dt_release, dt_event) %>% 
    summarise(n = sum(n), .groups = "drop") %>% 
    ## Esse passo Ã© necessario pois houve um periodo de dados diarios
    mutate(
      dt_release = dt_release - as.numeric(format(dt_release, "%w"))
    ) %>%
    group_by(dt_release, dt_event) %>% 
    ## Minimo pois os dados sao lancados segunda
    summarise(n = min(n), .groups = "drop") %>% 
    ## Caluclando o atraso em semanas
    mutate(
      delay = as.integer(dt_release - dt_event)/7
    ) %>% #View()
    arrange(dt_event, delay) %>%
    group_by(dt_event) %>%
    mutate(
      Y = diff(c(0,n))
    ) 
  
  Tmax <- max(dados$dt_release)
  Tmin <- min(dados$dt_release)
  if(!is.null(Window)){
    Tmin <- max( Tmin, Tmax - Window * 7)
  }
  
  Tforecast = Tmax + 7 * K
  
  tbl.delay.NA <- 
    tibble(dt_event = seq(Tmin, Tforecast, by = "weeks")) %>% 
    rowid_to_column(var = "Time")
  
  dados <- dados %>%
    select(dt_event, delay, Y) %>% 
    filter(delay <= Dmax) %>% 
    ## Completando a coluna de delays
    full_join( tibble(delay = 0:Dmax), by = "delay" ) %>% 
    spread(key = delay, value = Y) %>% 
    full_join(tbl.delay.NA, by = c("dt_event")) %>% #View() 
    filter(dt_event >= Tmin)
  
  dados <- dados %>% 
    gather(
      key = "delay", 
      value = "Y", -Time, -dt_event) %>% 
    ##   Preenchendo 0 para os valores desconhecidos e 
    ## mantendo NA para onde sera feito a previsao
    mutate(
      delay = as.integer(delay),
      Y = ifelse(is.na(Y) & 
                   as.numeric(Tmax - dt_event)/7 >= delay,
                 0, Y),
      ## Caso tenha alguma diferenca negativa (Ã© possivel)
      Y = ifelse( Y < 0, 0, Y)
    )
  
  ## Assumindo que todo caso tem pelo menos 1 semana de atraso
  dados <- dados %>% 
    mutate(
      delay = ifelse(delay == 0, 1, delay)
    ) %>% group_by(dt_event, Time, delay) %>% 
    summarise(Y = sum(Y), .groups = "drop") 
  
  ## ## Gambiarra para garantir estabilidade numerica
  ## ## Estou adicionando alguns casos qdo observamos zero em todo o periodo
  ## if(!is.null(Window)){
  ##   for(k in 1:3){
  ##     if(all(dados[dados$delay == k,]$Y[1:(Window-k+1)] == rep(0,Window-k+1) )){
  ##       dados[dados$delay == k,]$Y[sample(x = 1:(Window-k+1), size = 4-k, replace = F)] <- 1
  ##     }
  ##   }
  ## }
  
  dados
}
