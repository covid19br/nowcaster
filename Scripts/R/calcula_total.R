#' Title
#'
#' @param output_nowcasting_age 
#' @param serie_atual 
#' @param Dmax 
#'
#' @return
#' @export
#'
#' @examples
calcula_total = function(output_nowcasting_age, serie_atual, Dmax){
  
  ##serie_atual <- tbl.UF.age.h.covid
  
  ## Aux
  dt.without.nowcast <- max(serie_atual$dt_event) - Dmax*7
  fx.txt <- c("Total", paste0((0:7)*10, " - ",(0:7)*10+9), "80+")
  
  ## Total
  
  total.without.nowcast <- serie_atual %>% 
    filter(dt_event <= dt.without.nowcast) %>% 
    summarise(n = sum(n))
  
  total.with.nowcast <- output_nowcasting_age %>% 
    group_by(sample) %>% 
    summarise(Y = sum(Y)) %>% 
    summarise(
      Mean = mean(Y),
      LI = quantile(Y, probs = 0.025),
      LS = quantile(Y, probs = 0.975)
    )  %>% bind_cols(total.without.nowcast) %>% 
    transmute(
      fx_etaria = "99",
      Mean = Mean + n,
      LI = LI + n,
      LS = LS + n
    )
  
  
  total.age.without.nowcast <- serie_atual %>% 
    filter(dt_event <= dt.without.nowcast) %>% 
    group_by(fx_etaria) %>% 
    summarise(n = sum(n))
  
  total.age.with.nowcast <- output_nowcasting_age %>% 
    group_by(sample, fx_etaria) %>% 
    summarise(Y = sum(Y)) %>% 
    group_by(fx_etaria) %>% 
    summarise(
      Mean = mean(Y),
      LI = quantile(Y, probs = 0.025),
      LS = quantile(Y, probs = 0.975)
    ) %>% 
    left_join( total.age.without.nowcast, by = "fx_etaria") %>% 
    transmute(
      fx_etaria = fx_etaria,
      Mean = Mean + n,
      LI = LI + n,
      LS = LS + n
    )
  
  total.with.nowcast %>% bind_rows(total.age.with.nowcast) %>% 
    mutate(fx_etaria = factor(x = fx_etaria, 
                              levels = as.character(c(99,1:9)), 
                              labels = fx.txt)
    )
  
}
