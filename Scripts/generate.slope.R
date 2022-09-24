#' @title Generate Slope
#'
#' @param data
#' @param trajectories
#' @param n.samples
#' @param week
#' @param Dmax
#'
#' @return
#' @export
#'
#' @examples
generate.slope <- function(data,
                           trajectories,
                           n.samples=1000,
                           week=week,
                           Dmax=10){

  pred.srag.var <-data |>
    rename(Date = dt_event) |>
    filter(Date <= week - Dmax)

  sample.list <- sort(rep(seq(1, n.samples), nrow(pred.srag.var)))

  pred.srag.var <- tibble(sample=slist,
                          Date=rep(pred.srag.var$Date,
                                   n.samples)) |>
    left_join(pred.srag.var, by='Date') |>
    rbind(trajectories) |>
    arrange(sample, Date)

  # Tendência via modelo linear com janela móvel
  weeks.level <- pred.srag.var$Date %>%
    unique()
  variation.lvl.3s <- weeks.level[weeks.level >= max(pred.srag.var$Date) - 8] %>%
    map(slope.estimate.quant, trajectories=pred.srag.var, w=3) %>%
    unlist() %>%
    as_tibble_col(column_name = 'tendencia.3s') %>%
    cbind(Date = weeks.level[weeks.level >= max(pred.srag.var$Date) - 8])
  variation.lvl <- weeks.level[weeks.level >= max(pred.srag.var$Date) - 16] %>%
    map(slope.estimate.quant, trajectories=pred.srag.var, w=6) %>%
    unlist() %>%
    as_tibble_col(column_name = 'tendencia.6s') %>%
    cbind(Date = weeks.level[weeks.level >= max(pred.srag.var$Date) - 16]) %>%
    left_join(variation.lvl.3s, by='Date')
  rm(pred.srag.var)

  pred.srag.summy <- trajectories %>% group_by(Date) %>%
    dplyr::summarise( #Mean = mean(Casos),
      Median = replace_na(round(median(Casos, na.rm=T)), 0),
      Q1 = round(quantile(Casos, probs = 0.25, na.rm=T)),
      Q3 = round(quantile(Casos, probs = 0.75, na.rm=T)),
      IC80I = round(quantile(Casos, probs = 0.1, na.rm=T)),
      IC80S = round(quantile(Casos, probs = 0.9, na.rm=T)),
      IC90I = round(quantile(Casos, probs = 0.05, na.rm=T)),
      IC90S = round(quantile(Casos, probs = 0.95, na.rm=T)),
      LI = round(quantile(Casos, probs = 0.025, na.rm=T)),
      LS = round(quantile(Casos, probs = 0.975, na.rm=T))
    ) %>%
    mutate(LS = case_when(
      LS > 30 ~ pmin(3*Median, LS),
      TRUE ~ LS
    ),
    IC90S = case_when(
      IC90S > 30 ~ pmin(3*Median, IC90S),
      TRUE ~ LS
    )) %>%
    right_join(data, by='Date') %>%
    arrange(Date) %>%
    mutate(full_estimate = case_when(
      is.na(Median) ~ as.numeric(Casos),
      TRUE ~ as.numeric(Median)),
      Casos.cut = case_when(
        Date <= week - 2 ~ Casos,
        TRUE ~ NA_integer_),
      rolling_average = round(zoo::rollmean(full_estimate, k=3, fill=NA))) %>%
    left_join(variation.lvl, by='Date')

  return(pred.srag.summy)


}
