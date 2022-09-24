#' Title
#'
#' @param dados.summy
#' @param Dmax
#'
#' @return
#' @export
#'
#' @examples
ggplot.nowcasting <- function(dados.summy, Dmax = NULL){

  if(is.null(Dmax)){
    Dmax <- sum(!is.na(dados.summy$Median))
  }

  n <- nrow(dados.summy)

  dados.summy$Median[n-Dmax] <- dados.summy$n[n-Dmax]

  Tmax.plot <- max(dados.summy$dt_event[!is.na(dados.summy$Median)])
  dados.summy <- dados.summy |>
    dplyr::filter(dt_event <= Tmax.plot)

  p <- dados.summy |>
    ggplot2::ggplot(aes(x = dt_event,
                        y = n)) +
    ggplot2::geom_line() +
    ggplot2::theme_bw()

  p <- p +
    ggplot2::geom_ribbon(data = dados.summy |>
                           tidyr::drop_na(LI),
                         mapping = aes(x = dt_event,
                                       y = Median,
                                       ymin = LI,
                                       ymax = LS),
                         fill = "red",
                         alpha = 0.2) +
    ggplot2::geom_ribbon(data = dados.summy |>
                           tidyr::drop_na(LI),
                         mapping = aes(x = dt_event,
                                       y = Median,
                                       ymin = LIb,
                                       ymax = LSb),
                         fill = "red",
                         alpha = 0.3) +
    ggplot2::geom_line(data = dados.summy |>
                         tidyr::drop_na(Median),
                       mapping = aes(x = dt_event,
                                     y = Median),
                       color = "red")

  return(p)
}
