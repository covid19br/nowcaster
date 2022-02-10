## Auxiliar functions for nowcasting

## Auxiliar function, sampling from a negative binomial likelihood
#' Title
#'
#' @param x 
#' @param idx 
#'
#' @return
#' @export
#'
#' @examples
ff <- function(x, idx){
  rnbinom(n = idx, mu = exp(x$latent[idx]), size = x$hyperpar[1])
}