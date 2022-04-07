## Algorithm to get samples for the predictive distribution for the number of cases

#' Title
#'
#' @param output 
#' @param dados.ag 
#' @param MC.samples 
#'
#' @return
#' @export
#'
#' @examples
nowcasting <- function(output, dados.ag, MC.samples = 1000){
  
  index.missing = which(is.na(dados.ag$Y))
  
  
  ## Step 1: Sampling from the approximate posterior distribution using INLA
  samples.list <- inla.posterior.sample(n = MC.samples, output)
  
  ## Step 2: Sampling the missing triangle (in vector form) from the likelihood using INLA estimates
  vector.samples <- lapply(X = samples.list, 
                           FUN = ff,
                           idx = index.missing
  )
  
  
  ## Step 3: Calculate N_t for each triangle sample {N_t : t=Tactual-Dmax+1,...Tactual}
  tibble.samples <- lapply( X = vector.samples,
                            FUN = gg,
                            dados.gg = dados.ag, 
                            idx = index.missing
  )
  
  ## Nowcasting
  pred <- bind_rows(tibble.samples, .id = "sample")
  
  pred
}