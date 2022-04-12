#' Title
#'
#' @param dadosINLA 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
tbl.nowcasting.summy <- function(dadosINLA, ...){
  
  model.eq <- Y ~ 1 + 
    f( Time, model = "rw1", constr = T,
       ##f( Time, model = "rw2", constr = T,
       ##hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001) ))
       hyper = list("prec" = list(prior = "pc.prec", param = c(2, 0.001) ))
    ) +
    f(delay, model = "rw1", constr = T,
      ## hyper = list("prec" = list(prior = "loggamma", param = c(0.001, 0.001) ))
      hyper = list("prec" = list(prior = "pc.prec", param = c(2, 0.001) ))
    ) ##+
  ## f(TimeDelay, model = "iid",
  ##   ## Checar priori tau ~ gamma(1, 1) sigma in (0.5; 6) 
  ##   ## sqrt(1/qgamma(c(0.025, 0.975),1,1))
  ##   hyper = list("prec" = list(prior = "loggamma", param = c(.1, .01) ))
  ## )
  
  
  
  output <- inla(formula = model.eq, 
                 family = "nbinomial", 
                 data = dadosINLA %>% 
                   mutate(
                     TimeDelay = paste(Time,delay)
                   ),
                 num.threads = 4,
                 control.predictor = list(link = 1, compute = T),
                 control.compute = list( config = T),
                 ...
                 ## control.family = list( 
                 ## hyper = list("theta" = list(
                 ##   prior = "loggamma", param = c(1, 0.1)))
                 ##   )
  )
  
  
  sample.now <- nowcasting(output = output, dados.ag = dadosINLA )
  
  pred.summy <- sample.now %>% group_by(dt_event) %>% 
    summarise(Median = median(Y),
              LI = quantile(Y, probs = 0.025),
              LS = quantile(Y, probs = 0.975),
              LIb = quantile(Y, probs = 0.25),
              LSb = quantile(Y, probs = 0.75), 
              .groups = "drop")
  
  
  total.summy <- sample.now %>% group_by(sample) %>% 
    summarise(Total = sum(Y), .groups = "drop") %>% 
    summarise(Median = median(Total),
              LI = quantile(Total, probs = 0.025),
              LS = quantile(Total, probs = 0.975),
              .groups = "drop")
  
  list(pred = pred.summy, total = total.summy)
}