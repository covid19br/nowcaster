#' nowcasting.summary
#'
#' @param trajectory Trajectories to be summarized, trajectory should have sample, Time, dt_event and Y columns.
#' @param age Is by age data? [Default] FALSE, If its TRUE it will have columns  for age class, fx_etaria e fx_etaria.num
#'
#' @return
#' @export
#'
#' @examples
nowcasting.summary <- function(trajetory, age = F){

  total.summy <- trajetory |>
    dplyr::group_by(Time, dt_event, sample) |>
    dplyr::summarise(Y = sum(Y, na.rm = T)) |>
    dplyr::group_by(Time, dt_event) |>
    dplyr::summarise(Median = stats::median(Y, na.rm = T),
                     LI = stats::quantile(Y, probs = 0.025, na.rm = T),
                     LS = stats::quantile(Y, probs = 0.975, na.rm = T),
                     LIb = stats::quantile(Y, probs = 0.25, na.rm = T),
                     LSb = stats::quantile(Y, probs = 0.75, na.rm = T),
                     .groups = "drop")
  if(age){
    age.summy <- trajetory |>
      dplyr::group_by(Time, dt_event, fx_etaria, fx_etaria.num) |>
      dplyr::summarise(Median = stats::median(Y, na.rm = T),
                       LI = stats::quantile(Y, probs = 0.025, na.rm = T),
                       LS = stats::quantile(Y, probs = 0.975, na.rm = T),
                       LIb = stats::quantile(Y, probs = 0.25, na.rm = T),
                       LSb = stats::quantile(Y, probs = 0.75, na.rm = T),
                       .groups = "drop")

    output <- list()
    output$total <- total.summy
    output$age <- age.summy

  }else{
    output<- list()
    output$total <- total.summy
  }


  return(output)
}
