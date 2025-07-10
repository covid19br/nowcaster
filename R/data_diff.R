data_diff<-function(dataset,
                           trim.data=0,
                           date_start,
                           date_release,
                           cases,
                           use.epiweek = FALSE){
  
  
  data_w <- dataset |> 
    dplyr::rename(date_report = {{date_release}},
                  date_onset = {{date_start}},
                  cases = {{cases}}) |>
    arrange(date_onset, date_report) |>
    group_by(date_onset) |>
    mutate(delay = row_number() - 1) |>  
    arrange(delay, .by_group = TRUE) |>
    mutate(
      Y = cases - lag(cases, default = 0)
    ) |>
    ungroup()
  
  
  # Returning
  return(data_w)
  
}
