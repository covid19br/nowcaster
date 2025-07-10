data_diff<-function(dataset,
                           trim.data=0,
                           date_start,
                           date_release,
                           cases,
                           age_col,
                           bins_age="preset",
                           use.epiweek = FALSE){
  
  if(missing(age_col)){
    
    data_w <- dataset |> 
      dplyr::rename(date_report = {{date_release}},
                    date_onset = {{date_start}},
                    cases = {{cases}}) |>
      arrange(date_onset, date_report) |>
      group_by(date_onset) |>
      mutate(delay = row_number() - 1) |>  
      arrange(delay, .by_group = TRUE) |>
      mutate(
        Y = cases - lag(cases, default = 0),
        Y = pmax(Y, 0)
      ) |>
      ungroup()  
    
    
  }else{
    
    
    data_w <- dataset |> 
      dplyr::rename(date_report = {{date_release}},
                    date_onset = {{date_start}},
                    cases = {{cases}},
                    fx_etaria={{age_col}}) |>
      arrange(date_onset, date_report) |>
      group_by(fx_etaria, date_onset) |>
      mutate(delay = row_number() - 1) |>  
      arrange(delay, .by_group = TRUE) |>
      mutate(
        Y = cases - lag(cases, default = 0),
        Y = pmax(Y, 0)
      ) |>
      ungroup()    
    
    
  }
  
  
  # Returning
  return(data_w)
  
}
