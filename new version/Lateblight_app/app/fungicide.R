fungicide_recommendation <- function(df, resistance){
  print(df)
  print(class(df))
  print(resistance)
  
  fungicide_applications <- df |> 
    filter(`ID-Location`==1) |> 
    group_by(APP) |> 
    summarise(n = n()-1, date = first(date)) |> 
    mutate(fungicide_type = case_when(
        
      n > 7 ~ 'Systemic',
      n <= 7 ~ 'Contact',
      
    )) |> 
    mutate(APP = as.integer(APP)) |> 
    rename("Date"=date)
  
  
  if(resistance == 'mr'){
    
    fungicide_applications <- fungicide_applications |> 
      mutate(fungicide_type = case_when(
        n<12 & n>9 ~ 'Contact',
        n>=12 ~ 'Systemic',
        .default = fungicide_type
      ))
  }
  
  if(resistance == 'r' || resistance=="hr"){
      
      fungicide_applications <- fungicide_applications |> 
          mutate(fungicide_type = case_when(
              n<12  ~ 'Contact',
              n>=12 ~ 'Systemic',
              .default = fungicide_type
          ))
  }
  
  if(resistance == 's'){
      # For susceptible varieties ("s"), alternate the fungicide type:
      # odd-numbered applications get "Systemic", even-numbered get "Contact"
      fungicide_applications <- fungicide_applications %>%
          arrange(APP) %>%  # ensure the applications are in order
          mutate(fungicide_type = if_else(row_number() %% 2 == 1, "Systemic", "Contact"))
  } else if(resistance == 'ms'){
      # For moderately susceptible (ms), the first application is "Systemic"
      fungicide_applications[1, 'fungicide_type'] <- 'Systemic'
  } else {
      # For other resistance values, assign the first application as "Contact"
      fungicide_applications[1, 'fungicide_type'] <- 'Contact'
  }
  
  
  if(resistance == 's' || resistance == 'ms'){
    
    fungicide_applications[1, 'fungicide_type'] <- 'Systemic'
    
  } else {
    
    fungicide_applications[1, 'fungicide_type'] <- 'Contact'
    
  }
  
  fungicide_applications <- fungicide_applications |> 
    rename('Application Number'=APP, 'Type of Fungicide'=fungicide_type, 'Number of Days'=n)
  
  return(fungicide_applications)
  
}