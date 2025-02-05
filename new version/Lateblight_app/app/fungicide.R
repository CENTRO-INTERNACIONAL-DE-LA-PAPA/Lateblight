fungicide_recommendation <- function(df, resistance){
  print(df)
  print(class(df))
  print(resistance)
  fungicide_applications <- df |> 
    filter(APP != 0 & `ID-Location`==1) |> 
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
  
  
  if(resistance == 's' || resistance == 'ms'){
    
    fungicide_applications[1, 'fungicide_type'] <- 'Systemic'
    
  } else {
    
    fungicide_applications[1, 'fungicide_type'] <- 'Contact'
    
  }
  
  fungicide_applications <- fungicide_applications |> 
    rename('Application Number'=APP, 'Type of Fungicide'=fungicide_type, 'Number of Days'=n)
  
  return(fungicide_applications)
  
}