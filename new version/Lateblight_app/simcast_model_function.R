calc_bu <- function(hhr, htavg, vt) {
  if (htavg > 27 & hhr == 24) {
    return(0)
    
    
  } else if (in_range2(htavg, 22.5, 27)) {
    if (vt == "s") {
      if (hhr == 6) {
        return(0)
        
      }
      
      else if (in_range(hhr, 7.0, 9.0)) {
        return(1)
        
      }
      
      else if (in_range(hhr, 10.0, 12.0)) {
        return(2)
        
      }
      
      else if (in_range(hhr, 13.0, 15.0)) {
        return(3)
        
      }
      
      else if (in_range(hhr, 16.0, 18.0)) {
        return(4)
        
      }
      
      else if (in_range(hhr, 19.0, 24.0)) {
        return(5)
        
        
      } else {
        return(0)
        
      }
      
    }
    
    else if (vt == "ms") {
      if (hhr == 9) {
        return(0)
        
      }
      
      else if (in_range(hhr, 10.0, 18.0)) {
        return(1)
        
      }
      
      else if (in_range(hhr, 19.0, 24.0)) {
        return(2)
        
        
      } else{
        return(0)
        
      }
    }
    
    else if (vt == "mr" | vt == "r" | vt == "hr") {
      if (hhr == 15) {
        return(0)
        
      }
      
      else if (in_range(hhr, 16.0, 24.0)) {
        return(1)
        
        
      } else{
        return(0)
        
      }
    }
    
    else {
      return(0)
    }
    
  }
  
  else if (in_range2(htavg, 12.5, 22.5)) {
    if (vt == "s") {
      if (hhr == 6) {
        return(0)
        
      }
      
      else if (in_range(hhr, 7.0, 9.0)) {
        return(5)
        
      }
      
      else if (in_range(hhr, 10.0, 12.0)) {
        return(6)
        
      }
      
      else if (in_range(hhr, 13.0, 24.0)) {
        return(7)
        
      }
      
      else{
        return(0)
        
      }
    }
    
    else if (vt == "ms") {
      if (hhr == 6) {
        return(0)
        
      }
      
      else if (hhr == 7) {
        return(1)
        
      }
      
      else if (hhr == 8) {
        return(2)
        
      }
      
      else if (hhr == 9) {
        return(3)
        
      }
      
      else if (hhr == 10) {
        return(4)
        
      }
      
      else if (in_range(hhr, 11.0, 12.0)) {
        return(5)
        
      }
      
      else if (in_range(hhr, 13.0, 24.0)) {
        return(6)
        
      }
      
      else{
        return(0)
      }
    }
    
    else if (vt == "mr" | vt == "r" | vt == "hr") {
      if (hhr == 6) {
        return(0)
        
      }
      
      else if (hhr == 7) {
        return(1)
        
      }
      
      else if (hhr == 8) {
        return(2)
        
      }
      
      else if (hhr == 9) {
        return(3)
        
      }
      
      else if (in_range(hhr, 10.0, 12.0)) {
        return(4)
        
      }
      
      else if (in_range(hhr, 13.0, 24.0)) {
        return(5)
        
      }
      else {
        return(0)
        
      }
    }
    
    else{
      return(0)
    }
  }
  
  else if (in_range2(htavg, 7.5, 12.5)) {
    if (vt == "s") {
      if (hhr == 6) {
        return(0)
        
      }
      
      else if (hhr == 7) {
        return(1)
        
      }
      
      else if (in_range(hhr, 8.0, 9.0)) {
        return(2)
        
      }
      
      else if (hhr == 10) {
        return(3)
        
      }
      
      else if (in_range(hhr, 11.0, 12.0)) {
        return(4)
        
      }
      
      else if (in_range(hhr, 13.0, 15.0)) {
        return(5)
        
      }
      
      else if (in_range(hhr, 16.0, 24.0)) {
        return(6)
        
      }
      
      else{
        return(0)
      }
    }
    
    else if (vt == "ms") {
      if (hhr == 6) {
        return(0)
        
      }
      
      else if (in_range(hhr, 7.0, 9.0)) {
        return(1)
        
      }
      
      else if (in_range(hhr, 10.0, 12.0)) {
        return(2)
        
      }
      
      else if (in_range(hhr, 13.0, 15.0)) {
        return(3)
        
      }
      
      else if (in_range(hhr, 16.0, 18.0)) {
        return(4)
        
      }
      
      else if (in_range(hhr, 19.0, 24.0)) {
        return(5)
        
      }
      
      else{
        return(0)
        
      }
    }
    
    else if (vt == "mr" | vt == "r" | vt == "hr") {
      if (hhr == 9) {
        return(0)
        
      }
      
      else if (in_range(hhr, 10.0, 12.0)) {
        return(1)
        
      }
      
      else if (in_range(hhr, 13.0, 15.0)) {
        return(2)
        
      }
      
      else if (in_range(hhr, 16.0, 24.0)) {
        return(3)
        
      }
      else {
        return(0)
        
      }
    }
    
    else {
      return(0)
      
    }
  }
  
  else if (in_range(htavg, 3, 7.5)) {
    if (vt == "s") {
      if (hhr == 9) {
        return(0)
        
      }
      
      else if (in_range(hhr, 10.0, 12.0)) {
        return(1)
        
      }
      
      else if (in_range(hhr, 13.0, 15.0)) {
        return(2)
        
      }
      
      else if (in_range(hhr, 16.0, 18.0)) {
        return(3)
        
      }
      
      else if (in_range(hhr, 19.0, 24.0)) {
        return(4)
        
      }
      
      else{
        return(0)
        
      }
    }
    
    else if (vt == "ms") {
      if (hhr == 12) {
        return(0)
        
      }
      
      else if (in_range(hhr, 13.0, 24.0)) {
        return(1)
        
      }
      
      else{
        return(0)
        
      }
    }
    
    else if (vt == "mr" | vt == "r" | vt == "hr") {
      if (hhr == 18) {
        return(0)
        
      }
      
      else if (in_range(hhr, 19.0, 24.0)) {
        return(1)
        
      }
      
      else{
        return(0)
      }
    }
    
    else{
      return(0)
    }
  }
  
  else if (htavg < 3 & hhr == 24) {
    return(0)
  }
  
  else {
    return(0)
    
  }
  
}

# function for calculate fungicide units
# INPUTS: rain mm, dsa - days since fungicide aplication

calc_fu <- function(rain, dsa) {
  if (rain < 1 & rain > 0) {
    return(1)
    
    
  } else {
    if (dsa == 1) {
      if (in_range(rain, 1.0, 1.45)) {
        return(4)
        
      }
      
      else if (in_range2(rain, 1.45, 3.45)) {
        return(5)
        
      }
      
      else if (in_range2(rain, 3.45, 6.0)) {
        return(6)
        
      }
      
      else if (rain > 6) {
        return(7)
        
      }
      
      else{
        return(0)
        
      }
    }
    
    else if (dsa == 2) {
      if (in_range(rain, 1.0, 1.45)) {
        return(3)
        
      }
      
      else if (in_range2(rain, 1.45, 4.45)) {
        return(4)
        
      }
      
      else if (in_range2(rain, 4.45, 8.0)) {
        return(5)
        
      }
      
      else if (rain > 8) {
        return(6)
        
      }
      
      else{
        return(0)
        
      }
    }
    
    else if (dsa == 3) {
      if (in_range(rain, 1.0, 2.45)) {
        return(3)
        
      }
      
      else if (in_range2(rain, 2.45, 5.0)) {
        return(4)
        
      }
      
      else if (rain > 5) {
        return(5)
        
      }
      
      else{
        return(0)
        
      }
    }
    
    else if (in_range(dsa, 4.0, 5.0)) {
      if (in_range(rain, 1.0, 2.45)) {
        return(3)
        
      }
      
      else if (in_range2(rain, 2.45, 8)) {
        return(4)
        
      }
      
      else if (rain > 8) {
        return(5)
        
      }
      
      else{
        return(0)
        
      }
    }
    
    else if (in_range(dsa, 6.0, 9.0)) {
      if (in_range(rain, 1.0, 4.0)) {
        return(3)
        
      }
      
      else if (rain > 4) {
        return(4)
        
      }
      
      else{
        return(0)
        
      }
    }
    
    else if (in_range(dsa, 10.0, 14.0)) {
      if (in_range(rain, 1.0, 1.45)) {
        return(2)
        
      }
      
      else if (in_range2(rain, 1.45, 8.0)) {
        return(3)
        
      }
      
      else if (rain > 8) {
        return(4)
        
      }
      
      else{
        return(0)
        
      }
    }
    
    else if (dsa > 14) {
      if (in_range(rain, 1.0, 8.0)) {
        return(2)
        
      }
      
      else if (rain > 8) {
        return(3)
        
      }
      
      else{
        return(0)
        
      }
    }
    
    else {
      return(0)
      
    }
    
  }
  
}

# function for decision rules
# INPUTS: abu, afu

check_bu_cutoff <- function(abu, vt) {
  if (vt == "s" & abu >= 30) {
    return(TRUE)
    
    
  } else if (vt == "ms" & abu >= 35) {
    return(TRUE)
    
    
  } else if (vt == "mr" & abu >= 45) {
    return(TRUE)
    
    
  } else if (vt == "r" & abu >= 50) {
    return(TRUE)
    
    
  } else if (vt == "hr" & abu >= 55) {
    return(TRUE)
    
    
  } else {
    return(FALSE)
    
  }
}

check_fu_cutoff <- function(afu, vt) {
  if (vt == "s" & afu > 15) {
    return(TRUE)
    
    
  } else if (vt == "ms" & afu > 20) {
    return(TRUE)
    
    
  } else if (vt == "mr" & afu > 40) {
    return(TRUE)
    
    
  } else if (vt == "r" & afu > 60) {
    return(TRUE)
    
    
  } else if (vt == "hr" & afu > 80) {
    return(TRUE)
    
    
  } else {
    return(FALSE)
    
  }
}


simcast_model <- function(df_in_simcast, vt) {
  vt <- as.character(vt[[1]])
  
  lines <- list()
  for (i in 1:nrow(df_in_simcast)) {
    lines[[i]] = df_in_simcast[i, ]
  }
  
  #########
  firstApplication <- TRUE
  returnValue <- 1
  bu <- 0
  fu <- 0
  bua <- 0
  fua <- 0
  afu <- 0
  app <- 0
  i <- 0
  last_bua <- 0
  last_fua <- 0
  app_ctr <- 0
  days_since_app <- 0
  min_day <- 7
  abu <- 0
  tabu <- 0
  tafu <- 0
  #begin <- 0
  end <- length(lines)
  ###############
  
  bu_final <- 0
  fu_final <- 0
  bua_final <- 0
  fua_final <- 0
  abu_final <- 0
  afu_final <- 0
  app_final <- 0
  
  for (k in 1:end) {
    line <- lines[[k]]
    day <- line[[1]]
    hhr <- as.numeric(line[[3]])
    htavg <- as.numeric(line[[4]])
    rain <- as.numeric(line[[5]])
    bu <- calc_bu(hhr, htavg, vt)
    last_bua <- bua
    bua <- bu + bua
    
    if (abu != 1 && afu != 1 && days_since_app <= 0) {
      days_since_app <- 0
      
    } else {
      days_since_app <- days_since_app + 1
      fu = calc_fu(rain, days_since_app)
      last_fua = fu
      fua = fu + fua
    }
    
    # modify && by ll (and "!") if you need the first app according to the dss
    
    if (days_since_app > min_day && !firstApplication) {
      if (check_bu_cutoff(bua, vt)) {
        bua <- 0
        fua <- 0
      }
      
      if (check_fu_cutoff(fua, vt)) {
        bua <- 0
        fua <- 0
      }
    }
    
    if (bua >= last_bua ||
        days_since_app <= min_day && !firstApplication) {
      app <- FALSE
      abu <- 0
      
    } else {
      abu <- 1
      app <- TRUE
      app_ctr <- app_ctr + 1
      days_since_app <- 0
      #firstApplication <- FALSE
      
    }
    
    if (k == 1) { 
      abu <- 1
      app <- TRUE
      app_ctr <- app_ctr + 1
      days_since_app <- 0
      firstApplication <- FALSE
      bua <- 0
      fua <- 0
    }
    
    if (fua < last_fua && days_since_app > min_day) {
      afu <- 1
      app <- TRUE
      app_ctr <- app_ctr + 1
      days_since_app <- 0
      
    } else {
      app <- FALSE
      afu <- 0
    }
    
    tabu <- abu + tabu
    tafu <- afu + tafu
    
    bu_final[k] <- bu
    fu_final[k] <- fu
    bua_final[k] <- bua
    fua_final[k] <- fua
    abu_final[k] <- abu
    afu_final[k] <- afu
    app_final[k] <- app_ctr
  }
  
  output_simcast <-
    data.frame(bu_final,
               fu_final,
               bua_final,
               fua_final,
               abu_final,
               afu_final,
               app_final)
  
  names(output_simcast) <-
    c("BU", "FU", "BUA", "FUA", "ABU", "AFU", "APP")
  
  output_scmodel <- cbind(df_in_simcast, output_simcast)
  
  print("Running simcast model ... Loading")
  
  return(output_scmodel)
}

in_range <- function(val, min, max) {
  return(val >= min & val <= max)
  
}

in_range2 <- function(val, min, max) {
  return(val > min & val <= max)
  
}
