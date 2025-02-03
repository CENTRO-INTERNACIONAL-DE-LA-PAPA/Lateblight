server <- function(input, output, session) {
  # INDICAR LAS CREDENCIALES DEL API
  
  # aWhereAPI::get_token(uid="1iiGfRRH9BewFNGw7tEOoRv13WbdgI7z", secret="8VSUKD8mrVUQ1gLd")
  #register_google(key ='AIzaSyB5UWpjiUSyhi4fO7qKck81knTYSVAEdDI')
  
  URL_base <- 'http://api.weatherapi.com/v1'
  API_Method_history <- '/history.json'
  API_Method_forecast <- '/forecast.json'
  API_key <- paste0("?key=",Sys.getenv("API_KEY"))
  
  
  # Assign NULL to object values with fileinput NULL
  values <- reactiveValues(fileInput = NULL)
  
  # Clear map
  observe({
    input$clearMap1a
    values$fileInput <- NULL
    values$markers <- NULL
    values$points <- NULL
    leafletProxy("mymap1a") %>% clearMarkers()
  })
  
  # Input FileInput data
  observe({
    values$fileInput <- input$fileInput
  })
  
  # Create widget and show fileinput as a table
  output$resetfileInput <- renderUI({
    fileInput(
      "fileInput",
      "(COORDENADAS X Y)",
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      ) ,
      width = "100%"
    )
  })
  
  dataPoints <- reactive({
    inFile = values$fileInput
    
    if (!is.null(inFile)) {
      values$points <-
        read.table(file = inFile$datapath,
                   header = T,
                   sep = ";")
    }
  })
  
  output$fileInputPoints = DT::renderDataTable({
    server = TRUE
    data <- values$points
    
    isolate({
      datatable(
        data,
        class = "cell-border stripe",
        selection = "none",
        options = list(pageLength = 10)
      )
    })
  })
  
  # Create map with leaflet
  
  #"https://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
  
  map1a = leaflet() %>%
    addTiles() %>%
    setView(lng = -4.04296,
            lat = 16.30796,
            zoom = 2) %>%
    addControlGPS(
      options = gpsOptions(
        position = "topleft",
        activate = TRUE,
        autoCenter = TRUE,
        maxZoom = 60,
        setView = TRUE
      )
    )
  
  # Output variable "mymap1"
  output$mymap1a <- renderLeaflet(map1a)
  
  # Add GPS
  observeEvent(input$mymap1a_gps_located, {
    gps_points <- input$mymap1a_gps_located
    gps_lat <- as.numeric(gps_points$coordinates$lat)
    gps_lng <- as.numeric(gps_points$coordinates$lng)
    
    values$markers <- rbind(data.frame(
      Latitude = round(gps_lat, 4),
      Longitude = round(gps_lng, 4)
    ),
    values$markers)
    
  })
  
  # Add points markers
  observe({
    inFile = input$fileInput
    
    if (!is.null(inFile)) {
      df <- dataPoints()
      
      if (!is.null(df)) {
        for (i in 1:nrow(df)) {
          leafletProxy("mymap1a") %>%
            addMarkers(
              lng = df[i, 3],
              lat = df[i, 2],
              popup = paste(df[i, 1], round(df[i, 2], 4), round(df[i, 3], 4), sep = ", ")
            )
        }
      }
    }
  })
  
  # Insert points markers with the mouse
  observeEvent(input$mymap1a_click, {
    click <- input$mymap1a_click
    clat <- click$lat
    clng <- click$lng
    
    
    
    values$markers <- rbind(data.frame(
      Latitude = round(clat, 4),
      Longitude = round(clng, 4)
    ),
    values$markers)
    
    leafletProxy('mymap1a') %>%
      addMarkers(
        lng = clng,
        lat = clat,
        popup = paste(round(clat, 4), round(clng, 4), sep = ", "),
        icon = list(iconUrl = 'https://icons.iconarchive.com/icons/icons8/ios7/256/Maps-Location-icon.png',
                    iconSize = c(30, 30))
      )
    
    
  })
  
  # Create dataframe and table with ALL points markers
  output$inputMarkers = DT::renderDataTable({
    server = TRUE
    data <- values$markers
    
    isolate({
      datatable(
        data,
        class = "cell-border stripe",
        selection = "none",
        options = list(pageLength = 10)
      )
    })
  })
  
  reactive ({
    print(values$markers)
  })
  
  
  # --------------------------------------
  
  output$var_names1 <- renderUI({
    names_var_per <-
      read.csv(
        "potato-res-per.csv",
        header = T,
        sep = ";",
        stringsAsFactors = F,
        encoding = "UTF-8"
      )
    
    my_list_per <- as.list(names_var_per$Resistance)
    names(my_list_per) <- names_var_per$Cultivar
    
    ######################################
    
    names_var_ecu <-
      read.csv(
        "potato-res-ecu.csv",
        header = T,
        sep = ";",
        stringsAsFactors = F,
        encoding = "UTF-8"
      )
    
    my_list_ecu <- as.list(names_var_ecu$Resistance)
    names(my_list_ecu) <- names_var_ecu$Cultivar
    
    
    names_var_india <- 
      read.csv(
        "potato-res-india.csv",
        header = T,
        sep = ";",
        stringsAsFactors = F,
        encoding = "UTF-8"
      )
    
    my_list_india = as.list(names_var_india$Resistance)
    names(my_list_india) <- names_var_india$Cultivar
    
    
    if (input$country_var == "per") {
      selectInput("res",
                  "Select the potato variety:",
                  choices = my_list_per ,
                  selectize = FALSE)
    } else if (input$country_var == "ecu"){
      selectInput("res",
                  "Select the potato variety:",
                  choices = my_list_ecu ,
                  selectize = FALSE)
    } else {
      selectInput("res",
                  "Select the potato variety:",
                  choices = my_list_india ,
                  selectize = FALSE)
      
      
    }
    
  })
  
  
  # --------------------------------------
  
  # function for ranges
  in_range <- function(val, min, max) {
    return(val >= min & val <= max)
    
  }
  
  in_range2 <- function(val, min, max) {
    return(val > min & val <= max)
    
  }
  
  # function for calculate blight units
  # INPUTS: hrhours>90 , avg temp, variety type (resistance)
  
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
      
      
    } else if (vt == "mr" & abu >= 40) {
      return(TRUE)
      
      
    } else if (vt == "r" & abu >= 45) {
      return(TRUE)
      
      
    } else if (vt == "hr" & abu >= 50) {
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
      
      
    } else if (vt == "mr" & afu > 25) {
      return(TRUE)
      
      
    } else if (vt == "r" & afu > 35) {
      return(TRUE)
      
      
    } else if (vt == "hr" & afu > 45) {
      return(TRUE)
      
      
    } else {
      return(FALSE)
      
    }
  }
  
  # function for calculate humidity relative hourly
  # INPUTS: meterological data, coordinates (lat y long)
  
  calculate_hhr <- function(climdata, lon, lat) {
    #########################
    
    date_df <- as.Date(climdata[[1]], format = "%Y-%m-%d")
    tn <- as.numeric(climdata[[2]])
    tx <- as.numeric(climdata[[3]])
    tavg <- (tn + tx) / 2
    hrn <- as.numeric(climdata[[4]])
    hrx <- as.numeric(climdata[[5]])
    hravg <- (hrn + hrx) / 2
    tdew <- tavg - ((100 - hravg) / 5)
    rain <- as.numeric(climdata[[6]])
    
    
    # Calculate hourly for sunrise and sunset
    
    crds <- matrix(c(lon[[1]], lat[[1]]), ncol = 2)
    
    sunrise <- sunriset(
      crds,
      as.POSIXct(date_df),
      proj4string = CRS("+proj=longlat +datum=WGS84"),
      direction = c("sunrise"),
      POSIXct.out = F
    )
    
    sunset <- sunriset(
      crds,
      as.POSIXct(date_df),
      proj4string = CRS("+proj=longlat +datum=WGS84"),
      direction = c("sunset"),
      POSIXct.out = F
    )
    
    
    df_format_hhr <-
      data.frame(date_df[-c(length(date_df), 1)],
                 tn[-c(length(tn), 1)],
                 tn[-c(1:2)],
                 tx[-c(length(tx), 1)],
                 tx[-c(length(tx), length(tx) - 1)],
                 tdew[-c(length(tdew), 1)],
                 sunrise[-c(length(sunrise), 1)],
                 sunset[-c(length(sunset), 1)],
                 sunset[-c(length(sunset), length(sunset) - 1)])
    
    names(df_format_hhr) <- c(
      "date",
      "tmin",
      "tminnext",
      "tmax",
      "tmaxold",
      "tdew",
      "sunrise",
      "sunset",
      "sunset old"
    )
    
    #########################
    
    # Tn temp min, Tx temp max, To sunset , Tp temp min next
    # Hn sunrise, Hx h temp max, Ho h sunset , Hp h temp min next
    # date <- df_in_hhr[[1]]
    
    #####
    
    model_temp_hr <- function(df_in_hhr) {
      Tn <- as.numeric(df_in_hhr[[2]])
      Tp <- as.numeric(df_in_hhr[[3]])
      Tx <- as.numeric(df_in_hhr[[4]])
      Hn <- as.numeric(df_in_hhr[[7]]) * 24
      Ho <- as.numeric(df_in_hhr[[8]]) * 24
      tdew <- as.numeric(df_in_hhr[[6]])
      
      #####
      
      Tp_old <- Tn
      Hp_old <- Hn + 24
      Tx_old <- as.numeric(df_in_hhr[[5]])
      Ho_old <- as.numeric(df_in_hhr[[9]]) * 24
      To_old <- Tx_old - 0.39 * (Tx_old - Tp_old)
      
      # Parameters for model
      To <- Tx - 0.39 * (Tx - Tp)
      Hp <- Hn + 24
      Hx <- Ho - 4
      
      alpha <- Tx - Tn
      r <- Tx - To
      beta1 <- (Tp - To) / sqrt(Hp - Ho)
      beta2 <- (Tp_old - To_old) / sqrt(Hp_old - Ho_old)
      
      t <- 1:24
      T_model <- 0
      
      for (i in 1:24) {
        if (t[i] > Hn & t[i] <= Hx) {
          T_model[i] <- Tn + alpha * (((t[i] - Hn) / (Hx - Hn)) * (pi / 2))
          
        }
        
        else if (t[i] > Hx & t[i] <= Ho) {
          T_model[i] <- To + r * sin((pi / 2) + (((t[i] - Hx) / 4) * (pi / 2)))
          
        }
        
        else if (t[i] > Ho & t[i] <= 24) {
          T_model[i] <- To + beta1 * sqrt((t[i] - Ho))
          
        }
        
        else if (t[i] >= 1 & t[i] <= Hn) {
          T_model[i] <- To_old + beta2 * sqrt((t[i] + 24) - Ho_old)
          
        }
        
        
        else {
          T_model[i] <- "Error"
          
        }
        
      }
      
      
      # Buck formula for es and e (kPa)
      
      es <-
        0.61121 * exp(((18.678 - (
          T_model / 234.5
        )) * T_model) / (257.14 + T_model))
      e <-
        0.61121 * exp(((18.678 - (tdew / 234.5)) * tdew) / (257.14 + tdew))
      
      # hr
      hr <- (e / es) * 100
      df <- data.frame(T_model, hr)
      
      return(df)
    }
    
    
    df_temp_hr_hour <-
      apply(df_format_hhr, 1 , function(x)
        model_temp_hr(x))
    
    df_temp_hr_hour_simcast <- list()
    
    for (i in 1:(length(df_temp_hr_hour) - 1)) {
      df_temp_hr_hour_simcast[[i]] <- rbind(df_temp_hr_hour[[i]][13:24, ],
                                            df_temp_hr_hour[[i + 1]][1:12, ])
      
      
    }
    
    
    hr_limit90_temp <- function(x) {
      temp_model_prom <- ifelse(nrow(x[x[, 2] > 90,]) > 0,
                                mean(x[x[, 2] > 90, 1]),
                                mean(x[, 1]))
      
      
      hr_model_hours <- nrow(x[x[, 2] > 90,])
      df <- data.frame(temp_model_prom, hr_model_hours)
      return(df)
    }
    
    
    list_hours <-
      lapply(df_temp_hr_hour_simcast, function(x)
        hr_limit90_temp(x))
    df_hours <- rbindlist(list_hours)
    
    
    df_output_hr <-
      data.frame(date_df[-c(length(date_df), 1, 2)], df_hours[, 2],
                 tavg[-c(length(date_df), 1, 2)], rain[-c(length(rain), 1, 2)])
    
    names(df_output_hr) <-
      c("date", "hr90_hour", "tavg_C", "rain_mm")
    
    print("Run temperature and humidity model...")
    
    return(df_output_hr)
    
  }
  
  
  # function for calculate humidity relative hourly - Weather API
  # INPUTS: day initial, day final, coordinates (lat y long), hr limit
  
  climDataAPI_input_model <- function(day0,
             dayn,
             lat,
             long,
             hrlimite,
             calculate_hhr) {
        
      dayinitial <- day0
      dayfinal <- dayn
      sysdate <- Sys.Date()
      API_q_coordinates <- paste('&q=', lat, ',', long, sep = '')
      difftime_day <- as.numeric(dayn - day0)
      
      print(paste0("Planting date: ",dayinitial))
      print(paste0("Forecast day: ",dayfinal))
      
      
      if((dayinitial < sysdate) == TRUE && (dayfinal < sysdate) == TRUE) {
          
        if (difftime_day <= 30) {
            
          API_date <- paste('&dt=', dayinitial, '&end_dt=', dayfinal, sep = '')
          
          PreDataAPI <- paste(URL_base,
                  API_Method_history,
                  API_key,
                  API_q_coordinates,
                  API_date,
                  sep = '')
          
          dataAPI_json = rjson::fromJSON(file = PreDataAPI)
          dataAPI_df1 <-
            do.call(rbind, dataAPI_json$forecast$forecastday)
          dataAPI_df2 <-
            do.call(cbind, dataAPI_df1[, "hour"]) #datos horarios
          dataAPI_df3 <-
            apply(dataAPI_df2, 2, function(x)
              as.data.frame(do.call(rbind, x)))
          dataAPI_df4 <- do.call(rbind, dataAPI_df3)
          print(head(dataAPI_df4))
          
          
          climData <- dataAPI_df4[, c("time","temp_c","humidity","precip_mm")]
          climData[, 1] <-
            as.POSIXct(as.character(climData[, 1]), format = "%Y-%m-%d %H:%M")
          climData[, 2] <- as.numeric(climData[, 2])
          climData[, 3] <- as.numeric(climData[, 3])
          climData[, 4] <- as.numeric(climData[, 4])
          names(climData) <- c("date_hour", "tmean", "hr", "pp")
          
          
          climData$date_daily <- format(climData$date_hour, "%Y-%m-%d")
          tmean2 <- aggregate(tmean ~ date_daily, climData, mean)
          pp2 <- aggregate(pp ~ date_daily, climData, sum)
          hr_hours2 <- aggregate(
            hr ~ date_daily,
            climData,
            FUN = function(RH) {
              return(length(RH[RH > as.numeric(hrlimite)]))
            }
          )
          hr_avg <- aggregate(hr ~ date_daily, climData, mean)
          
          input_runsimcast <-
            data.frame(hr_hours2, tmean2[, 2], pp2[, 2], hr_avg[,2])
          
          names(input_runsimcast) <-
            c("date", "hr90_hour", "tavg_C", "rain_mm","avg_hr")
          print(input_runsimcast)
          
          # input-runsimcast: date, hr>90, temp avg, rain
          
          return(input_runsimcast)
          
          
        }
        
        else if (difftime_day > 30) {
          seq_API_date <-
            unique(c(seq(
              from = dayinitial,
              to = dayfinal,
              by = 30
            ), dayfinal))
          
          list_input_runsimcast <- list()
          
          
          for (i in 1:length(seq_API_date)) {
            dayinitial2 = seq_API_date[i]
            dayfinal2 = seq_API_date[i + 1]
            
            API_date <-
              paste('&dt=', dayinitial2, '&end_dt=', dayfinal2, sep = '')
            
            PreDataAPI <-
              paste(URL_base,
                    API_Method_history,
                    API_key,
                    API_q_coordinates,
                    API_date,
                    sep = '')
            
            dataAPI_json = rjson::fromJSON(file = PreDataAPI)
            dataAPI_df1 <-
              do.call(rbind, dataAPI_json$forecast$forecastday)
            dataAPI_df2 <-
              do.call(cbind, dataAPI_df1[, 4]) #datos horarios
            dataAPI_df3 <-
              apply(dataAPI_df2, 2, function(x)
                as.data.frame(do.call(rbind, x)))
            dataAPI_df4 <- do.call(rbind, dataAPI_df3)
            print(head(dataAPI_df4))
            
            climData <- dataAPI_df4[, c("time","temp_c","humidity","precip_mm")]
            climData[, 1] <-
              as.POSIXct(as.character(climData[, 1]), format = "%Y-%m-%d %H:%M")
            climData[, 2] <- as.numeric(climData[, 2])
            climData[, 3] <- as.numeric(climData[, 3])
            climData[, 4] <- as.numeric(climData[, 4])
            names(climData) <- c("date_hour", "tmean", "hr", "pp")
            
            climData$date_daily <-
              format(climData$date_hour, "%Y-%m-%d")
            tmean2 <- aggregate(tmean ~ date_daily, climData, mean)
            pp2 <- aggregate(pp ~ date_daily, climData, sum)
            hr_hours2 <- aggregate(
              hr ~ date_daily,
              climData,
              FUN = function(RH) {
                return(length(RH[RH > as.numeric(hrlimite)]))
              }
            )
            
            hr_avg <- aggregate(hr ~ date_daily, climData, mean)
            
            input_runsimcast <-
              data.frame(hr_hours2, tmean2[, 2], pp2[, 2], hr_avg[,2])
            
            names(input_runsimcast) <-
              c("date", "hr90_hour", "tavg_C", "rain_mm", "avg_hr")
            
            # input-runsimcast: date, hr>90, temp avg, rain
            
            list_input_runsimcast[[i]] <- input_runsimcast
            
            if (i == (length(seq_API_date) - 1)) {
              break
            }
            
            
          }
          
          input_runsimcast <- do.call(rbind, list_input_runsimcast)
          input_runsimcast <- unique(input_runsimcast)
          print(input_runsimcast)
          
          return(input_runsimcast)
          
          
        }
        
      }
      
      else if ((dayinitial < sysdate) == TRUE &&
               (dayfinal >= sysdate) == TRUE) {
          
        dayinitial_h <- dayinitial
        dayfinal_h <- sysdate - 1
        dayinitial_f <- sysdate
        dayfinal_f <- dayfinal
        
        difftime_day_h <- as.numeric(dayfinal_h  - dayinitial_h)
        difftime_day_f <- as.numeric(dayfinal_f  - dayinitial_f)
        
        ######## HISTORICAL DATA FOR API
        
        if (difftime_day_h <= 30) {
          API_date <-
            paste('&dt=', dayinitial_h, '&end_dt=', dayfinal_h, sep = '')
          
          PreDataAPI <-
            paste(URL_base,
                  API_Method_history,
                  API_key,
                  API_q_coordinates,
                  API_date,
                  sep = '')
          
          dataAPI_json = rjson::fromJSON(file = PreDataAPI)
          dataAPI_df1 <-
            do.call(rbind, dataAPI_json$forecast$forecastday)
          dataAPI_df2 <-
            do.call(cbind, dataAPI_df1[, 4]) #datos horarios
          dataAPI_df3 <-
            apply(dataAPI_df2, 2, function(x)
              as.data.frame(do.call(rbind, x)))
          dataAPI_df4 <- do.call(rbind, dataAPI_df3)
          #print(head(dataAPI_df4))
          
          climData <- dataAPI_df4[, c("time","temp_c","humidity","precip_mm")]
          climData[, 1] <-
            as.POSIXct(as.character(climData[, 1]), format = "%Y-%m-%d %H:%M")
          climData[, 2] <- as.numeric(climData[, 2])
          climData[, 3] <- as.numeric(climData[, 3])
          climData[, 4] <- as.numeric(climData[, 4])
          names(climData) <- c("date_hour", "tmean", "hr", "pp")
          
          
          climData$date_daily <- format(climData$date_hour, "%Y-%m-%d")
          tmean2 <- aggregate(tmean ~ date_daily, climData, mean)
          pp2 <- aggregate(pp ~ date_daily, climData, sum)
          hr_hours2 <- aggregate(
            hr ~ date_daily,
            climData,
            FUN = function(RH) {
              return(length(RH[RH > as.numeric(hrlimite)]))
            }
          )
          
          hr_avg <- aggregate(hr ~ date_daily, climData, mean)
          
          input_runsimcast_h <-
            data.frame(hr_hours2, tmean2[, 2], pp2[, 2], hr_avg[,2])
          
          names(input_runsimcast_h) <-
            c("date", "hr90_hour", "tavg_C", "rain_mm", "avg_hr")
          #print(input_runsimcast_h)
          
          # input-runsimcast: date, hr>90, temp avg, rain
          
          #return(input_runsimcast_h);
          
        }
        
        else if (difftime_day_h > 30) {
          seq_API_date <-
            unique(c(
              seq(
                from = dayinitial_h,
                to = dayfinal_h,
                by = 30
              ),
              dayfinal_h
            ))
          
          list_input_runsimcast_h <- list()
          
          
          for (i in 1:length(seq_API_date)) {
            dayinitial2 = seq_API_date[i]
            dayfinal2 = seq_API_date[i + 1]
            
            API_date <-
              paste('&dt=', dayinitial2, '&end_dt=', dayfinal2, sep = '')
            
            PreDataAPI <-
              paste(URL_base,
                    API_Method_history,
                    API_key,
                    API_q_coordinates,
                    API_date,
                    sep = '')
            
            dataAPI_json = rjson::fromJSON(file = PreDataAPI)
            dataAPI_df1 <-
              do.call(rbind, dataAPI_json$forecast$forecastday)
            dataAPI_df2 <-
              do.call(cbind, dataAPI_df1[, 4]) #datos horarios
            dataAPI_df3 <-
              apply(dataAPI_df2, 2, function(x)
                as.data.frame(do.call(rbind, x)))
            dataAPI_df4 <- do.call(rbind, dataAPI_df3)
            #print(head(dataAPI_df4))
            
            climData <- dataAPI_df4[, c("time","temp_c","humidity","precip_mm")]
            climData[, 1] <-
              as.POSIXct(as.character(climData[, 1]), format = "%Y-%m-%d %H:%M")
            climData[, 2] <- as.numeric(climData[, 2])
            climData[, 3] <- as.numeric(climData[, 3])
            climData[, 4] <- as.numeric(climData[, 4])
            names(climData) <- c("date_hour", "tmean", "hr", "pp")
            
            climData$date_daily <-
              format(climData$date_hour, "%Y-%m-%d")
            tmean2 <- aggregate(tmean ~ date_daily, climData, mean)
            pp2 <- aggregate(pp ~ date_daily, climData, sum)
            hr_hours2 <- aggregate(
              hr ~ date_daily,
              climData,
              FUN = function(RH) {
                return(length(RH[RH > as.numeric(hrlimite)]))
              }
            )
            
            hr_avg <- aggregate(hr ~ date_daily, climData, mean)
            
            input_runsimcast_h <-
              data.frame(hr_hours2, tmean2[, 2], pp2[, 2], hr_avg[,2])
            
            names(input_runsimcast_h) <-
              c("date", "hr90_hour", "tavg_C", "rain_mm","avg_hr")
            
            # input-runsimcast: date, hr>90, temp avg, rain
            
            list_input_runsimcast_h[[i]] <- input_runsimcast_h
            
            if (i == (length(seq_API_date) - 1)) {
              break
            }
            
            
          }
          
          input_runsimcast_h <-
            do.call(rbind, list_input_runsimcast_h)
          input_runsimcast_h <- unique(input_runsimcast_h)
          #print(input_runsimcast_h)
          
          #return(input_runsimcast_h);
          
        }
        
        ######## FORECAST DATA FOR API
        
        if (difftime_day_f <= 30) {
          API_date <-
            paste('&days=', (as.Date(dayfinal_f) - as.Date(dayinitial_f) + 1), sep = '')
          
          PreDataAPI <-
            paste(URL_base,
                  API_Method_forecast,
                  API_key,
                  API_q_coordinates,
                  API_date,
                  sep = '')
          print(PreDataAPI)
          
          dataAPI_json = rjson::fromJSON(file = PreDataAPI)
          dataAPI_df1 <-
            do.call(rbind, dataAPI_json$forecast$forecastday)
          dataAPI_df2 <-
            do.call(cbind, dataAPI_df1[, 5]) #datos horarios
          dataAPI_df3 <-
            apply(dataAPI_df2, 2, function(x)
              as.data.frame(do.call(rbind, x)))
          dataAPI_df4 <- do.call(rbind, dataAPI_df3)
          print(head(dataAPI_df4))
          
          climData <- dataAPI_df4[, c("time","temp_c","humidity","precip_mm")]
          climData[, 1] <-
            as.POSIXct(as.character(climData[, 1]), format = "%Y-%m-%d %H:%M")
          climData[, 2] <- as.numeric(climData[, 2])
          climData[, 3] <- as.numeric(climData[, 3])
          climData[, 4] <- as.numeric(climData[, 4])
          names(climData) <- c("date_hour", "tmean", "hr", "pp")
          
          
          climData$date_daily <- format(climData$date_hour, "%Y-%m-%d")
          tmean2 <- aggregate(tmean ~ date_daily, climData, mean)
          pp2 <- aggregate(pp ~ date_daily, climData, sum)
          hr_hours2 <- aggregate(
            hr ~ date_daily,
            climData,
            FUN = function(RH) {
              return(length(RH[RH > as.numeric(hrlimite)]))
            }
          )
          hr_avg <- aggregate(hr ~ date_daily, climData, mean)
          
          input_runsimcast_f <-
            data.frame(hr_hours2, tmean2[, 2], pp2[, 2], hr_avg[,2])
          
          names(input_runsimcast_f) <-
            c("date", "hr90_hour", "tavg_C", "rain_mm", "avg_hr")
          
          # input-runsimcast: date, hr>90, temp avg, rain
          
          #return(input_runsimcast_f);
          
        }
        
        input_runsimcast_h_y_f <-
          rbind(input_runsimcast_h, input_runsimcast_f)
        
        print(input_runsimcast_h_y_f)
        return(input_runsimcast_h_y_f)
        
      }
      
      
      else if ((dayinitial >= sysdate) == TRUE &&
               (dayfinal >= sysdate) == TRUE) {
        API_date <-
          paste('&days=', (as.Date(dayfinal) - as.Date(dayinitial) + 1), sep = '')
        
        PreDataAPI <-
          paste(URL_base,
                API_Method_forecast,
                API_key,
                API_q_coordinates,
                API_date,
                sep = '')
        print(PreDataAPI)
        
        dataAPI_json = rjson::fromJSON(file = PreDataAPI)
        dataAPI_df1 <-
          do.call(rbind, dataAPI_json$forecast$forecastday)
        dataAPI_df2 <- do.call(cbind, dataAPI_df1[, 4]) #datos horarios
        dataAPI_df3 <-
          apply(dataAPI_df2, 2, function(x)
            as.data.frame(do.call(rbind, x)))
        dataAPI_df4 <- do.call(rbind, dataAPI_df3)
        print(head(dataAPI_df4))
        
        climData <- dataAPI_df4[, c("time","temp_c","humidity","precip_mm")]
        climData[, 1] <-
          as.POSIXct(as.character(climData[, 1]), format = "%Y-%m-%d %H:%M")
        climData[, 2] <- as.numeric(climData[, 2])
        climData[, 3] <- as.numeric(climData[, 3])
        climData[, 4] <- as.numeric(climData[, 4])
        names(climData) <- c("date_hour", "tmean", "hr", "pp")
        
        
        climData$date_daily <- format(climData$date_hour, "%Y-%m-%d")
        tmean2 <- aggregate(tmean ~ date_daily, climData, mean)
        pp2 <- aggregate(pp ~ date_daily, climData, sum)
        hr_hours2 <- aggregate(
          hr ~ date_daily,
          climData,
          FUN = function(RH) {
            return(length(RH[RH > as.numeric(hrlimite)]))
          }
        )
        hr_avg <- aggregate(hr ~ date_daily, climData, mean)
        
        input_runsimcast_f <-
          data.frame(hr_hours2, tmean2[, 2], pp2[, 2], hr_avg[,2])
        
        names(input_runsimcast_f) <-
          c("date", "hr90_hour", "tavg_C", "rain_mm", "avg_hr")
        
        return(input_runsimcast_f)
        
        
      }
      
      
    }
  
  
  # function for SIMCAST model Fry et. al 1983
  
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
    # if(vt == 's' || vt == 'ms'){
    #   min_day <- 7
    # } else if(vt == 'mr'){
    #   min_day <- 9
    # } else if(vt == 'r'){
    #   min_day <- 11
    # } else if (vt == 'hr'){
    #   min_day <- 15
    # }
    # 
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
      hhr <- as.numeric(line[[2]])
      htavg <- as.numeric(line[[3]])
      rain <- as.numeric(line[[4]])
      bu <- calc_bu(hhr, htavg, vt)
      last_bua <- bua
      bua <- bu + bua
      
      if (abu != 1 && afu != 1 && days_since_app <= 0) {
        days_since_app <- 0} else {
            
        days_since_app <- days_since_app + 1
        fu = calc_fu(rain, days_since_app)
        last_fua = fua
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
      
      if (bua >= last_bua || days_since_app <= min_day && !firstApplication) {
        app <- FALSE
        abu <- 0
        afu <- 0
        
      } else {
        abu <- 1
        app <- TRUE
        app_ctr <- app_ctr + 1
        days_since_app <- 0
        #firstApplication <- FALSE
        
      }
      
      if (k == 24) { ##
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
  
  # -------------------------------------
  
  observe({
    data_pts <- values$markers
    print(data_pts)
  })
  
  
  tableGS <- reactive({
    click <- input$mymap1a_click
    
    day0_i <- input$date0
    # -2 for initial day
    day0 <- day0_i - 2
    dayn <- input$daten
    hrlimite <- as.numeric(90)
    
    dataxycoord <- values$markers
    print(dataxycoord)
    print(day0_i)
    
    
    strTable <-
      apply(dataxycoord, 1, function(x)
        climDataAPI_input_model(day0, dayn, x[1], x[2], hrlimite, calculate_hhr))
    
    write.csv(strTable, file = "strTable.csv")
    
    ###############################
    
    runsimcast2 <- lapply(strTable,
                          function(x)
                            simcast_model(x, input$res))
    
    print(runsimcast2)
    
    rsf <- rbindlist(runsimcast2, fill = TRUE, idcol = "ID-Location")
    fungicide_rec <- fungicide_recommendation(rsf,input$res)
    rsf <- rsf |> 
        left_join(fungicide_rec,by=join_by("date"=="Date"))
    
    return(list(runsimcast2, rsf))
    
  })
  
  tableGS2 <- eventReactive(input$run1, {
    tableGS()
  })
  
  
  output$table2 <- renderDataTable({
    DT::datatable(tableGS2()[[2]])
  })
  
  # pop up with recomendations
  observeEvent(input$run1, {
    shinyalert::shinyalert("Generando recomendaciones",
               text = NULL,
               size = 'm',
               closeOnEsc = F, closeOnClickOutside = T,type = 'info',
               showConfirmButton = T, showCancelButton = F, timer=0,
               imageUrl = 'field_image.jpeg',
               imageWidth = 400,imageHeight = 400, animation = T)
    
    
  })
  
  output$calendars <- toastui::renderCalendar({
    
    req(tableGS2())
    
    data_for_calendar <- function(df_output_runsimcast) {
      print('Printing data for data for calendar')
      print(df_output_runsimcast)
      
      
    data_final <- df_output_runsimcast |>
        dplyr::select(date, APP) |>
        rename('app' = APP) |>
        mutate(date = as.POSIXct(date, format = "%Y-%m-%d"))
    
    
      print(data_final)
      
    data_final <- data_final |> 
        group_by(app) |> 
        dplyr::filter(row_number() == 1 | row_number() == n()) |> 
        dplyr::filter(app != 0) |> 
        summarise(start=first(date), end=first(date)) |> 
        ungroup() |> 
        mutate(category = 'allday', backgroundColor = '#FAFAFA', color="white",
               borderColor = "#FF0000",title = paste0('Application ',app))
        
      print('data final after modifications')
      print(data_final)
      
      return(data_final)
      
    }
    
    df <- data_for_calendar(tableGS2()[[2]])
    
    recommendations <- fungicide_recommendation(tableGS2()[[2]], input$res)
    
    df <- df |> 
      left_join(recommendations, by = join_by(app=='Application Number')) |> 
      rename('body'='Type of Fungicide') |> 
      mutate(backgroundColor = case_when(
        body == 'Systemic' ~ 'purple',
        body == 'Contact' ~ '#20C997'
      )) |> 
      mutate(body = paste0('Please apply a ', body,' fungicide'))
    
    toastui::calendar(df, navigation = T, defaultDate = df$start[1]) |> 
      cal_month_options(visibleWeeksCount = 8)
    
    
  })
  
  output$downloadData <- downloadHandler(
      filename = function() {
          "GEOSIMCAST.csv"
      },
      content = function(fname) {
          write.csv(tableGS2()[[2]], fname, row.names = T)
      }
  )
  
  ####################
  ####################
  ####################
  ####################
  ####################
  
  tableGS3 <- reactive({
    inFileWD <- input$fileWD
    dataWD <-
      read.csv(
        inFileWD$datapath,
        header = T,
        sep = ",",
        stringsAsFactors = F
      )
    dataWD
    
  })
  
  tableWSD <- eventReactive(input$view1, {
    tableGS3()
  })
  
  
  output$table3 <- renderDataTable({
    DT::datatable(tableWSD())
  })
  
  tableGSWD <- reactive({
    lat2 <- as.numeric(input$lat2)
    long2 <- as.numeric(input$long2)
    day02 <- input$date02
    dayn2 <- input$daten2
    hrlimite2 <- as.numeric(90)
    
    strTable1 <- tableWSD()
    strTable1$yyyymmdd <- as.character(strTable1$yyyymmdd)
    strTable1$Hour_HH <- as.character(strTable1$Hour_HH)
    strTable1[, 2] <-
      paste(strTable1$yyyymmdd, strTable1$Hour_HH, sep = "")
    strTable1$Hour_HH <-
      as.POSIXct(strTable1$Hour_HH, format = "%Y%m%d%H")
    strTable1$yyyymmdd <-
      as.POSIXct(strTable1$yyyymmdd, format = "%Y%m%d")
    
    
    date <- strTable1$yyyymmdd
    hr_hours <- aggregate(
      RH ~ yyyymmdd,
      strTable1,
      FUN = function(RH) {
        return(length(RH[RH > 90]))
      }
    )
    
    precip <- aggregate(Rain.mm. ~ yyyymmdd, strTable1, sum)
    tmeanC <- aggregate(Temp_C ~ yyyymmdd, strTable1, mean)
    
    strTable2 <-
      data.frame(hr_hours, tmeanC[1:length(tmeanC) - 1, 2], precip[1:length(precip) -
                                                                     1, 2])
    names(strTable2) <- c("date", "hr90_h", "tempC", "rainmm")
    
    runsimcast <-
      applySimcastFromString(strTable2, input$res2, 1, nrow(strTable2), T,  T)
    #print(runsimcast)
    a <-
      data.frame(strsplit(runsimcast[1], "\\s+")[[1]], stringsAsFactors = F)
    a <- as.numeric(a[2:nrow(a), ])
    print(a)
    
    
    
    b <- t(data.frame(split(a, rep(
      1:(nrow(strTable2) - 1), each = 7
    ))))
    
    resultf <- data.frame(strTable2[2:nrow(strTable2), ], b)
    names(resultf) = c("DATE",
                       "HHr",
                       "Tavg",
                       "RAIN",
                       "BU",
                       "FU",
                       "BUA",
                       "FUA",
                       "ABU",
                       "AFU",
                       "APP")
    print(resultf)
    resultf
    
  })
  
  tableGS2WD <- eventReactive(input$run2, {
    tableGSWD()
  })
  
  
  output$tableWD <- renderDataTable({
    DT::datatable(tableGS2WD())
  })
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      "GEOSIMCAST.csv"
    },
    content = function(fname) {
      write.csv(tableGSWD(), fname, row.names = T)
    }
  )
  
  ########
  
  fungicide_recomendations <- reactive({
    
    
    fungicide_recommendation(tableGS2()[[2]], input$res)
    
  })
  
  output$fungicide_table <- renderTable({
    
    req(fungicide_recomendations())
    fungicide_recomendations()
    
  })
  
  ## Change the cip logo according to light or dark theme
  
  currentTheme <- reactiveVal("light")
  
  observeEvent(input$theme_switch, {
    currentTheme(input$theme_switch)
  })
  
  output$dynamicFooterImage <- renderUI({
    
    if (currentTheme() == FALSE) {
      htmltools::img(src = 'inverted_cip_cgiar_corrected.png', style = "height: 40px;")
    } else {
      htmltools::img(src = 'cip_cgiar.png', style = "height: 40px;")
    }
    
  })
  
}
