library(shiny)
library(bs4Dash)
# library(shinydashboard)
library(toastui)
library(shinycustomloader)
library(DT)
library(gdata)
library(DBI)
library(RMySQL)
library(maps)
library(ggmap)
# library(rgdal)
library(shinyTree)
library(dismo)
library(rJava)
library(leaflet)
library(rsconnect)
library(shiny.i18n)
library(leaflet.extras)
library(data.table)
# library(maptools)
library(svglite)
library(slickR)
library(openair)
library(shinyalert)
library(bslib)
library(htmltools)
library(tidyverse)
library(tippy)
library(fresh)
library(bsicons)

source("./fungicide.R")

#####

ui <- bs4Dash::dashboardPage(
  
  freshTheme = fresh::create_theme(
    
    bs4dash_status(light = "#005475",primary = "#00755c"),
    bs4dash_color(
      gray_900 = "black",
      white = "#FAF9F5"
    ),
    bs4dash_yiq(
      contrasted_threshold = 10,
      text_dark = "#FFF",
      text_light = "#272c30"
    ),
    bs4dash_sidebar_light(
      
      color = "black",
      hover_color = "#005475",
      submenu_color = "black",
      submenu_hover_color = "black"
    )
  ),
  options = NULL,
  
  #CABECERA DE PAGINA
  dashboardHeader(
    title = tags$a(href = 'https://github.com/CIP-RIU/LateBlight',
                   tags$img(
                     src = 'logo.png',
                     width = "200px",
                     height = "50px"
                   ))
  ),
  
  #BARRA LATERAL
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("th")),
    
    menuItem("About", tabName = "about", icon = icon("info-circle")),
    
    # \u2007 es para un espacio en blanco
    menuItem("\u2007Github",
             icon = icon("github"),
             href = "https://github.com/CIP-RIU/LateBlight")
    
  )),
  
  
  bs4Dash::dashboardBody(
    # Obtener mediante Javascript el ancho y largo de sesion
    tags$head(
      tags$script(
        '
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            '
      ),
      
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                bs4Dash::tabBox(
                  title = "POTATO LATE BLIGHT - DSS",
                  id = "tabset1",
                  width = 12,
                  
                  
                  #INGRESAR LOS ARCHIVOS DE ENTRADA
                  tabPanel(
                    title = "INPUT DATA",
                    icon  =  icon ("file-import"),
                    
                    fluidRow(
                      column(
                        12,
                        h3("Crop information"),
                        br(),
                        column(5,
                               dateInput("date0", "Emergence date:", Sys.Date() -
                                           7)),
                        column(
                          5,
                          dateInput("daten", "Forecast date:", Sys.Date()-1, max = Sys.Date() + 14)
                        ),
                        column(2)
                        
                      ),
                      
                      column(12,
                             column(
                               5,
                               selectInput(
                                 "country_var",
                                 "Select your country:",
                                 c("Peru" = "per", "Ecuador" = "ecu", "India"="india"),
                                 selectize = FALSE
                               )
                             ),
                             
                             column(5),
                             
                             column(2)),
                      
                      column(12,
                             column(5,
                                    uiOutput("var_names1")),
                             
                             column(5),
                             
                             column(2))
                    ),
                    
                    fluidRow(column(
                      12,
                      h3("Area of interest"),
                      br(),
                      fluidRow(
                        bs4Dash::box(
                          width = 6,
                          
                          h4("Select only one location(s) of interest on the map"),
                          h5("Preferably center your location"),
                          br(),
                          DT::dataTableOutput("inputMarkers")
                          
                        ),
                        
                        bs4Dash::box(
                          width = 6,
                          #h4("MAP" ,align = "center"), height = "750px",
                          leafletOutput("mymap1a"),
                          #br(),
                          actionButton("clearMap1a", "Clear", class =
                                         "btn-warning", width = "120px")
                        )
                        
                      )
                      
                    ))
                    
                    
                    
                    
                  ),
                  
                  #PANEL PARA VISUALIZAR Y DESCARGAR LOS DATOS
                  tabPanel(
                    "RESULTS",
                    icon  =  icon ("poll"),
                    style = "overflow-x: scroll" ,
                    actionButton("run1", " Run model by Weather API", icon = icon ("play")),
                    
                    br(),
                    br(),
                    toastui::calendarOutput(outputId = 'calendars'),
                    # withLoader(
                    #   #br(),
                    #   slickROutput("calendars", width =
                    #                  "90%"),
                    #   type = "html",
                    #   loader = "loader4"
                    # ),
                    br(),
                    br(),
                    br(),
                    downloadButton("downloadData", " Download data")
                  ),
                  
                  tabPanel(
                    title='RECOMMENDATIONS',
                    layout_columns(
                      card(full_screen = T,
                           card_header('Fungicide Applications',
                                       class = "d-flex justify-content-between"),
                           card_body(tableOutput(outputId = 'fungicide_table'),
                             class = "align-items-center")
                           ),
                      card(
                        bslib::value_box('Caution',value = markdown("Dont use the same systemic fungicide more than **four times**."),
                                         showcase = bs_icon("exclamation-octagon"),
                                         theme_color = 'danger'),
                        card(
                          full_screen = T,
                          card_header('Systemic'),
                          bslib::value_box('Systemic fungicide',value = 'Pesticide',
                                           showcase = bs_icon("prescription"),
                                           theme_color = 'purple')
                        ),
                        card(
                          full_screen = T,
                          card_header('Contact'),
                          bslib::value_box('Contact fungicide',value = 'Pesticide',
                                           showcase = bs_icon("prescription"),
                                           theme_color = 'teal')
                        )
                      )
                    )
                  )
                )
                
                
              )),
      
      
      tabItem(
        tabName = "about",
        fluidRow(column(
          width = 12,
          bs4Jumbotron(
            title = markdown(
              'Welcome to *Digital Decision Support Tool for Late Blight Management*'
            ),
            lead = 'The tool was designed to promote efficient and sustainable pesticide use, while also providing educational resources and adaptive management strategies. This tool aimed to not only monitor and predict late blight outbreaks but also to offer actionable, real-time advice to farmers.',
            btnName = 'Visit GitHub Repository',
            href = 'https://github.com/CENTRO-INTERNACIONAL-DE-LA-PAPA/Lateblight',
            status = 'success'
          )
        )),
        fluidRow(
          column(12, 
                 bs4Callout(title = 'Authors', status = 'info',width = 6,markdown('Henry Juarez, Piero Palacios, Willmer Perez, Marvin Quispe and Jorge Andrade')),
                 bs4Callout(title = 'Contact', status = 'info', width = 6, 'h.juarez@cgiar.org')
                 )
        )
        
        
      )
      
      
      
    )
    
    
  ),
  footer = dashboardFooter(
    left =  card(
      full_screen = F,
      uiOutput('dynamicFooterImage'),
      card_body(
        fill = FALSE,
        p('CIP thanks all donors and organizations that globally support its work through their contributions to the CGIAR Trust Fund.'),
        p('This publication is copyrighted by the International Potato Center (CIP). It is licensed for use under the Creative Commons Attribution 4.0 International License.'),
      ),
      card_footer(
        bs_icon('badge-cc'),bs_icon('person-standing')
      )
    ),
    fixed = F
  )
  
  
)















