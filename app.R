#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(RSocrata)
library(sf)
library(tmap)

years_ago <- today() - years(2)
crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")

crash_raw <- as_tibble(read.socrata(crash_url))

crash <- crash_raw %>%
  arrange(desc(crash_date)) %>%
  transmute(
    injuries = if_else(injuries_total > 0, "Com feridos", "Sem feridos"),
    crash_date,
    crash_hour,
    report_type = if_else(report_type == "", "UNKNOWN", report_type),
    num_units,
    posted_speed_limit,
    weather_condition,
    lighting_condition,
    roadway_surface_cond,
    first_crash_type,
    trafficway_type,
    prim_contributory_cause,
    latitude, longitude
  ) %>%
  na.omit()

first_crash_type <- levels(as.factor(crash$first_crash_type))

comm_file <- "https://data.cityofchicago.org/resource/igwz-8jzy.geojson"
chicago_comm <- read_sf(comm_file)

chicago_comm <- st_transform(chicago_comm,32616)


ui <-fluidPage(
  
  titlePanel("Acidentes de carro em Chicago"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = 'tipo', 
                  label = 'Tipo de acidente', 
                  choices = c("Com feridos", "Sem feridos"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Pontos",plotOutput(outputId = "map")), 
        tabPanel("Área", plotOutput(outputId = "map2")) 
      #  tabPanel("Sobre", textOutput("sobre"))
      )
    )
  )
)
  


server <- function(input, output, session) {
  
  selectedData <- reactive(
    
    crash %>% 
      filter(injuries == input$tipo)%>%
      
      filter(latitude > 0) 
    #%>% 
     # filter(first_crash_type == input$tipo_acidente)
  )

  
  
  output$map <- renderPlot({
    
    
    
    selectedData()  %>%
      ggplot(aes(longitude, latitude, color = 'red')) +
      geom_point(size = 0.5, alpha = 0.4) +
      labs(color = NULL) +
      scale_color_manual(values = c("deeppink4", "gray80")) +
      coord_fixed() + theme_void() + theme(legend.position = "none", axis.text.x=element_blank(),
                                              axis.ticks.x=element_blank(),
                                              axis.text.y=element_blank(),
                                              axis.ticks.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
     
    
    
  }, height = 800, width = 1200  )
  
  
  output$map2 <- renderPlot({
    
    crash_point <- st_as_sf(selectedData(), coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
    crash_point <- st_transform(crash_point,32616)
    comm_pnts <- st_join(crash_point,chicago_comm["area_num_1"])
    comm_pnts$area_num_1 <- as.integer(comm_pnts$area_num_1)
    chicago_comm$area_num_1 <- as.integer(chicago_comm$area_num_1)
    st_geometry(comm_pnts) <- NULL
    
    crash_com <- comm_pnts %>% count(area_num_1)
    crash_com <- crash_com %>% rename(comm = area_num_1, 'Total' = n)
    
    chicago_comm <- left_join(chicago_comm,crash_com, by = c("area_num_1" = "comm"))
    
    tm_shape(chicago_comm, max.plot = 12) +
      tm_polygons("Total") + 
      tm_layout(frame = FALSE)
    
  }, height = 800, width = 1200  )
  
  
}



shinyApp(ui, server)