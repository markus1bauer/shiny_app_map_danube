# Dike grasslands along River Danube
# Interactive map ####
# Markus Bauer
# 2022-03-24



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(sf)
library(ggmap)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(htmltools)
library(mapview)
library(shiny)
library(shinydashboard)
library(ggrepel)

### Start ###
rm(list = ls())
rsconnect::setAccountInfo(name='markusbauer',
               token='BB9C744354C19D1217D8FAD42760C1ED',
               secret='5XG8BIuY7IF40mc6OO7TUSMzJbZEoe4lH5Q8aEGf')

### Load data ###
sites <- read_csv(here("data", "processed", "data_processed_sites.csv"),
                 col_names = TRUE,
                 na = c("na", "NA"), col_types =
                   cols(
                     .default = "?",
                     plot = "c",
                     surveyYear = "d"
                   )
) %>%
  filter(accumulatedCov > 0)

plots <- st_read(here("data", "processed", "spatial",
                      "sites_epsg4326.shp")) %>%
  select(plot, geometry) %>%
  left_join(sites %>%
              select(plot, location, exposition, side) %>%
              group_by(plot) %>%
              slice(1),
              by = "plot") %>%
  mutate(plot = as.character(as.numeric(plot)))
  


wms_flood <- "https://www.lfu.bayern.de/gdi/wms/wasser/ueberschwemmungsgebiete?"

ffh_area <- "https://www.lfu.bayern.de/gdi/wms/natur/schutzgebiete?"

dikes <- st_read(here("data", "processed", "spatial",
                      "dikes_epsg4326.shp"))


### Create theme ###
theme_mb <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 12, color = "black", family = "Arial"),
    strip.text = element_text(size = 12),
    axis.text =
      element_text(angle = 0, hjust = 0.5, size = 12, color = "black"),
    axis.title =
      element_text(angle = 0, hjust = 0.5, size = 12, color = "black"),
    axis.line = element_line(),
    legend.key = element_rect(fill = "white"),
    legend.position = "none",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )
}



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B App #######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 1 UI #######################################################################

### a Header ------------------------------------------------------------------
header <- dashboardHeader(
  title = "Deichgruenland Donau",
  titleWidth = 300,
  tags$li(a(href = "https://github.com/markus1bauer", 
            icon("github", 
                 lib = "font-awesome",
                 height = "30px"),
            style = "padding-top:15px; padding-bottom:15px;
            padding-left:15px; padding-right:15px; font-size: 20px"),
          class = "dropdown")
)

### b Sidebar -----------------------------------------------------------------
sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    
    #### Radio buttons ####
    menuItem(
      radioButtons(
        inputId = "response", 
        label = "Zeige:",
        choices = c("Artenzahl" = "speciesRichness",
                    "Rote-Liste-Arten" = "rlgRichness",
                    #"FFH-Lebensraumtyp" = "ffh",
                    "Biotopwertpunkte" = "biotopePoints")
      )
    )
  )
)


### c Body --------------------------------------------------------------------
body <- dashboardBody(
  #tags$style(type = "text/css", "#mymap {height: calc(70vh - 80px) !important;}"),
  leafletOutput("mymap", height = "60vh"),
  plotOutput("plot", height = "32vh", width = "50vh")
  )

### d Dashboard page ---------------------------------------------------------
ui <- dashboardPage(
  header, sidebar, body,
  skin = "blue",
  tags$head(
    tags$style(
      HTML('* {font-family: "Arial"};')
      )
    )
  )


## 2 Server ###################################################################

server <- function(input, output) {
  
  ### a Set up map -------------------------------------------------------
  output$mymap <- renderLeaflet({
    
    plots %>%
    leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Esri") %>%
      setView(lng = 12.885, lat = 48.800, zoom = 11) %>%
      
      ### b Add layer control -------------------------------------------------
    addLayersControl(
      baseGroups = c("OSM", "Esri"),
      overlayGroups = c("Plots", "Dikes", "FFH Areas", "HQ100"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
      
      addScaleBar() %>%
      
      addMeasure(
        primaryLengthUnit = "kilometers",
        secondaryLengthUnit = FALSE,
        activeColor = "red",
        completedColor = "darkred",
        primaryAreaUnit = "sqkilometers",
        localization = "en"
      ) %>%
      
      addMiniMap(
        toggleDisplay = TRUE,
        tiles = providers$OSM,
        zoomLevelOffset = -5,
        minimized = FALSE
      ) %>%
    
      ### c Add layers -------------------------------------------------------
    
      #### Plots ####
    addCircleMarkers(
      data = plots,
      layerId = ~unique(plot),
      radius = 5, color = "red", weight = 2, opacity = 1, fillOpacity = .5,
      popup = ~ paste0(
        "<b>", htmlEscape(location), "</b>", "<br/>",
        "ID: ", htmlEscape(plot), "</b>", "<br/>",
        "Exposition: ", htmlEscape(exposition), "</b>", "<br/>",
        "Seite: ", htmlEscape(side)
      ),
      label = ~ paste0(location, " (ID: ", plot, ")"),
      group = "Plots"
    ) %>%
      
      #### Dikes ####
    addPolylines(
      data = dikes, color = "black", opacity = 1, weight = 2,
      label = ~ paste0(
        "Baujahr: ", htmlEscape(BAUJAHR), ", Sanierung:",
        htmlEscape(SANIERUNG)
      ),
      popup = ~ paste0(
        "Baujahr: ", htmlEscape(BAUJAHR), "<br/>",
        "Sanierung: ", htmlEscape(SANIERUNG)
      ),
      highlight = highlightOptions(weight = 2, color = "blue",
                                   opacity = .8),
      group = "Dikes"
    ) %>%
      
      #### FFH area ####
    addWMSTiles(
      baseUrl = ffh_area,
      layers = "fauna_flora_habitat_gebiet",
      group = "FFH Areas",
      options = WMSTileOptions(format = "image/png", transparent = TRUE,
                               opacity = .7)
    ) %>%
      
      #### HQ 100 ####
    addWMSTiles(
      baseUrl = wms_flood,
      layers = "hwgf_hq100",
      group = "HQ100",
      options = WMSTileOptions(format = "image/png", transparent = TRUE,
                               opacity = .7)
    ) %>%
      
      ### Hide groups ####
      hideGroup(c("Dikes", "FFH Areas", "HQ100"))
    
  })
  
  ### d Plot -------------------------------------------------------
  
  output$plot <- renderPlot({
    
    #### Data ####
    if(input$response == "speciesRichness"){
      
      data <- sites %>%
        rename(y = speciesRichness) %>%
        select(location, plot, surveyYear, y, biotopeType)
      
    } else {
      
      if(input$response == "biotopePoints") {
        
        data <- sites %>%
          rename(y = biotopePoints) %>%
          select(location, plot, surveyYear, y, biotopeType)
        
      } else {
        
        data <- sites %>%
          rename(y = rlgRichness) %>%
          select(location, plot, surveyYear, y, biotopeType)
        
      }
    }
    
    temp <- reactive({
      
      id <- input$mymap_marker_click$id
      data %>%
        filter(plot %in% id)
      
    })
    
    #### Create Plot ####
    if(input$response == "speciesRichness"){
      
      ggplot(data = temp(), aes(x = surveyYear, y = y,
                                label = biotopeType)) +
        geom_text_repel(
          nudge_y      = Inf,
          direction    = "x",
          segment.size = .1,
          segment.color = "grey80"
        ) +
        geom_line() +
        geom_point() +
        scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) +
        scale_x_continuous(limits = c(min(sites$surveyYear),
                                      max(sites$surveyYear))) +
        labs(x = "Aufnahmejahr",
             y = "Artendichte (25 m²)",
             title = paste0(htmlEscape(temp()$location[1]),
                            ". ID: ", htmlEscape(temp()$plot[1]))) +
        theme_mb()
      
    } else {
      
      if(input$response == "biotopePoints") {
        
        ggplot(data = temp(), aes(x = surveyYear, y = y,
                                  label = biotopeType)) +
          annotate("rect",
                   xmin = min(sites$surveyYear),
                   xmax = max(sites$surveyYear),
                   ymin = 11.5, ymax = 15,
                   alpha = .4, fill = "green3"
          ) +
          annotate("rect",
                   xmin = min(sites$surveyYear),
                   xmax = max(sites$surveyYear),
                   ymin = 8.5, ymax = 11.5,
                   alpha = .4, fill = "green"
          ) +
          annotate("rect",
                   xmin = min(sites$surveyYear),
                   xmax = max(sites$surveyYear),
                   ymin = 0, ymax = 8.5,
                   alpha = .4, fill = "red"
          ) +
          geom_text_repel(
            nudge_y      = Inf,
            direction    = "x",
            segment.size = .1,
            segment.color = "grey60"
          ) +
          geom_line() +
          geom_point() +
          scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 1),
                             expand = expansion(add = c(0, 1))
                             ) +
          scale_x_continuous(limits = c(min(sites$surveyYear),
                                        max(sites$surveyYear))) +
          labs(x = "Aufnahmejahr",
               y = "Biotopwertpunkte",
               title = paste0(htmlEscape(temp()$location[1]),
                              ". ID: ", htmlEscape(temp()$plot[1]))) +
          theme_mb()
        
      } else {
        
        ggplot(data = temp(), aes(x = surveyYear, y = y,
                                  label = biotopeType)) +
          geom_text_repel(
            nudge_y      = Inf,
            direction    = "x",
            segment.size = .1,
            segment.color = "grey80"
          ) +
          geom_line() +
          geom_point() +
          scale_y_continuous(limits = c(0, 8), breaks = seq(0, 15, 1)) +
          scale_x_continuous(limits = c(min(sites$surveyYear),
                                        max(sites$surveyYear))) +
          labs(x = "Aufnahmejahr",
               y = "Artendichte Rote Liste Deutschland (25 m²)",
               title = paste0(htmlEscape(temp()$location[1]),
                              ". ID: ", htmlEscape(temp()$plot[1]))) +
          theme_mb()
        
      }
    }
    
    
  })
}


## 3 Run app ################################################################
#shinyApp(ui, server)
runApp(shinyApp(ui, server), launch.browser = TRUE)
