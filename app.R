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

### Start ###
rm(list = ls())
setwd(here("data", "spatial"))
rsconnect::setAccountInfo(name='markusbauer',
               token='BB9C744354C19D1217D8FAD42760C1ED',
               secret='5XG8BIuY7IF40mc6OO7TUSMzJbZEoe4lH5Q8aEGf')

### Load data ###
data <- read_csv(here("data", "data_processed_sites_spatial.csv"),
                 col_names = TRUE,
                 na = c("na", "NA"), col_types =
                   cols(
                     .default = "?",
                     plot = "c"
                   )
) %>%
  select(plot, location, exposition, side,
         surveyYear, constructionYear,
         speciesRichness) %>%
  pivot_wider(id_cols = c("plot", "location", "exposition", "side",
                          "constructionYear"),
              names_from = surveyYear,
              names_prefix = "sr_",
              values_from = speciesRichness)
sites <- st_read("sites_epsg4326.shp") %>%
  select(plot, geometry) %>%
  mutate(plot = as.character(as.numeric(plot))) %>%
  right_join(data, by = "plot")
wms_flood <- "https://www.lfu.bayern.de/gdi/wms/wasser/ueberschwemmungsgebiete?"
ffh_area <- st_read("ffh_area_epsg4326.shp")
dikes <- st_read("dikes_epsg4326.shp")

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
  title = "Deichgrünland Donau",
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
                    #"FFH-Lebensraumtyp" = "ffh",
                    "Biotopwertpunkte" = "biotopePoints")
      )
    )
  )
)


### c Body --------------------------------------------------------------------
body <- dashboardBody(
  tags$style(
    type = "text/css", "#mymap {height: calc(70vh - 80px) !important;}"
    ),
  leafletOutput("mymap")
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
  
  output$mymap <- renderLeaflet({
    sites %>%
      
      ### a Set up map -------------------------------------------------------
    leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Esri") %>%
      setView(lng = 12.885, lat = 48.839, zoom = 10) %>%
      
      ### b Add layer control -------------------------------------------------
    addLayersControl(
      baseGroups = c("OSM", "Esri"),
      overlayGroups = c("Plots", "HQ100"),
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
    #### HQ 100 ####
    addWMSTiles(
      baseUrl = wms_flood,
      layers = "hwgf_hq100",
      group = "HQ100",
      options = WMSTileOptions(format = "image/png", transparent = TRUE)
    ) %>%
      
      #### Plots ####
    addCircleMarkers(
      radius = 5, color = "red", weight = 2, opacity = 1,
      fillOpacity = .5,
      popup = ~ paste0(
        "<b>", htmlEscape(location), "</b>", "<br/>",
        "ID: ", htmlEscape(plot), "</b>", "<br/>",
        "Baujahr Deich:", htmlEscape(constructionYear)
      ),
      label = ~ paste0(location, " (ID: ", plot, ")"),
      group = "Plots"
    ) %>%
      
      hideGroup(c("HQ100"))
    
    
  })
}


## 3 Run app ################################################################

runApp(shinyApp(ui, server), launch.browser = TRUE)
