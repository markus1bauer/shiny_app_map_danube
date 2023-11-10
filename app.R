# Dike grasslands along River Danube
# Interactive map as Shiny app ####
# Markus Bauer
# 2023-11-03



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(rlang)
library(dplyr)
library(here)
library(tidyverse)
library(sf)
library(ggmap)
library(ggrepel)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(htmltools)
library(mapview)
library(rsconnect)
library(shiny)
library(shinydashboard)
library(renv)

### Start ###
rm(list = ls())
#renv::status()
#installr::updateR(browse_news = FALSE, install_R = TRUE, copy_packages = TRUE, copy_Rprofile.site = TRUE, keep_old_packages = TRUE, keep_install_file = TRUE, update_packages = TRUE, start_new_R = FALSE, quit_R = TRUE, print_R_versions = TRUE, GUI = TRUE)
#sessionInfo()

rsconnect::setAccountInfo(
  name = 'markusbauer',
  token = 'BB9C744354C19D1217D8FAD42760C1ED',
  secret = '5XG8BIuY7IF40mc6OO7TUSMzJbZEoe4lH5Q8aEGf'
  )

### Load data ###
sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE,
  na = c("na", "NA"),
  col_types =
    cols(
      .default = "?",
      plot = "c",
      surveyYear = "d"
    )
)

plots <- st_read(
  here("data", "processed", "spatial", "sites_epsg4326.shp")
  ) %>%
  select(plot, geometry) %>%
  left_join(
    sites %>%
      select(plot, location, exposition, side) %>%
      group_by(plot) %>%
      slice(1),
    by = "plot"
  ) %>%
  mutate(plot = as.character(as.numeric(plot)))
  


wms_flood <- "https://www.lfu.bayern.de/gdi/wms/wasser/ueberschwemmungsgebiete?"

ffh_area <- "https://www.lfu.bayern.de/gdi/wms/natur/schutzgebiete?"

st_layers(here("data", "processed", "spatial", "sites_epsg4326.shp"))

dikes <- st_read(here("data", "processed", "spatial", "dikes_epsg4326.shp")) %>%
  select(
    BAUJAHR, SANIERUNG, GWO_L, L_GEW_KNZ, LKR_L, GEM_L, WWA_L, BETREIBE_L,
    LAGE_FLI_L, geometry
    )


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
  tags$li(a(href = "https://github.com/markus1bauer/shiny_app_map_danube", 
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
                    "Rote Liste Deutschland" = "rlgRichness",
                    "Biotopwertpunkte" = "biotopePoints")
      )
    )
  )
)


### c Body --------------------------------------------------------------------

body <- dashboardBody(
  leafletOutput("mymap", height = "55vh"),
  fluidRow(
    box(plotOutput("plot", height = "34vh", width = "55vh")),
    box(htmlOutput("text"))
    )
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
      )
  })
  


## 3 Run app ################################################################


shinyApp(ui, server)
rsconnect::showLogs()

