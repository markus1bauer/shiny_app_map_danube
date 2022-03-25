# Dike grasslands along River Danube
# Interactive map ####
# Markus Bauer
# 2022-03-24



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ##########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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

### Start ###
rm(list = ls())
setwd(here("data", "spatial"))

### Load data ###
data <- read_csv(here("data", "data_processed_sites_spatial.csv"),
                  col_names = TRUE,
                  na = c("na", "NA"), col_types =
                    cols(
                      .default = "?",
                      id = "f",
                      locationAbb = "f",
                      block = "f",
                      plot = "f",
                      exposition = "f",
                      side = "f",
                      #ffh = "f",
                      locationYear = "f"
                    )
                 )
sites <- st_read("sites_epsg4326.shp")
wms_flood <- "https://www.lfu.bayern.de/gdi/wms/wasser/ueberschwemmungsgebiete?"
ffh_area <- st_read("ffh_area_epsg4326.shp")
dikes <- st_read("dikes_epsg4326.shp")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create map ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


map <- sites %>%
  
  
  ## 1 Set up for map #######################################################

  leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Esri") %>%
  setView(lng = 12.885, lat = 48.839, zoom = 10) %>%
  
  
  ## 2 Layer control ##########################################################

  addLayersControl(
    baseGroups = c("OSM", "Esri"),
    overlayGroups = c("FFH Areas", "Dikes", "Plots", "HQ100"),
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
    zoomLevelOffset = -10,
    minimized = FALSE
  ) %>%

  
  ## 3 Add layers #############################################################

  ### a FFH area --------------------------------------------------------------
  addPolygons(
    data = ffh_area,
    color = "grey60", opacity = 0, weight = 0,
    fillColor = "grey60", fillOpacity = .6,
    highlight = highlightOptions(
      weight = 2, color = "blue",
      opacity = .8, bringToFront = FALSE
    ),
    popup = ~ paste0(
      "<b>", htmlEscape(NAME), "</b>", "<br/>",
      "ID: ", htmlEscape(ID)
    ),
    group = "FFH Areas"
  ) %>%
  
  ### b HQ 100 ----------------------------------------------------------------
  addWMSTiles(
    baseUrl = wms_flood,
    layers = "hwgf_hq100",
    group = "HQ100",
    options = WMSTileOptions(format = "image/png", transparent = TRUE)
  ) %>%
  
  ### c Dikes ----------------------------------------------------------------
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
  
  ### d Plots ----------------------------------------------------------------
  addCircleMarkers(
    radius = 5, color = "red", weight = 2, opacity = 1,
    fillOpacity = .5,
    popup = ~ paste0(
      "<b>", htmlEscape(locatin), "</b>", "<br/>",
      "ID: ", htmlEscape(id)
    ),
    label = ~ paste0(locatin, " (ID: ", id, ")"),
    group = "Plots"
  )


## 4 Save map ################################################################

mapshot(map, url = here("outputs", "interactive_map_danube.html"))
