## ________________________________________________________________________

## Title:    No-go map development
## Purpose:  Draw leaflet map
## Author:   Dominic Henry
## Date:     02/09/2020

## Libraries
library(sf)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(leafem)
## ________________________________________________________________________

# Increase upload file size -----------------------------------------------
maxsize_MB <- 30 # megabytes
options(shiny.maxRequestSize = maxsize_MB*1024^2)

# Load data ---------------------------------------------------------------
load("data_input/spatial_data_inputs.RData")
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# crane_points <- crane_points %>%
#   distinct(geometry, .keep_all = TRUE)

# Load functions ----------------------------------------------------------

## Add if necessary

# Base layers -------------------------------------------------------------

## TRY SOME OF THESE BASEMAPS
# http://leaflet-extras.github.io/leaflet-providers/preview/

nogo_basemap <- leaflet(
  high_sens_uni
) %>%
  # addTiles() %>% # Open maps (default)
  addEsriBasemapLayer(
  key = esriBasemapLayers$Topographic,
  options = list(detectRetina = TRUE),
  group = "Topographic"
  ) %>%
  addEsriBasemapLayer(  ## See https://esri.github.io/esri-leaflet/api-reference/layers/basemap-layer.html
    key = esriBasemapLayers$Streets,
    options = list(detectRetina = TRUE),
    group = "Streets"
  ) %>%
  addEsriBasemapLayer(
    key = esriBasemapLayers$Imagery,
    options = list(detectRetina = TRUE),
    group = "Imagery"
  ) %>%
  addScaleBar(
    position = "bottomleft"
  ) %>%
  addMapPane("farm_polys", zIndex = 410) %>% # Farms will plot beneath no-go polys
  addMapPane("erf_polys", zIndex = 415) %>%
  addMapPane("nogo_polys", zIndex = 420) %>% # No-go polys will plot above farms
  addPolygons(
    data = high_sens_uni,
    group = "No-go areas",
    popup = "NO-GO AREA",
    label = "NoGO",
    fillColor = "red",
    fillOpacity = 0.75,
    stroke = TRUE,
    color = "black",
    weight = 0.8,
    smoothFactor = 1.5,
    options = pathOptions(pane = "nogo_polys")
  )


# Farm portions -----------------------------------------------------------
nogo_basemap <- nogo_basemap %>%
  addPolygons(
    data = farms,
    group = "Farm portions",
    popup =  ~ str_c(MAJ_REGION, " - PARCEL: ",PARCEL_NO),
    label =  ~ str_c(MAJ_REGION, " - PARCEL: ",PARCEL_NO),
    fillColor = "blue",
    fillOpacity = 0.4,
    stroke = TRUE,
    color = "black",
    weight = 0.8,
    smoothFactor = 3,
    options = pathOptions(pane = "farm_polys")
  )

# ERFs --------------------------------------------------------------------
nogo_basemap <- nogo_basemap %>%
  addPolygons(
    data = erf,
    group = "ERFs",
    popup =  "ERF property (click for details)",
    # popup =  ~ str_c(MAJ_REGION, " - PARCEL: ",PARCEL_NO),
    # label =  ~ str_c(MAJ_REGION, " - PARCEL: ",PARCEL_NO),
    fillColor = "yellow",
    fillOpacity = 0.4,
    stroke = TRUE,
    color = "black",
    weight = 0.8,
    smoothFactor = 3,
    options = pathOptions(pane = "erf_polys")
  )


# Protected areas + RSA Border --------------------------------------------
nogo_basemap <- nogo_basemap %>%
  addPolygons(
    data = pa,
    group = "Protected areas",
    popup = ~ CUR_NME,
    fillColor = "lime",
    fillOpacity = 0.4,
    stroke = TRUE,
    color = "black",
    weight = 0.8,
    smoothFactor = 2
  )


nogo_basemap <- nogo_basemap %>%
  addPolygons(
    data = za,
    group = "RSA border",
    fill = FALSE,
    stroke = TRUE,
    color = "black",
    weight = 1.2,
    smoothFactor = 2
  )


# Layers control ----------------------------------------------------------
nogo_basemap <- nogo_basemap %>%
  addLayersControl(
    baseGroups = c("Topographic",
                   "Streets",
                   "Imagery"
                   ),
    overlayGroups= c("No-go areas","Protected areas","Farm portions","ERFs"),
    options = layersControlOptions(collapsed=FALSE)
  ) %>%
  addLegend("bottomright",
            colors = c("red","lime","blue","yellow"),
            labels = c("No-Go","PA","Farm portion","ERF"),
            title = "Legend",
            opacity = 0.6
  ) %>%
  hideGroup("ERFs") %>%
  hideGroup("Farm portions") %>%
  setView(
    lng = 25.4015133,
    lat = -29.1707702,
    zoom = 6
  ) %>%
  addResetMapButton() %>%
  leafem::addMouseCoordinates() #%>%
  # leafem::addLogo(img = "data_input/ewt_01.png",
  #                 src = "local",
  #                 url = "https://www.ewt.org.za/",
  #                 position = "topleft",
  #                 offset.x = 50,
  #                 width = 60)

# nogo_basemap
# rm(list = ls()[!(ls() %in% c('nogo_basemap'))])
# save.image("data_input/nogo_base.RData")
