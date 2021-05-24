## Libraries ----
library(sf)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(leaflet)
library(rgdal)
library(shinythemes)
library(DT)
library(leaflet.extras)
library(leaflet.esri)
library(leafem)
library(gt)
library(log4r)

## Choose geom complexity
geom_complex = "SIMPLE"
# geom_complex = "COMPLEX"

latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## Intiate logger ----
log_file <- "nogo-map-logging.log"
file_logger <- logger("INFO", appenders = list(file_appender(log_file),console_appender()))
info(file_logger, "### START NEW GLOBAL SESSION ###")

## Load data ----
info(file_logger, "Start RData import")

if(geom_complex == "SIMPLE"){
  load("data_input/spatial_data_inputs_SIMPLE.RData")
} else if (geom_complex == "COMPLEX"){
  load("data_input/spatial_data_inputs_COMPLEX.RData")
  }

info(file_logger, "Finish RData import")

sgdata <- c(unique(farms$ID),unique(erfs$ID))

## Increase upload file size ----
maxsize_MB <- 30 # megabytes
options(shiny.maxRequestSize = maxsize_MB * 1024^2)

# Profiling ----

# https://lukesingham.com/shiny-r-performance-profiling/
# profvis::profvis({ runApp(appDir = getwd())},
#                  prof_output = "nogo_profvis.Rprof")

# renv ----

# renv::init()
# renv::snapshot()
# renv::restore()
# Use renv::history() to view past versions of renv.lock that have been committed to your repository
# Use renv::revert() to pull out an old version of renv.lock based on the previously-discovered commit
# pkg_check <- installed.packages()


# Leaflet map settings ----
opacity_cols <- 0.5
overlay_grp_names <-  c("No-go areas","Protected areas","Farm portions","ERFs")
layer_cols <- c("#EE2C2C", "#A2CD5A", "#1E90FF", "#FFB90F")
# ("firebrick2", "darkolivegreen3", "dodgerblue", "darkgoldenrod1")

## Compile base map -----

### Add basemap layers and controls ----
info(file_logger, "Start Basemap compilation 1")

global_base_map <- leaflet() %>%
  addEsriBasemapLayer(
    key = esriBasemapLayers$Topographic,
    options = list(detectRetina = TRUE),
    group = "Topographic"
  ) %>%
  addEsriBasemapLayer(  ## Note 1
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
  addLayersControl(
    baseGroups = c("Topographic",
                   "Streets",
                   "Imagery"
    ),
    options = layersControlOptions(collapsed=FALSE)
  ) %>%
  setView(lng = 25.4015133,
          lat = -29.1707702,
          zoom = 6)

### Add logo and draw toolbar ----
info(file_logger, "Start Basemap compilation 2")
global_base_map <- global_base_map %>%
  leafem::addLogo(img = "ewt_01.png", # Note 5
                  src = "remote",
                  url = "https://www.ewt.org.za/",
                  position = "bottomleft",
                  offset.x = 5,
                  offset.y = 40,
                  width = 70) %>%
  addResetMapButton() %>%
  leafem::addMouseCoordinates() %>%
  addDrawToolbar(polylineOptions = FALSE,
                 polygonOptions = drawPolygonOptions(
                   shapeOptions = drawShapeOptions(color = "#A0522D",
                                                   fillColor = "#A0522D")
                 ),
                 rectangleOptions = drawRectangleOptions(
                   shapeOptions = drawShapeOptions(color = "#A0522D",
                                                   fillColor = "#A0522D")
                 ),
                 markerOptions = FALSE,
                 circleMarkerOptions = FALSE,
                 circleOptions = FALSE,
                 editOptions = editToolbarOptions())

### Add cadastral and EST data ----
info(file_logger, "Start Basemap compilation 3")
global_base_map <- global_base_map %>%
  addLayersControl(
    baseGroups = c("Topographic",
                   "Streets",
                   "Imagery"
    ),
    overlayGroups= overlay_grp_names,
    options = layersControlOptions(collapsed=FALSE)
  ) %>%
  addMapPane("farm_polys", zIndex = 410) %>% # Farms plot beneath no-go polys
  addMapPane("erf_polys", zIndex = 415) %>%
  addMapPane("nogo_polys", zIndex = 420) %>% # No-go plot above farms
  addPolygons(
    data = farms,
    group = "Farm portions",
    popup =  ~ str_c(MAJ_REGION, " - PARCEL: ",PARCEL_NO),
    label =  ~ str_c(MAJ_REGION, " - PARCEL: ",PARCEL_NO),
    fillColor = layer_cols[3],
    fillOpacity = opacity_cols,
    stroke = TRUE,
    color = "black",
    weight = 0.8,
    smoothFactor = 3,
    options = pathOptions(pane = "farm_polys")
  ) %>%
  addPolygons(
    data = erfs,
    popup =  ~ str_c(MAJ_REGION, " - PARCEL: ",PARCEL_NO),
    label =  ~ str_c(MAJ_REGION, " - PARCEL: ",PARCEL_NO),
    group = "ERFs",
    fillColor = layer_cols[4],
    fillOpacity = opacity_cols,
    stroke = TRUE,
     color = "black",
     weight = 0.8,
     smoothFactor = 3,
     options = pathOptions(pane = "erf_polys")
   ) %>%
  addPolygons(
    data = protect_area,
    group = "Protected areas",
    popup = ~ CUR_NME,
    fillColor = layer_cols[2],
    fillOpacity = opacity_cols,
    stroke = TRUE,
    color = "black",
    weight = 0.8,
    smoothFactor = 3
  ) %>%
  addPolygons(
    data = nogo,
    popup = ~ SENSFEAT,
    label = "No-go area",
    group = "No-go areas",
    fillColor = layer_cols[1],
    fillOpacity = opacity_cols,
    stroke = TRUE,
    color = "black",
    weight = 0.8,
    smoothFactor = 1.5,
    options = pathOptions(pane = "nogo_polys")
  ) %>%
  addLegend("bottomright",
            colors = layer_cols,
            labels = c("No-Go","PA","Farm portion","ERF"),
            title = "Legend",
            opacity = opacity_cols
  ) %>%
  hideGroup("Protected areas") %>%
  hideGroup("ERFs") %>%
  hideGroup("Farm portions")

## Helper functions ----
info(file_logger, "Load helper functions")

set_zoom <- function(x){

  if(x > 50){
    zoom_lev <- 9
  } else if (x > 40 & x < 50){
    zoom_lev <- 10
  } else if (x > 30 & x < 40){
    zoom_lev <- 11
  } else if (x > 20 & x < 30){
    zoom_lev <- 12
  } else if (x > 10 & x < 20){
    zoom_lev <- 13
  } else if (x > 4 & x < 10){
    zoom_lev <- 14
  } else if (x < 4){
    zoom_lev <- 15
  }

  return(zoom_lev)
}


compile_species_table <- function(x){

  x %>%
    st_drop_geometry() %>%
    group_by(SENSFEAT, THEME) %>%
    tally() %>%
    rename(Species = SENSFEAT, 'EST Theme' = THEME,
           'Polygon count' = n) %>%
    select(Species,everything()) %>%
    ungroup()


}

compile_property_table <- function(x, prop_type){

  prop_df <- x %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    select(-GID) %>%
    select(PRCL_KEY, PRCL_TYPE, ID, PROVINCE, MAJ_REGION, MAJ_CODE, PARCEL_NO, PORTION) %>%
    mutate_all(as.character)

  prop_df <- prop_df %>%
    mutate(prop = str_c("property_", 1:nrow(.))) %>%
    pivot_longer(cols =-prop,
                 names_to = prop_type,
                 values_to = "value") %>%
    pivot_wider(names_from = prop,
                values_from = value) %>%
    rename_with(., ~str_to_sentence(str_replace_all(., "_", " ")), starts_with("prop")) %>%
    rename("Property attribute" = 1) %>%
    mutate(`Property attribute` = c("Parcel key", "Parcel type","21 digit ID",
                                    "Province","Major region name",
                                    "Major region code","Parcel number",
                                    "Portion of parcel"))

}

draw_gt <- function(x){

  x %>%
    gt() %>%
    tab_header(
      title = md("List of species flagged in No-Go property"),
      subtitle = ""
    ) %>%
    tab_footnote(footnote = "Environmental Screening Tool",
                 locations = cells_column_labels(
                   columns = vars(`EST Theme`)
                 )
    ) %>%
    tab_style(
      style = list(
        cell_text(style = "italic")
      ),
      locations = cells_body(
        columns = vars(Species)
      )
    )

}


draw_gt_property <- function(x){

  x %>%
    gt() %>%
    tab_header(
      title = md("List of properties that intersect with species No-go polygons"),
      subtitle = ""
    ) %>%
    tab_style(
      style = list(
        cell_text(style = "italic")
      ),
      locations = cells_body(
        columns = vars(`Property attribute`)
      )
    ) %>%
    opt_align_table_header(align = "left") %>%
    tab_style(
      style = cell_borders(
        sides = "right",
        color = "#BBBBBB",
        weight = px(1.5),
        style = "solid"
      ),
      locations = cells_body(
        columns = "Property attribute"
      )
    )

}


