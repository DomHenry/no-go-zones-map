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
library(leaflet.extras)
library(leaflet.esri)
library(leafem)
library(gt)
sf_use_s2(FALSE)

# DOCKER VERSION 4.1

latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## Load data ----
load("data_input/spatial_data_inputs.RData")
sgdata <- c(unique(farms$ID),unique(erfs$ID))

## Increase upload file size ----
maxsize_MB <- 30 # megabytes
options(shiny.maxRequestSize = maxsize_MB * 1024^2)

# Leaflet map settings ----
opacity_cols <- 0.5
overlay_grp_names <-  c("No-go areas","Protected areas","Farm portions","ERFs")
layer_cols <- c("#EE2C2C", "#A2CD5A", "#1E90FF", "#FFB90F")
# ("firebrick2", "darkolivegreen3", "dodgerblue", "darkgoldenrod1")

## Compile base map -----

### Add basemap layers and controls ----
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
global_base_map <- global_base_map %>%
  leafem::addLogo(img = "info.png", # Note 5
                  src = "remote",
                  url = "https://www.ewt.org.za/no-go-threatened-species-mapping-tool/",
                  position = "bottomleft",
                  offset.x = 5,
                  offset.y = 40,
                  width = 40,
                  height = 40) %>%
  leaflet.extras::addResetMapButton() %>%
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
  addMapPane("protect_area_polys", zIndex = 417) %>%
  addMapPane("nogo_polys", zIndex = 420) %>% # No-go plot above farms
  leafem:::addFgb(
    file = "fgb_data/farms/farms.fgb",
    group = "Farm portions",
    label =  "leaf_popup",
    popup = "leaf_popup",
    fillColor = layer_cols[3],
    fill = TRUE,
    fillOpacity = opacity_cols,
    stroke = TRUE,
    color = "black",
    weight = 0.8,
    options = pathOptions(pane = "farm_polys")
  ) %>%
  leafem:::addFgb(
    file = "fgb_data/erfs/erfs.fgb",
    group = "ERFs",
    label = "leaf_popup",
    popup = "leaf_popup",
    fillColor = layer_cols[4],
    fill = TRUE,
    fillOpacity = opacity_cols,
    stroke = TRUE,
    color = "black",
    weight = 0.8,
    options = pathOptions(pane = "erf_polys")
   ) %>%
  leafem:::addFgb(
    file = "fgb_data/protect_area/protect_area.fgb",
    group = "Protected areas",
    label = "leaf_popup",
    popup = "leaf_popup",
    fillColor = layer_cols[2],
    fill = TRUE,
    fillOpacity = opacity_cols,
    stroke = TRUE,
    color = "black",
    weight = 0.8,
    options = pathOptions(pane = "protect_area_polys")
  ) %>%
  leafem:::addFgb(
    file = "fgb_data/nogo/nogo.fgb",
    popup = "SENSFEAT",
    label = "No-go area",
    group = "No-go areas",
    fillColor = layer_cols[1],
    fill = TRUE,
    fillOpacity = opacity_cols,
    stroke = TRUE,
    color = "black",
    weight = 0.8,
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
    arrange(`EST Theme`, Species) %>%
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
      title = md("List of species flagged in no-go property"),
      subtitle = ""
    ) %>%
    # tab_footnote(footnote = "Environmental Screening Tool",
    #              locations = cells_column_labels(
    #                columns = vars(`EST Theme`)
    #              )
    # ) %>%
    cols_label(`EST Theme` = "Theme") %>%
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
      title = md("List of properties that intersect with species no-go polygons"),
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

sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

species_summary_table <- spp_list %>%
  group_by(THEME, CLASS) %>%
  tally() %>%
  rename(Theme = THEME, `Class or taxa` = CLASS, `Number of species` = n) %>%
  ungroup() %>%
  mutate(`Class or taxa` = ifelse(is.na(`Class or taxa`), "-", `Class or taxa`)) %>%
  gt::gt(.) %>%
  tab_header(
    title = md("Number of species in each group included in no-go map"),
    subtitle = ""
  )


set_zoom_spp <- function(x){

  if(x > 2100){
    zoom_lev <- 10
  } else if (x > 1500 & x < 2100){
    zoom_lev <- 10
  } else if (x > 1200 & x < 1500){
    zoom_lev <- 11
  } else if (x > 800 & x < 1200){
    zoom_lev <- 12
  } else if (x > 600 & x < 800){
    zoom_lev <- 13
  } else if (x > 300 & x < 600){
    zoom_lev <- 13
  } else if (x < 300){
    zoom_lev <- 14
  }

  return(zoom_lev)
}
