## Libraries ----
library(sf)
library(tidyverse)
library(shinysky)
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

## Load data ----
load("data_input/spatial_data_inputs.RData")
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
sgdata <- c(unique(farms$PRCL_KEY),unique(erf_all$PRCL_KEY))

## Increase upload file size ----
maxsize_MB <- 30 # megabytes
options(shiny.maxRequestSize = maxsize_MB * 1024^2)

## DRAW BASE MAP HERE?!? -----


# Profiling ----

# https://lukesingham.com/shiny-r-performance-profiling/
# profvis::profvis({ runApp(appDir = getwd())})

# renv ----

# renv::init()
# renv::snapshot()
# renv::restore()
# Use renv::history() to view past versions of renv.lock that have been committed to your repository
# Use renv::revert() to pull out an old version of renv.lock based on the previously-discovered commit
# pkg_check <- installed.packages()


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
    group_by(SENSFEA, THEME, SENSITI) %>%
    tally() %>%
    rename(Species = SENSFEA, 'EST Theme' = THEME,
           'EST sensistivity' = SENSITI, 'Polygon count' = n) %>%
    mutate(Class = str_split(Species, pattern = "-")[[1]][1],
           Species = str_split(Species, pattern = "-")[[1]][2]) %>%
    select(Species, Class, everything()) %>%
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
    rename("Property attribute" = 1)

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


