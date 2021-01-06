## Libraries ----
library(sf)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(rgdal)
library(shinythemes)
library(DT)
library(leaflet.extras)
library(leaflet.esri)
library(leafem)

# Profiling ---------------------------------------------------------------

# https://lukesingham.com/shiny-r-performance-profiling/
# profvis::profvis({ runApp(appDir = getwd())})

# renv --------------------------------------------------------------------

# renv::init()
# renv::snapshot()
# renv::restore()
# Use renv::history() to view past versions of renv.lock that have been committed to your repository
# Use renv::revert() to pull out an old version of renv.lock based on the previously-discovered commit
# pkg_check <- installed.packages()

# Increase upload file size -----------------------------------------------
maxsize_MB <- 30 # megabytes
options(shiny.maxRequestSize = maxsize_MB*1024^2)

# Source ------------------------------------------------------------------

## UI ----
header <- dashboardHeader(
  title = "No-Go Zones Map"
)

## Sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome", tabName = "welcome"),
    menuItem("Interactive map", tabName = "int_map"),
    menuItem("Data output", tabName = "data_output"),
    menuItem("Help", tabName = "help")
  )
)

blue_button <- "color: #fff; background-color: #337ab7; border-color: #2e6da4"

# More buttons options
# https://getbootstrap.com/docs/4.0/components/buttons/

## Body ----
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "welcome",
      h1("Welcome to the No-Go Map"),
      h1("Load time is approximately 10 seconds - please be patient"),
      h1("Naviagte to 'Interactive Map' tab to get started"),
      h1("In order to load shapefiles - select all files simultaneously"),
      h1("Try out functionality with the example SG code, or Lat long point below")
    ),
    tabItem(
      tabName = "int_map",
      fluidRow(
        column(
          width = 3,
          box(
            title = "Inputs", width = NULL, solidHeader = TRUE,
            status = "primary",
            useShinyjs(), # Add this to allow shinyjs functions to work in server
            fileInput("user_shape", "Upload shapefile",
                      multiple = TRUE,
                      accept = c(
                        # ".csv",
                        ".kml",
                        # ".zip",
                        ".shx", ".shp", ".sbn", ".sbx",
                        ".dbf", ".prj"
                      )
            ),
            actionButton("plot_footprint", "Plot shapefile", style = blue_button),
            tags$hr(),
            textInput("sgcode", "Enter 21 digit SG code", "K272N0HV000000017445000000"),
            actionButton("search_prop", "Search property", style = blue_button),
            br(),
            tags$hr(),
            tags$b("Enter latitude and longitude"),
            numericInput("lat", "Latitude", value = -30.375),
            numericInput("long", "Longitude", value = 30.6858),
            actionButton("add_point", "Add point",
                         style = blue_button)
          )
        ),
        column(
          width = 9,
          box(
            title = NULL, width = NULL, solidHeader = TRUE,
            leafletOutput("nogomap", width = "100%", height = 620),
            absolutePanel(
              id = "clear_control", class = "panel panel-default",
              fixed = TRUE, draggable = FALSE,
              top = 350, right = 30, left = "auto", bottom = "auto",
              width = "auto", height = "auto",
              actionButton("map_reset", "Clear map inputs")
            ),
            absolutePanel(
              id = "add_data", class = "panel panel-default",
              fixed = TRUE, draggable = FALSE,
              top = 400, right = 30, left = "auto", bottom = "auto",
              width = "auto", height = "auto",
              actionButton("add_cadastral", "Add cadastral data")
            ),
            shinyjs::hidden(div(id = "downloaddiv",
                                absolutePanel(
                                  id = "download_shapefile", class = "panel panel-default",
                                  fixed = TRUE, draggable = FALSE,
                                  top = 350, right = 790, left = "auto", bottom = "auto",
                                  width = "auto", height = "auto",
                                  downloadButton("downloadData", "Download shape")
                                )
            )
            )
          )
        )
      )
    ),

    tabItem(
      tabName = "data_output",
      fluidRow(
        box(
          title = "Sensitive species - user polygon",
          width = 4, solidHeader = TRUE, status = "primary",
          dataTableOutput("sens_feat_table_user")
        ),
        box(
          title = "Property details - user polygon",
          width = 8, solidHeader = TRUE, status = "primary",
          dataTableOutput("property_table_user")
        )
      ),
      fluidRow(
        box(
          title = "Sensitive species - SG code",
          width = 4, solidHeader = TRUE, status = "success",
          dataTableOutput("sens_feat_table_sg")
        ),
        box(
          title = "Property details - SG code",
          width = 8, solidHeader = TRUE, status = "success",
          dataTableOutput("property_table_sg")
        )
      ),
      fluidRow(
        box(
          title = "Sensitive species - user point",
          width = 4, solidHeader = TRUE, status = "warning",
          dataTableOutput("sens_feat_table_point")
        ),
        box(
          title = "Property details - user point",
          width = 8, solidHeader = TRUE, status = "warning",
          dataTableOutput("property_table_point")
        )
      ),
      fluidRow(
        box(
          title = "Sensitive species - user hand drawn",
          width = 4, solidHeader = TRUE, status = "danger",
          dataTableOutput("sens_feat_table_hand")
        ),
        box(
          title = "Property details - user hand drawn",
          width = 8, solidHeader = TRUE, status = "danger",
          dataTableOutput("property_table_hand")
        )
      )
    ),

    tabItem(
      tabName = "help",
      h2("Help to come"),
      h3("EST: https://screening.environment.gov.za/screeningtool/#/pages/welcome"),
      h3("Support: science@ewt.org.za")
    )
  )
)

## Dashboard page ----
ui <- dashboardPage(
  header,
  sidebar,
  body
)


# Notes -------------------------------------------------------------------

# GitHub reset
# https://happygitwithr.com/reset.html
# https://jennybc.github.io/2014-05-12-ubc/ubc-r/session03_git.html

# Guide to walking back changes in GitHub/Rstudio
# https://ohi-science.org/news/github-going-back-in-time

# See https://shiny.rstudio.com/gallery/superzip-example.html
# Read in shapefile https://gist.github.com/RCura/9587685

# Good guide
# https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html

## Reactivity
# https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent