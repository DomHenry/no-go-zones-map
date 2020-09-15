library(sf)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(shinythemes)

## Increase upload file size
maxsize_MB <- 30 # megabytes
options(shiny.maxRequestSize = maxsize_MB*1024^2)

# See https://shiny.rstudio.com/gallery/superzip-example.html
# Read in shapefile https://gist.github.com/RCura/9587685

# Good guide
# https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html

## Reactivity
# https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent


source("src/01_draw leaflet.R")

## UI ----

header <- dashboardHeader(
  title = "no go map"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Interactive map", tabName = "int_map"),
    menuItem("Data output", tabName = "data_output"),
    menuItem("Help", tabName = "help")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "int_map",
            fluidRow(
                  column(
                    width = 3,
                    box(title = "inputs", width = NULL, solidHeader = TRUE,
                        status = "primary",
                        fileInput("user_shape", "Upload development footprint (kml or shp)",
                                  multiple = TRUE,
                                  accept = c(".csv",".kml",".zip",
                                             ".shx", ".shp", ".sbn", ".sbx",
                                             ".dbf",".prj")),
                        tags$hr(),
                        textInput("sgcode", "Enter 21 digit SG code", ""),
                        actionButton("search_prop", "Search property"),
                        br(),
                        tags$hr(),
                        tags$b("Enter latitude/longitude to add point"),
                        numericInput("lat", "Latitude", value = 0),
                        numericInput("long", "Longitude", value = 0),
                        actionButton("add_point", "Add point")
                        )
                  ),
                  column(
                    width = 9,
                    box(title = NULL, width = NULL, solidHeader = TRUE,
                        leafletOutput("nogomap", width = "100%", height = 620),
                        absolutePanel(id = "clear_control", class = "panel panel-default",
                                      fixed = TRUE, draggable = FALSE,
                                      top = 350, right = 30, left = "auto", bottom = "auto",
                                      width = "auto", height = "auto",
                                      actionButton("map_reset", "Clear map")
                                      )
                        )
                      )
                  )
          ),

    tabItem(tabName = "data_output",
            h2("Some outputs"),
            tableOutput("sens_feat_table"),
            tags$hr(),
            tableOutput("property_table")
            ),

    tabItem(tabName = "help",
            h2("Help to come")
    )
  )
)


ui <- dashboardPage(
  header,
  sidebar,
  body
)

## Server ----
server <- function(input, output, session) {

  ## Add user point ----
  user_point <- reactive({
    st_sfc(st_point(c(input$long,input$lat)),
           crs = latlongCRS)
  })

  observeEvent(input$add_point,{
    leafletProxy("nogomap") %>%
      addMarkers(
        data = user_point(),
        popup = "User point",
        group = "User point"
      ) %>%
      setView(lng = user_point()[[1]][1],
              lat = user_point()[[1]][2],
              zoom = 8)

  })

  ## Plot base map ----
  output$nogomap <- renderLeaflet({
    nogo_basemap
  })

  ## Reset map to original state ----
  observeEvent(input$map_reset,{

    leafletProxy("nogomap") %>%
      setView(lng = 25.4015133,
              lat = -29.1707702,
              zoom = 6) %>%
      clearGroup("User shapefile") %>%
      clearGroup("User point") %>%
      clearControls() %>%
      addLegend("bottomright",
                colors = c("red","lime","blue","yellow"),
                labels = c("No-Go","PA","Farm portion","ERF"),
                title = "Legend",
                opacity = 0.6) %>%
      removeLayersControl() %>%
      addLayersControl(
        baseGroups = c("Topographic", "Gray", "Streets","Imagery"),
        overlayGroups= c("No-go areas","Protected areas","Farm portions","ERFs"),
        options = layersControlOptions(collapsed=FALSE)
      ) %>%
      hideGroup("Farm portions")

    })

  ##  Plot user input polygon ----
  observeEvent(input$user_shape,{

    cen <- sfc_as_cols(st_centroid(user_polygon())) %>%
      st_drop_geometry()

    leafletProxy("nogomap") %>%
      addPolygons(
        data = user_polygon(),
        group = "User shapefile",
        popup = "User shapefile",
        fillColor = "purple",
        fillOpacity = 0.4,
        stroke = TRUE,
        color = "black",
        weight = 0.8,
        smoothFactor = 2
      ) %>%
      setView(
        lng = cen$x,
        lat = cen$y,
        zoom = 10
      )  %>%
      removeLayersControl() %>%
      addLayersControl(
        baseGroups = c("Topographic", "Gray", "Streets","Imagery"),
        overlayGroups= c("No-go areas","Protected areas","Farm portions","ERFs", "User shapefile"),
        options = layersControlOptions(collapsed=FALSE)
      ) %>%
      showGroup("Farm portions") %>%
      clearControls() %>%
      addLegend("bottomright",
                colors = c("red","lime","blue","yellow","purple"),
                labels = c("No-Go","PA","Farm portion","ERF","User shapefile"),
                title = "Legend",
                opacity = 0.6
      )

  })

  ## Extract property from SG code ----
  prop_extract <- reactive({

    req(input$sgcode)
    ref_farm <- which(farms$PRCL_KEY %in% input$sgcode)
    ref <- ref_farm
    prop_extract <- farms[ref,]
    prop_extract

  })

  ## Plot property from SG code ----
  observeEvent(input$search_prop,{

    cen <- sfc_as_cols(st_centroid(prop_extract())) %>%
      st_drop_geometry()

    leafletProxy("nogomap") %>%
      addPolygons(
        data = prop_extract(),
        group = "User shapefile",
        popup = "User shapefile",
        fillColor = "purple",
        fillOpacity = 0.4,
        stroke = TRUE,
        color = "black",
        weight = 0.8,
        smoothFactor = 2
      ) %>%
      setView(
        lng = cen$x,
        lat = cen$y,
        zoom = 10
      )  %>%
      removeLayersControl() %>%
      addLayersControl(
        baseGroups = c("Topographic", "Gray", "Streets","Imagery"),
        overlayGroups= c("No-go areas","Protected areas","Farm portions","ERFs", "User shapefile"),
        options = layersControlOptions(collapsed=FALSE)
      ) %>%
      hideGroup("Farm portions") %>%
      clearControls() %>%
      addLegend("bottomright",
                colors = c("red","lime","blue","yellow","purple"),
                labels = c("No-Go","PA","Farm portion","ERF","User shapefile"),
                title = "Legend",
                opacity = 0.6
      )

  })

  ## Upload and extract polygon ----
  user_polygon <- reactive({

    req(input$user_shape)
    shpdf <- input$user_shape
    tempdirname <- dirname(shpdf$datapath[1])

    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    poly <- st_read(paste(tempdirname,
                          shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                          sep = "/"
    ))
    poly
  })

  ## Create sensitivity data table ----
  output$sens_feat_table <- renderTable(
    sens_df(),
    Caption = "Test output"
  )

  ## Intersect polygon and high sensitivity layer ----
  sens_df <- reactive({

    req(input$user_shape)

    nogo_user_int <- st_intersection(user_polygon(), high_sens_all)

    df <- nogo_user_int %>%
      st_drop_geometry() %>%
      group_by(SENSFEA) %>%
      tally() %>%
      rename(Species = SENSFEA, NoGo_count = n)

    df

  })

  ## Create property data table ----
  output$property_table <- renderTable(

    prop_df(),
    Caption = "Test property output"

  )

  ## Intersect polygon and farm layer ----

  prop_df <- reactive({

    req(input$user_shape)
    farm_int <- st_intersection(user_polygon(),farms)

    farm_df <- farm_int %>%
      st_drop_geometry() %>%
      as_tibble() %>%
      select(-GID) %>%
      select(PRCL_KEY, PRCL_TYPE, ID, PROVINCE, MAJ_REGION, MAJ_CODE, PARCEL_NO, PORTION) %>%
      mutate_all(as.character)

    farm_df %>%
      mutate(prop = str_c("property_", 1:nrow(.))) %>%
      pivot_longer(cols =-prop,
                   names_to = "Farm_field",
                   values_to = "value") %>%
      pivot_wider(names_from = prop,
                  values_from = value)

  })

}

# Run the application
shinyApp(ui = ui, server = server)

## Footnotes ----

