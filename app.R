library(sf)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)

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
ui <- navbarPage(title = "No-go Zones",

             tabPanel(title = "Interactive map",
                      leafletOutput("nogomap", width="100%", height=650),
                      absolutePanel(id = "controls", class = "panel panel-default",
                                    fixed = TRUE,draggable = TRUE,
                                    top = 200, right = "auto", left = 20, bottom = "auto",
                                    width = 330, height = "auto",

                                    h2("User inputs"),
                                    tags$hr(),
                                    "Shapefile uploads (add a clear feature button)",
                                    fileInput("user_shape", "Choose file (csv, kml or shp)",
                                              multiple = TRUE,
                                              accept = c(".csv",".kml",".zip",
                                                         ".shx", ".shp", ".sbn", ".sbx",
                                                         ".dbf",".prj")),
                                    tags$hr(),
                                    "Property searches (add submit or action button)",
                                    textInput("sgcode", "SG CODE", ""),
                                    br(),
                                    tags$hr(),
                                    actionButton("search_prop", "Search property"),
                                    radioButtons("file_type", "Point or polygon",
                                                 choices = c(Point = "point",
                                                             Polygon = "polygon"),
                                                 selected = "point")


                                  )
                      ),

             tabPanel(title = "Outputs",
                      "testingtesting",
                      tableOutput("sens_feat_table")
                      ),

             tabPanel(title = "Help/instructions",
                      "Useful text"
             )

             )

server <- function(input, output) {

  ## Plot base map ----
  output$nogomap <- renderLeaflet({
    nogo_basemap
  })

  ## Reset map to original state ----

  eventReactive(input$map_reset, {

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

}

# Run the application
shinyApp(ui = ui, server = server)
