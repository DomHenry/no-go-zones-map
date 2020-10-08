library(sf)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(rgdal)
library(shinythemes)
library(DT)

# renv::init()
# renv::snapshot()
# renv::restore()

source("src/01_draw_leaflet.R")

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

## Body ----
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "welcome",
      h1("Welcome to the No-Go Map"),
      h1("Load time is approximately 10 seconds - please be patient"),
      h1("Naviagte to 'Interactive Map' tab to get started"),
      h1("In order to load shapefiles - select all files simultaneously")
    ),
    tabItem(
      tabName = "int_map",
      fluidRow(
        column(
          width = 3,
          box(
            title = "User inputs", width = NULL, solidHeader = TRUE,
            status = "primary",
            useShinyjs(), # Add this to allow shinyjs functions to work in server
            fileInput("user_shape", "Upload development footprint (kml or shp)",
              multiple = TRUE,
              accept = c(
                ".csv", ".kml", ".zip",
                ".shx", ".shp", ".sbn", ".sbx",
                ".dbf", ".prj"
              )
            ),
            actionButton("plot_footprint", "Plot footprint"),
            tags$hr(),
            textInput("sgcode", "Enter 21 digit SG code", "K436N0FS000000016416000001"),
            actionButton("search_prop", "Search property"),
            br(),
            tags$hr(),
            tags$b("Enter latitude/longitude to add point"),
            numericInput("lat", "Latitude", value = -30.375),
            numericInput("long", "Longitude", value = 30.6858),
            actionButton("add_point", "Add point")
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

## Server ----
server <- function(input, output, session) {

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
      clearGroup("cranes") %>%
      clearControls() %>%
      addLegend("bottomright",
                colors = c("red","lime","blue","yellow"),
                labels = c("No-Go","PA","Farm portion","ERF"),
                title = "Legend",
                opacity = 0.6) %>%
      removeLayersControl() %>%
      addLayersControl(
        baseGroups = c("Topographic", "Streets","Imagery"),
        overlayGroups= c("No-go areas","Protected areas","Farm portions","ERFs"),
        options = layersControlOptions(collapsed=FALSE)
      ) %>%
      hideGroup("Farm portions")

    ## Reset input boxes
    updateTextInput(session, "sgcode", label = "Enter 21 digit SG code", value = "")
    updateNumericInput(session, "lat", "Latitude", value = 0)
    updateNumericInput(session, "long", "Longitude", value = 0)
    shinyjs::reset("user_shape") # Doesn't remove underlying data (only text in widget)

  })

  ## Upload and extract user polygon ----
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

    poly <- poly %>%
      st_transform(crs = latlongCRS)

    st_crs(poly) <- latlongCRS
    poly
  })

  ##  Plot user input polygon ----
  observeEvent(input$plot_footprint,{

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
        zoom = 11
      )  %>%
      removeLayersControl() %>%
      addLayersControl(
        baseGroups = c("Topographic", "Streets","Imagery"),
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


  ## Add user point ----

  # Farm
  "-33.7"
  "20.5"
  # ERF
  "-30.375"
  "30.6858"

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
              zoom = 13)

  })

  ## Extract property from SG code ----
  prop_extract <- reactive({

    req(input$sgcode)

    "K436N0FS000000016416000001" # FARM
    "K282N0GV042100011439000001" #ERF

    ref_farm <- which(farms$PRCL_KEY %in% input$sgcode)
    ref_erf <- which(erf_all$PRCL_KEY %in% input$sgcode)

    if (all(c(length(ref_farm) == 0,length(ref_erf) == 0))) {
      prop_extract <- NULL
    } else if (length(ref_farm) >= 1){
      ref <- ref_farm
      prop_extract <- farms[ref,]
    } else if (length(ref_erf) >= 1){
      ref <- ref_erf
      prop_extract <- erf_all[ref,]
    }

  return(prop_extract)

  })

  ## Plot property from SG code ----
  observeEvent(input$search_prop,{

    req(prop_extract()) # Won't plot if prop_extract() is NULL

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
        baseGroups = c("Topographic", "Streets","Imagery"),
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

  ## Intersect user polygon and high sensitivity layer ----
  sens_df_user <- reactive({

    req(input$user_shape)

    nogo_user_int <- st_intersection(user_polygon(), high_sens_all)

    df <- nogo_user_int %>%
      st_drop_geometry() %>%
      group_by(SENSFEA) %>%
      tally() %>%
      rename(Species = SENSFEA, NoGo_count = n)

    df

  })

  ## Intersect SG code property and high sensitivity layer ----
  sens_df_sg <- reactive({

    req(prop_extract())

    nogo_user_int <- st_intersection(prop_extract(), high_sens_all)

    df <- nogo_user_int %>%
      st_drop_geometry() %>%
      group_by(SENSFEA) %>%
      tally() %>%
      rename(Species = SENSFEA, NoGo_count = n)

    df

  })

  # Intersect user point and high sensitivity layer ----

  sens_df_point <- reactive({

    req(user_point())

    farm_int <- st_intersection(farms, user_point())
    erf_int <- st_intersection(erf_all, user_point())

    if (nrow(farm_int) > 0) {

      farm_int <- farms %>%
        filter(PRCL_KEY == farm_int$PRCL_KEY)

      nogo_user_int <- st_intersection(farm_int, high_sens_all)

      df <- nogo_user_int %>%
        st_drop_geometry() %>%
        group_by(SENSFEA) %>%
        tally() %>%
        rename(Species = SENSFEA, NoGo_count = n)
      df


    } else if (nrow(erf_int) > 0){

      erf_int <- erf_all %>%
        filter(PRCL_KEY == erf_int$PRCL_KEY)

      nogo_user_int <- st_intersection(erf_int, high_sens_all)

      df <- nogo_user_int %>%
        st_drop_geometry() %>%
        group_by(SENSFEA) %>%
        tally() %>%
        rename(Species = SENSFEA, NoGo_count = n)
      df
      }

    })


  ## Intersect polygon and farm/erf layer ----

  prop_df_user <- reactive({

    req(input$user_shape)
    farm_int <- st_intersection(user_polygon(),farms)
    erf_int <- st_intersection(user_polygon(),erf_all)

    if (nrow(farm_int) > 0) {
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
    } else if (nrow(erf_int) > 0){

      erf_df <- erf_int %>%
        st_drop_geometry() %>%
        as_tibble() %>%
        select(-GID) %>%
        select(PRCL_KEY, PRCL_TYPE, ID, PROVINCE, MAJ_REGION, MAJ_CODE, PARCEL_NO, PORTION) %>%
        mutate_all(as.character) %>%
        mutate(prop = str_c("property_", 1:nrow(.)))

      erf_df %>%
        pivot_longer(cols =-prop,
                     names_to = "ERF_field",
                     values_to = "value") %>%
        pivot_wider(names_from = prop,
                    values_from = value)

    }



  })

  ## Intersect point and farm/erf layer ----

  prop_df_point <- reactive({

    req(user_point())

    farm_int <- st_intersection(farms, user_point())
    erf_int <- st_intersection(erf_all, user_point())

    if (nrow(farm_int) > 0) {

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

    } else if (nrow(erf_int) > 0) {

      erf_df <- erf_int %>%
        st_drop_geometry() %>%
        as_tibble() %>%
        select(-GID) %>%
        select(PRCL_KEY, PRCL_TYPE, ID, PROVINCE, MAJ_REGION, MAJ_CODE, PARCEL_NO, PORTION) %>%
        mutate_all(as.character) %>%
        mutate(prop = str_c("property_", 1:nrow(.)))

      erf_df %>%
        pivot_longer(cols =-prop,
                     names_to = "ERF_field",
                     values_to = "value") %>%
        pivot_wider(names_from = prop,
                    values_from = value)

    }

  })

  ## Create sensitivity user polygon data table ----
  output$sens_feat_table_user <- renderDataTable(
    sens_df_user() # NEED TO MERGE THESE TWO FUNCTIONS
  )

  ## Create sensitivity point data table ----
  output$sens_feat_table_point <- DT::renderDataTable(
    sens_df_point()
  )

  ## Create sensitivity SG data table ----
  output$sens_feat_table_sg <- DT::renderDataTable(
    sens_df_sg()
  )

  ## Create user polygon property data table ----

  # See help for DT setup: https://rstudio.github.io/DT/options.html
  #                        https://shiny.rstudio.com/articles/datatables.html
  output$property_table_user <- DT::renderDataTable(
    prop_df_user(),
    options = list(
      deferRender = TRUE,
      scrollY = 400,
      scrollX = TRUE,
      scroller = TRUE,
      autoWidth = FALSE,
      columnDefs = list(list(width = '5px', targets = c(1,2,3)))
    )
  )

  ## Create SG code property data table ----
  output$property_table_sg <- renderDataTable({

    req(prop_extract())

    farm_df <- prop_extract() %>%
      st_drop_geometry() %>%
      as_tibble() %>%
      select(-GID) %>%
      select(PRCL_KEY, PRCL_TYPE, ID, PROVINCE, MAJ_REGION, MAJ_CODE, PARCEL_NO, PORTION) %>%
      mutate_all(as.character)

    farm_df <- farm_df %>%
      mutate(prop = str_c("property_", 1:nrow(.))) %>%
      pivot_longer(cols =-prop,
                   names_to = "Farm_field",
                   values_to = "value") %>%
      pivot_wider(names_from = prop,
                  values_from = value)
    farm_df
  })

  ## Create point property data table ----
  output$property_table_point <- renderDataTable(
  prop_df_point(),
  extensions = "Buttons",
  options = list(
    dom = "Bfrtip",
    buttons =
      list("copy", "print", list(
        extend = "collection",
        buttons = c("csv", "excel", "pdf"),
        text = "Download"
      ))
  )
)


}

# Run the application
shinyApp(ui = ui, server = server)

## Footnotes ----

# See https://shiny.rstudio.com/gallery/superzip-example.html
# Read in shapefile https://gist.github.com/RCura/9587685

# Good guide
# https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html

## Reactivity
# https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent