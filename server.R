library(shiny)

# Load data ---------------------------------------------------------------
load("data_input/spatial_data_inputs.RData")
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## Server ----
server <- function(input, output, session) {

  ## GEOMETRY PLOTTING ----

  ## Plot base map ----
  output$nogomap <- renderLeaflet({

    leaflet() %>%
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
      addLayersControl(
        baseGroups = c("Topographic",
                       "Streets",
                       "Imagery"
        ),
        overlayGroups= c("No-go areas"),
        options = layersControlOptions(collapsed=FALSE)
      ) %>%
      setView(lng = 25.4015133,
              lat = -29.1707702,
              zoom = 6) %>%
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
        smoothFactor = 1.5
      ) %>%
      addLegend("bottomright",
                colors = c("red"),
                labels = c("No-Go"),
                title = "Legend",
                opacity = 0.6
      ) %>%
      # leafem::addLogo(img = "data_input/ewt_01.png",
      #                 src = "local",
      #                 url = "https://www.ewt.org.za/",
      #                 position = "topleft",
      #                 offset.x = 50,
      #                 width = 60) %>%
      addResetMapButton() %>%
      leafem::addMouseCoordinates() %>%
      addDrawToolbar(polylineOptions = FALSE,
                     polygonOptions = drawPolygonOptions(
                       shapeOptions = drawShapeOptions(color = "orange", fillColor = "orange")
                     ),
                     rectangleOptions = drawRectangleOptions(
                       shapeOptions = drawShapeOptions(color = "orange", fillColor = "orange")
                     ),
                     markerOptions = FALSE,
                     circleMarkerOptions = FALSE,
                     circleOptions = FALSE,
                     editOptions = editToolbarOptions())

  })

  ## Add cadastral data -----
  observeEvent(input$add_cadastral, {

    leafletProxy("nogomap") %>%
      clearControls() %>%
      addLayersControl(
        baseGroups = c("Topographic",
                       "Streets",
                       "Imagery"
        ),
        overlayGroups= c("No-go areas","Protected areas","Farm portions","ERFs"),
        options = layersControlOptions(collapsed=FALSE)
      ) %>%

      clearGroup("No-go areas") %>%
      addMapPane("farm_polys", zIndex = 410) %>% # Farms will plot beneath no-go polys
      addMapPane("erf_polys", zIndex = 415) %>%
      addMapPane("nogo_polys", zIndex = 420) %>% # No-go polys will plot above farms
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
      ) %>%
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
      ) %>%
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
      ) %>%
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
      ) %>%
      addLegend("bottomright",
                colors = c("red","lime","blue","yellow"),
                labels = c("No-Go","PA","Farm portion","ERF"),
                title = "Legend",
                opacity = 0.6
      )

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
      hideGroup("Farm portions") %>%
      hideGroup("ERFs") %>%
      hideGroup("Protected areas")

    # NOT WORKING:
    # removeDrawToolbar(clearFeatures=TRUE) #%>%
    # addDrawToolbar(polylineOptions = FALSE,
    #                markerOptions = FALSE,
    #                circleMarkerOptions = FALSE,
    #                circleOptions = FALSE,
    #                editOptions = editToolbarOptions())

    ## Reset input boxes
    updateTextInput(session, "sgcode", label = "Enter 21 digit SG code", value = "")
    updateNumericInput(session, "lat", "Latitude", value = 0)
    updateNumericInput(session, "long", "Longitude", value = 0)
    shinyjs::reset("user_shape") # Doesn't remove underlying data (only text in widget)
    shp_value$poly_shp <- NULL

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


  ## Plot user point ----

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


  ## DOWNLOADS ----

  ## Show download button ----
  # Note - map name (e.g., nogomap) is the first part of "_draw_new_feature"
  observeEvent(input$nogomap_draw_new_feature, {
    shinyjs::show("downloaddiv")
  })

  ## Download user drawn shapes ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("shapefile", "zip", sep=".")
    },
    content = function(file) {
      temp_shp <- tempdir()
      geo = input$nogomap_draw_new_feature$geometry$coordinates[[1]]
      lng = map_dbl(geo, `[[`, 1)
      lat = map_dbl(geo, `[[`, 2)
      shp = st_as_sf(tibble(lon = lng, lat = lat),
                     coords = c("lon", "lat"),
                     crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")

      shp_files <- list.files(temp_shp, "shapefile*",
                              full.names = TRUE)
      if(length(shp_files) != 0) {
        file.remove(shp_files)
      }
      st_write(shp, paste(temp_shp, "shapefile.shp", sep = "\\"))
      # copy the zip file to the file argument
      shp_files <- list.files(temp_shp, "shapefile*",
                              full.names = TRUE)
      zip(zipfile = file, files = shp_files, flags = "-j")
      file.remove(shp_files)
    }
  )
  ## GEOMETRY INTERSECTIONS ----

  ## Intersect hand drawn polygon and high sensitivity layer ----

  # Create holder for reactive value
  shp_value <- reactiveValues(
    poly_shp = NULL
  )

  # Populate when shape is drawn
  observeEvent(input$nogomap_draw_new_feature, {
    shp_value$poly_shp = input$nogomap_draw_new_feature$geometry$coordinates[[1]]
  })

  sens_df_hand <- reactive({

    # Use reactive value
    req(shp_value)
    geo <- shp_value$poly_shp
    lng <- map_dbl(geo, `[[`, 1)
    lat <- map_dbl(geo, `[[`, 2)
    shp <- st_as_sf(tibble(lon = lng, lat = lat),
                    coords = c("lon", "lat"),
                    crs = 4326) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")

    nogo_user_int <- st_intersection(shp, high_sens_all)

    df <- nogo_user_int %>%
      st_drop_geometry() %>%
      group_by(SENSFEA) %>%
      tally() %>%
      rename(Species = SENSFEA, NoGo_count = n)

    df

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

  ## Intersect hand drawn polygon and farm/erf layer ----
  prop_df_hand <- reactive({

    req(shp_value)

    geo <- shp_value$poly_shp
    lng <- map_dbl(geo, `[[`, 1)
    lat <- map_dbl(geo, `[[`, 2)
    shp <- st_as_sf(tibble(lon = lng, lat = lat),
                    coords = c("lon", "lat"),
                    crs = 4326) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")

    farm_int <- st_intersection(shp,farms)
    erf_int <- st_intersection(shp,erf_all)


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

  ## DT OPTIONS ----
  # See help for DT setup: https://rstudio.github.io/DT/options.html
  #                        https://shiny.rstudio.com/articles/datatables.html

  dt_options <- list(
    deferRender = TRUE,
    scrollY = 400,
    scrollX = TRUE,
    scroller = TRUE,
    autoWidth = FALSE,
    # columnDefs = list(list(width = '5px', targets = c(1,2,3))), # Causes error with filtering
    dom = "Bfrtip", # Order in which DT components are added
    # dom = "lBtip",
    buttons = list("copy", "print", list(
      extend = "collection",
      buttons = c("csv", "excel", "pdf"),
      text = "Download")
    )
  )

  dt_button_options <- list(
    dom = "Bfrtip", # Order in which DT components are added
  # dom = "lBtip",
  buttons = list("copy", "print", list(
    extend = "collection",
    buttons = c("csv", "excel", "pdf"),
    text = "Download")
    )
  )


  ## TABLES SENSITIVTY -----

  ## Create sensitivity user polygon data table ----
  output$sens_feat_table_user <- renderDataTable(
    sens_df_user(), # NEED TO MERGE THESE TWO FUNCTIONS
    extensions = "Buttons",
    options = dt_button_options
  )

  ## Create sensitivity point data table ----
  output$sens_feat_table_point <- DT::renderDataTable(
    sens_df_point(),
    extensions = "Buttons",
    options = dt_button_options
  )

  ## Create sensitivity SG data table ----
  output$sens_feat_table_sg <- DT::renderDataTable(
    sens_df_sg(),
    extensions = "Buttons",
    options = dt_button_options
  )

  ## Create sensitivity hand data table ----
  output$sens_feat_table_hand <- DT::renderDataTable(
    sens_df_hand(),
    extensions = "Buttons",
    options = dt_button_options
  )

  ## TABLES PROPERTIES -----

  ## Create user polygon property data table ----
  output$property_table_user <- DT::renderDataTable(
    prop_df_user(),
    extensions = "Buttons",
    options = dt_options
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
  },
  extensions = "Buttons",
  options = dt_options)

  ## Create point property data table ----
  output$property_table_point <- renderDataTable(
    prop_df_point(),
    extensions = "Buttons",
    options = dt_options
  )

  ## Create hand polygon property data table ----
  output$property_table_hand <- renderDataTable(
    prop_df_hand(),
    extensions = "Buttons",
    options = dt_options
  )

}

