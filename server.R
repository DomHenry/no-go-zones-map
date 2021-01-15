## SERVER ----
server <- function(input, output, session) {

  ## GEOMETRY PLOTTING ----

  ### Compile base map ----
  output$nogomap <- renderLeaflet({
    global_base_map
  })

  ### Render base map ----
  observeEvent(input$add_cadastral, {

    shinyjs::hide("cadastraldiv")
    shinyjs::show("clearcontroldiv")

    output$nogomap_base <- renderUI({

      leafletOutput("nogomap", width = "100%", height = 620) %>%
        withSpinner(type = 1, size = 1.5)

    })

  })

  ### Reset map to original state ----
  observeEvent(input$map_reset,{

    leafletProxy("nogomap") %>%
      setView(lng = 25.4015133,
              lat = -29.1707702,
              zoom = 6) %>%
      clearGroup("User property") %>%
      clearGroup("User point") %>%
      clearGroup("cranes") %>%
      clearControls() %>%
      addLegend("bottomright",
                colors = layer_cols,
                labels = c("No-Go","PA","Farm portion","ERF"),
                title = "Legend",
                opacity = opacity_cols) %>%
      removeLayersControl() %>%
      addLayersControl(
        baseGroups = c("Topographic", "Streets","Imagery"),
        overlayGroups= overlay_grp_names,
        options = layersControlOptions(collapsed=FALSE)
      ) %>%
      hideGroup("Farm portions") %>%
      hideGroup("ERFs") %>%
      hideGroup("Protected areas")
    # # NOT WORKING: # Note 6
    # removeDrawToolbar(clearFeatures=TRUE) %>%
    # addDrawToolbar(polylineOptions = FALSE,
    #                markerOptions = FALSE,
    #                circleMarkerOptions = FALSE,
    #                circleOptions = FALSE,
    #                editOptions = editToolbarOptions())

    ## Reset input boxes
    updateSelectizeInput(session,
                         "sg_key", "Search with 21 digit SG key:",
                         choices = c("Enter SG key" = "", sgdata),
                         options=list(create=FALSE, selectOnTab = TRUE))
    updateNumericInput(session, "lat", "Latitude", value = NA)
    updateNumericInput(session, "long", "Longitude", value = NA)
    shinyjs::reset("user_shape") # Note 2
    shp_value$poly_shp <- NULL
  })


  ### Reset shapefile upload input ----
  values <- reactiveValues(
    upload_state = NULL
  )

  observeEvent(input$user_shape, {
    values$upload_state <- "uploaded"
  })

  observeEvent(input$map_reset, {
    values$upload_state <- "reset"
  })

  file_input <- reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == "uploaded") {
      return(input$user_shape)
    } else if (values$upload_state == "reset") {
      return(NULL)
    }
  })

  ### Upload and extract user polygon (either KML or SHAPEFILE) ----
  user_polygon <- reactive({

    shpdf <- file_input()

    validate(
      need(!is.null(shpdf), "Please upload valid shapefile files")
    )

    shp_needed <- c("shp","shx","dbf","prj")
    shp_ext <- tools::file_ext(shpdf$datapath)

    if(str_detect(shpdf$datapath[1], ".kml")){

      poly <- st_read(shpdf$datapath)
      poly <- poly %>%
        st_transform(crs = latlongCRS)
      st_crs(poly) <- latlongCRS
      return(poly)

    } else if (all(shp_needed %in% shp_ext)){
      print("import valid shapefile")
      tempdirname <- dirname(shpdf$datapath[1])

      for (i in 1:nrow(shpdf)) {
        file.rename(
          shpdf$datapath[i],
          paste0(tempdirname, "/", shpdf$name[i])
        )
      }
      poly <- st_read(paste(tempdirname,
                            shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                            sep = "/"))

      poly <- poly %>%
        st_transform(crs = latlongCRS)
      st_crs(poly) <- latlongCRS
      return(poly)

    } else {
      print("return NULL")
      poly <- NULL
    }

  })

  ###  Plot user input polygon ----
  observeEvent(input$plot_footprint,{

    if(!is.null(user_polygon())){

      cen <- sfc_as_cols(st_centroid(user_polygon())) %>%
      st_drop_geometry()

      poly_area <- as.numeric(st_area(user_polygon())/1000000)

      leafletProxy("nogomap") %>%
        addPolygons(
          data = user_polygon(),
          group = "User property",
          popup = "User property",
          fillColor = "#912CEE",
          fillOpacity = opacity_cols,
          stroke = TRUE,
          color = "black",
          weight = 0.8,
          smoothFactor = 2
        ) %>%
        setView(
          lng = cen$x,
          lat = cen$y,
          zoom = set_zoom(poly_area)
        )  %>%
        removeLayersControl() %>%
        addLayersControl(
          baseGroups = c("Topographic", "Streets","Imagery"),
          overlayGroups= c(overlay_grp_names, "User property"),
          options = layersControlOptions(collapsed=FALSE)
        ) %>%
        showGroup("Farm portions") %>%
        clearControls() %>%
        addLegend("bottomright",
                  colors = c(layer_cols,"#912CEE"),
                  labels = c(overlay_grp_names, "User property"),
                  title = "Legend",
                  opacity = opacity_cols
        )
        }
  })


  ### Plot user point ----

  # Farm
  "-33.7"
  "20.5"
  # ERF
  "-30.375"
  "30.6858"

  user_point <- reactive({

    validate(
      need(is.numeric(input$long) & is.numeric(input$lat),
           "Please enter a valid latitude and longitude")
    )

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

  ### Extract property from SG code ----
  prop_extract <- reactive({

    req(input$sg_key)

    "K272N0HV000000017445000000" # FARM
    "W048C039000500001399000001" # ERF

    ref_farm <- which(farms$ID %in% input$sg_key)
    ref_erf <- which(erf_all$ID %in% input$sg_key)

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

  ### Plot property from SG code ----
  observeEvent(input$search_prop,{

    req(prop_extract()) # Note 3

    cen <- sfc_as_cols(st_centroid(prop_extract())) %>%
      st_drop_geometry()

    poly_area <- prop_extract()$poly_area[[1]]

    leafletProxy("nogomap") %>%
      addPolygons(
        data = prop_extract(),
        group = "User property",
        popup = "User property",
        fillColor = "#912CEE",
        fillOpacity = opacity_cols,
        stroke = TRUE,
        color = "black",
        weight = 0.8,
        smoothFactor = 2
      ) %>%
      setView(
        lng = cen$x[1],
        lat = cen$y[1],
        zoom = set_zoom(poly_area)
      )  %>%
      removeLayersControl() %>%
      addLayersControl(
        baseGroups = c("Topographic", "Streets","Imagery"),
        overlayGroups= c(overlay_grp_names, "User property"),
        options = layersControlOptions(collapsed=FALSE)
      ) %>%
      hideGroup("Farm portions") %>%
      clearControls() %>%
      addLegend("bottomright",
                colors = c(layer_cols,"#912CEE"),
                labels = c(overlay_grp_names, "User property"),
                title = "Legend",
                opacity = opacity_cols)

  })


  ## HAND-DRAWN POLYGON DOWNLOAD ----

  ### Show download button ----
  observeEvent(input$nogomap_draw_new_feature, { # Note 4
    shinyjs::show("downloaddiv")
  })

  ### Download hand-drawn shapes ----
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
      st_write(shp, paste(temp_shp, "user_shapefile.shp", sep = "\\"))
      ## Copy the zip file to the file argument
      shp_files <- list.files(temp_shp, "shapefile*",
                              full.names = TRUE)
      zip(zipfile = file, files = shp_files, flags = "-j")
      file.remove(shp_files)
    }
  )

  ## GEOM INTERSECTIONS: EST ----

  ### User polygon and EST layer ----
  sens_df_user <- reactive({

    # req(input$user_shape)

    validate(
      need(!is.null(user_polygon()), "Please upload valid shapefile files")
    )

    nogo_user_int <- st_intersection(user_polygon(), high_sens_all)
    df <- compile_species_table(nogo_user_int)
    df <- draw_gt(df)
    return(df)

  })

  ### SG code property and EST layer ----
  sens_df_sg <- reactive({

    req(prop_extract())

    nogo_user_int <- st_intersection(prop_extract(), high_sens_all)
    df <- compile_species_table(nogo_user_int)
    df <- draw_gt(df)

  })

  ### User point and EST layer ----
  sens_df_point <- reactive({

    req(user_point())

    farm_int <- st_intersection(farms, user_point())
    erf_int <- st_intersection(erf_all, user_point())

    if (nrow(farm_int) > 0) {

      farm_int <- farms %>%
        filter(ID == farm_int$ID)

      nogo_user_int <- st_intersection(farm_int, high_sens_all)
      df <- compile_species_table(nogo_user_int)
      df <- draw_gt(df)


    } else if (nrow(erf_int) > 0){

      erf_int <- erf_all %>%
        filter(ID == erf_int$ID)

      nogo_user_int <- st_intersection(erf_int, high_sens_all)
      df <- compile_species_table(nogo_user_int)
      df <- draw_gt(df)
    }

  })

  ### Hand-drawn polygon and EST layer ----

  ## Create holder for reactive value
  shp_value <- reactiveValues(
    poly_shp = NULL
  )

  ## Populate when shape is drawn
  observeEvent(input$nogomap_draw_new_feature, {
    shp_value$poly_shp = input$nogomap_draw_new_feature$geometry$coordinates[[1]]
  })

  sens_df_hand <- reactive({

    ## Use reactive value
    req(shp_value)

    geo <- shp_value$poly_shp

    validate(
      need(!is.null(geo), "Please draw polygon on map first")
    )

    lng <- map_dbl(geo, `[[`, 1)
    lat <- map_dbl(geo, `[[`, 2)
    shp <- st_as_sf(tibble(lon = lng, lat = lat),
                    coords = c("lon", "lat"),
                    crs = 4326) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")

    nogo_user_int <- st_intersection(shp, high_sens_all)
    df <- compile_species_table(nogo_user_int)
    df <- draw_gt(df)
    return(df)

  })

  ## GEOM INTERSECTIONS: FARM/ERF ----

  ### User polygon and farm/erf layer ----
  prop_df_user <- reactive({

    req(input$user_shape)

    validate(
      need(!is.null(user_polygon()), "Please upload valid shapefile files")
    )

    farm_int <- st_intersection(user_polygon(),farms)
    erf_int <- st_intersection(user_polygon(),erf_all)

    if (nrow(farm_int) > 0) {
      df <- compile_property_table(farm_int,"Farm_field")
      df <- draw_gt_property(df)

    } else if (nrow(erf_int) > 0){
      df <- compile_property_table(erf_int,"ERF_field")
      df <- draw_gt_property(df)

    }


  })

  ### User point and farm/erf layer ----
  prop_df_point <- reactive({

    req(user_point())

    farm_int <- st_intersection(farms, user_point())
    erf_int <- st_intersection(erf_all, user_point())

    if (nrow(farm_int) > 0) {
      df <- compile_property_table(farm_int,"Farm_field")
      df <- draw_gt_property(df)
    } else if (nrow(erf_int) > 0) {
      df <- compile_property_table(erf_int,"ERF_field")
      df <- draw_gt_property(df)
    }

  })

  ### Hand-drawn polygon and farm/erf layer ----
  prop_df_hand <- reactive({

    req(shp_value)

    geo <- shp_value$poly_shp

    validate(
      need(!is.null(geo), "Please draw polygon on map first")
    )

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
      df <- compile_property_table(farm_int,"Farm_field")
      df <- draw_gt_property(df)
    } else if (nrow(erf_int) > 0){
      df <- compile_property_table(erf_int,"ERF_field")
      df <- draw_gt_property(df)
    }

  })

  ## TABLES: SPECIES DATA -----
  table_width <- px(1200)
  table_height <- px(400)

  ### EST user polygon data table ----
  output$sens_feat_table_user <- render_gt(
      expr = sens_df_user(),
      # width = table_width,
      # height = table_height,
      align = "left"
  )

  ### EST SG data table ----
  output$sens_feat_table_sg <- render_gt(
    expr = sens_df_sg(),
    align = "left"
  )

  ### EST point data table ----
  output$sens_feat_table_point <-render_gt(
    sens_df_point(),
    align = "left"
  )

  ### EST hand-drawn data table ----
  output$sens_feat_table_hand <- render_gt(
    expr = sens_df_hand(),
    align = "left"
  )

  ## TABLES: PROPERTY DATA -----

  ### User polygon property data table ----
  output$property_table_user <- render_gt(
    expr = prop_df_user(),
    align = "left"
    )

  ### SG code property data table ----
  output$property_table_sg <- render_gt(
    expr = {
      req(prop_extract())
      df <- compile_property_table(prop_extract(), "Farm_field")
      df <- draw_gt_property(df)
      },
    align = "left"
    )

  ### Point property data table ----
  output$property_table_point <- render_gt(
    prop_df_point(),
    align = "left"
  )

  ### Hand-drawn polygon property data table ----
  output$property_table_hand <- render_gt(
    expr = prop_df_hand(),
    align = "left"
  )

  # DOWNLOADS ----
  output$download_species_01 <- downloadHandler(
   filename = function() {
      paste0(Sys.Date(), "_species_data_01.csv")
    },
    content = function(file) {
      write.csv(sens_df_user(), file, row.names = FALSE)
    }
  )

  output$download_property_01 <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_property_data_01.csv")
    },
    content = function(file) {
      write.csv(prop_df_user(), file, row.names = FALSE)
    }
  )

  output$download_species_02 <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_species_data_02.csv")
    },
    content = function(file) {
      write.csv(sens_df_sg(), file, row.names = FALSE)
    }
  )

  output$download_property_02 <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_property_data_02.csv")
    },
    content = function(file) {
      df <- compile_property_table(prop_extract(), "Farm_field")
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_species_03 <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_species_data_03.csv")
    },
    content = function(file) {
      write.csv(sens_df_point(), file, row.names = FALSE)
    }
  )

  output$download_property_03 <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_property_data_03.csv")
    },
    content = function(file) {
      write.csv(prop_df_point(), file, row.names = FALSE)
    }
  )

  output$download_species_04 <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_species_data_04.csv")
    },
    content = function(file) {
      write.csv(sens_df_hand(), file, row.names = FALSE)
    }
  )

  output$download_property_04 <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_property_data_04.csv")
    },
    content = function(file) {
      write.csv(prop_df_hand(), file, row.names = FALSE)
    }
  )
}


# NOTES -------------------------------------------------------------------

## Note 1 ----
# See https://esri.github.io/esri-leaflet/api-reference/layers/basemap-layer.html

## Note 2 ----
# This function doesn't remove underlying data (only text in widget)

## Note 3 ----
# This will not plot if prop_extract() is NULL

## Note 4 ----
# Map name (e.g., nogomap) is the first part of "_draw_new_feature"

## Note 5 ----
# Create a folder called "www" in app.R directory and put images in there. Don't reference the folder as a the source, shiny will automatically look there.

## Note 6 ----

# In order to get this function working I would need to follow this and make manual changes to package for fix:
  # https://github.com/bhaskarvk/leaflet.extras/issues/148