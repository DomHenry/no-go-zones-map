## SERVER ----
server <- function(input, output, session) {

  ## GEOMETRY PLOTTING ----

  ### Compile base map ----
  output$nogomap <- renderLeaflet({
    global_base_map
  })

  ### Render base map in the background ----
  outputOptions(output, "nogomap", suspendWhenHidden = FALSE)

  ## Server-side selectize for massively improved performance
  updateSelectizeInput(session, "sg_key", "Search with 21 digit SG key:",
                       choices = c("Enter SG key" = "", sgdata),
                       options=list(create=FALSE, selectOnTab = TRUE),
                       server = TRUE)

  updateSelectizeInput(session,
                       inputId = "spp_choice", label = "Species", server = TRUE,
                       choices = list(
                         `Amphibia` = spp_list %>% filter(CLASS == "Amphibia") %>% pull(SENSFEAT),
                         `Arachnida` = spp_list %>% filter(CLASS == "Arachnida") %>% pull(SENSFEAT),
                         `Aves` = spp_list %>% filter(CLASS == "Aves") %>% pull(SENSFEAT),
                         `Insecta` = spp_list %>% filter(CLASS == "Insecta") %>% pull(SENSFEAT),
                         `Invertebrate` = spp_list %>% filter(CLASS == "Invertebrate") %>% pull(SENSFEAT),
                         `Mammalia` = spp_list %>% filter(CLASS == "Mammalia") %>% pull(SENSFEAT),
                         `Reptilia` = spp_list %>% filter(CLASS == "Reptilia") %>% pull(SENSFEAT),
                         `Plants` = spp_list %>% filter(THEME  == "Plants") %>% pull(SENSFEAT)

                       ),
                       selected = NULL,
                       # multiple = FALSE,
                       options = list(
                         placeholder = "Start typing or select from dropdown",
                         onInitialize = I('function() { this.setValue("a"); }')
                       )
  )

  ### Reset map to original state ----

  observeEvent(input$map_reset,{

    output$nogomap <- renderLeaflet({
      global_base_map
    })

    ## Reset input boxes
    updateSelectizeInput(session,
                         "sg_key", "Search with 21 digit SG key:", server = TRUE,
                         choices = c("Enter SG key" = "", sgdata),
                         options=list(create=FALSE, selectOnTab = TRUE))
    updateNumericInput(session, "lat", "Latitude", value = NA)
    updateNumericInput(session, "long", "Longitude", value = NA)
    shinyjs::reset("user_shape") # Note 2
    shp_value$poly_shp <- NULL
    shinyjs::hide("downloadData")

  updateSelectizeInput(session,
    inputId = "spp_choice", label = "Species", server = TRUE,
    choices = list(
      `Amphibia` = spp_list %>% filter(CLASS == "Amphibia") %>% pull(SENSFEAT),
      `Arachnida` = spp_list %>% filter(CLASS == "Arachnida") %>% pull(SENSFEAT),
      `Aves` = spp_list %>% filter(CLASS == "Aves") %>% pull(SENSFEAT),
      `Insecta` = spp_list %>% filter(CLASS == "Insecta") %>% pull(SENSFEAT),
      `Invertebrate` = spp_list %>% filter(CLASS == "Invertebrate") %>% pull(SENSFEAT),
      `Mammalia` = spp_list %>% filter(CLASS == "Mammalia") %>% pull(SENSFEAT),
      `Reptilia` = spp_list %>% filter(CLASS == "Reptilia") %>% pull(SENSFEAT),
      `Plants` = spp_list %>% filter(THEME  == "Plants") %>% pull(SENSFEAT)

    ),
    selected = NULL,
    # multiple = FALSE,
    options = list(
      placeholder = "Start typing or select from dropdown",
      onInitialize = I('function() { this.setValue("a"); }')
    )
  )
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

    shinyjs::show("clearcontroldiv")

    if(!is.null(user_polygon())){

      cen <- rgeos::gCentroid(as(user_polygon(), "Spatial"), byid = FALSE) %>%
        as(., "data.frame")

      poly_area <- as.numeric(st_area(st_union(user_polygon()))/1000000)

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
        hideGroup("Farm portions") %>%
        showGroup("User property") %>%
        showGroup("No-go areas") %>%
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

    shinyjs::show("clearcontroldiv")

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

    shinyjs::show("clearcontroldiv")

    ref_farm <- which(farms$ID %in% input$sg_key)
    ref_erf <- which(erfs$ID %in% input$sg_key)

    if (all(c(length(ref_farm) == 0,length(ref_erf) == 0))) {
      prop_extract <- NULL
    } else if (length(ref_farm) >= 1){
      ref <- ref_farm
      prop_extract <- farms[ref,]
    } else if (length(ref_erf) >= 1){
      ref <- ref_erf
      prop_extract <- erfs[ref,]
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
      clearControls() %>%
      addLegend("bottomright",
                colors = c(layer_cols,"#912CEE"),
                labels = c(overlay_grp_names, "User property"),
                title = "Legend",
                opacity = opacity_cols)

  })

  ### Extract species data from list choice ----
  spp_extract <- reactive({

    req(input$spp_choice)

    spp_poly <- nogo %>%
      filter(SENSFEAT %in% input$spp_choice)

    return(spp_poly)

  })

  ### Plot species distribution from list choice ----

  observeEvent(input$plot_spp,{

    shinyjs::show("clearcontroldiv")

    req(spp_extract())

    cen <- sfc_as_cols(st_centroid(spp_extract())) %>%
      st_drop_geometry()

    square <- st_make_grid(st_bbox(spp_extract()), n = 1)
    sq_area <- as.numeric(st_area(square))/1e+6

    leafletProxy("nogomap") %>%
      addPolygons(
        data = spp_extract(),
        group = "Species",
        popup = input$spp_choice,
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
        zoom = set_zoom_spp(sq_area)
      )  %>%
      removeLayersControl() %>%
      addLayersControl(
        baseGroups = c("Topographic", "Streets","Imagery"),
        overlayGroups= c(overlay_grp_names, "Species"),
        options = layersControlOptions(collapsed=FALSE)
      ) %>%
      hideGroup("Farm portions") %>%
      hideGroup("ERFs") %>%
      hideGroup("Protected areas") %>%
      hideGroup("No-go areas") %>%
      clearControls() %>%
      addLegend("bottomright",
                colors = c(layer_cols,"#912CEE"),
                labels = c(overlay_grp_names, "Species"),
                title = "Legend",
                opacity = opacity_cols)

  })

  ## HAND-DRAWN POLYGON DOWNLOAD ----

  ### Show download button ----
  observeEvent(input$nogomap_draw_new_feature, { # Note 4
    shinyjs::show("downloaddiv")
    shinyjs::show("clearcontroldiv")

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

    nogo_user_int <- st_intersection(user_polygon(), nogo)
    df <- compile_species_table(nogo_user_int)
    df <- draw_gt(df)
    return(df)

  })

  ### SG code property and EST layer ----
  sens_df_sg <- reactive({

    req(prop_extract())

    validate(
      need(!is.null(input$sg_key), "Please select/enter valid SG code")
    )

    nogo_user_int <- st_intersection(prop_extract(), nogo)
    df <- compile_species_table(nogo_user_int)
    df <- draw_gt(df)

  })

  ### User point and EST layer ----
  sens_df_point <- reactive({

    req(user_point())

    farm_int <- st_intersection(farms, user_point())
    erf_int <- st_intersection(erfs, user_point())

    if (nrow(farm_int) > 0) {

      farm_int <- farms %>%
        filter(ID == farm_int$ID)

      nogo_user_int <- st_intersection(farm_int, nogo)
      df <- compile_species_table(nogo_user_int)
      df <- draw_gt(df)


    } else if (nrow(erf_int) > 0){

      erf_int <- erfs %>%
        filter(ID == erf_int$ID)

      nogo_user_int <- st_intersection(erf_int, nogo)
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

    observeEvent(input$nogomap_draw_new_feature, { # Note 4
      shinyjs::show("downloaddiv")
    })


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

    nogo_user_int <- st_intersection(shp, nogo)
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
    erf_int <- st_intersection(user_polygon(),erfs)

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
    erf_int <- st_intersection(erfs, user_point())

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
    erf_int <- st_intersection(shp,erfs)


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
  # TABLE: Species summary ----

  output$spp_summary_table <- render_gt(
    expr = species_summary_table,
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

  output$download_species_list <- downloadHandler(
    filename = function() {
     "no-go species list.csv"
    },
    content = function(file) {
      file.copy("data_input/nogo_species_list.csv", file)
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

# In order to get this function working I would need to follow this and make manual changes to package for fix: https://github.com/bhaskarvk/leaflet.extras/issues/148