library(shiny)

# Load data ---------------------------------------------------------------
load("data_input/spatial_data_inputs.RData")
source("src/helper_functions.R")
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## Server ----
server <- function(input, output, session) {

  ## GEOMETRY PLOTTING ----

  ### Map settings ----

  # Names: ("firebrick2", "darkolivegreen3", "dodgerblue", "darkgoldenrod1")
  layer_cols <- c("#EE2C2C", "#A2CD5A", "#1E90FF", "#FFB90F")
  opacity_cols <- 0.5
  overlay_grp_names <-  c("No-go areas","Protected areas","Farm portions","ERFs")

  ### Plot base map ----
  output$nogomap <- renderLeaflet({

    leaflet() %>%
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
        overlayGroups= c("No-go areas"),
        options = layersControlOptions(collapsed=FALSE)
      ) %>%
      setView(lng = 25.4015133,
              lat = -29.1707702,
              zoom = 6) %>%
      addPolygons(
        data = high_sens_all,
        popup = ~ scntfc_,
        label = "No-go area",
        group = "No-go areas",
        fillColor = layer_cols[[1]],
        fillOpacity = opacity_cols,
        stroke = TRUE,
        color = "black",
        weight = 0.8,
        smoothFactor = 1.5
      ) %>%
      addLegend("bottomright",
                colors = layer_cols[[1]],
                labels = c("No-Go"),
                title = "Legend",
                opacity = opacity_cols
      ) %>%
      # leafem::addLogo(img = "data_input/ewt_01.png",
      #                 src = "local",
      #                 url = "https://www.ewt.org.za/",
      #                 position = "topleft",
      #                 offset.x = 50,
      #                 width = 60) %>%
      addResetMapButton() %>%
      hideGroup("Protected areas") %>%
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

  })

  ### Add cadastral data -----
  observeEvent(input$add_cadastral, {

    shinyjs::hide("cadastraldiv")
    shinyjs::show("clearcontroldiv")

    leafletProxy("nogomap") %>%
      clearControls() %>%
      addLayersControl(
        baseGroups = c("Topographic",
                       "Streets",
                       "Imagery"
        ),
        overlayGroups= overlay_grp_names,
        options = layersControlOptions(collapsed=FALSE)
      ) %>%

      clearGroup("No-go areas") %>%
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
        data = erf_all,
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
        data = pa,
        group = "Protected areas",
        popup = ~ cur_nme,
        fillColor = layer_cols[2],
        fillOpacity = opacity_cols,
        stroke = TRUE,
        color = "black",
        weight = 0.8,
        smoothFactor = 2
      ) %>%
      addPolygons(
        data = high_sens_all,
        popup = ~ scntfc_,
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
      )

  })

  ### Reset map to original state ----
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

    # NOT WORKING:
    # removeDrawToolbar(clearFeatures=TRUE) #%>%
    # addDrawToolbar(polylineOptions = FALSE,
    #                markerOptions = FALSE,
    #                circleMarkerOptions = FALSE,
    #                circleOptions = FALSE,
    #                editOptions = editToolbarOptions())

    ## Reset input boxes
    updateTextInput(session, "sgcode", label = "Enter 21 digit SG code", value = "")
    updateNumericInput(session, "lat", "Latitude", value = NA)
    updateNumericInput(session, "long", "Longitude", value = NA)
    shinyjs::reset("user_shape") # Note 2
    shp_value$poly_shp <- NULL
  })


  ### Upload and extract user polygon (either KML or SHAPEFILE) ----
  user_polygon <- reactive({

    req(input$user_shape)
    shpdf <- input$user_shape

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

      leafletProxy("nogomap") %>%
        addPolygons(
          data = user_polygon(),
          group = "User shapefile",
          popup = "User shapefile",
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
          zoom = 11
        )  %>%
        removeLayersControl() %>%
        addLayersControl(
          baseGroups = c("Topographic", "Streets","Imagery"),
          overlayGroups= c(overlay_grp_names, "User shapefile"),
          options = layersControlOptions(collapsed=FALSE)
        ) %>%
        showGroup("Farm portions") %>%
        clearControls() %>%
        addLegend("bottomright",
                  colors = c(layer_cols,"#912CEE"),
                  labels = c(overlay_grp_names, "User shapefile"),
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

    req(input$sgcode)

    "K272N0HV000000017445000000" # FARM
    "W048C039000500001399000001" # ERF

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

  ### Plot property from SG code ----
  observeEvent(input$search_prop,{

    req(prop_extract()) # Note 3

    cen <- sfc_as_cols(st_centroid(prop_extract())) %>%
      st_drop_geometry()

    leafletProxy("nogomap") %>%
      addPolygons(
        data = prop_extract(),
        group = "User shapefile",
        popup = "User shapefile",
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
        zoom = 10
      )  %>%
      removeLayersControl() %>%
      addLayersControl(
        baseGroups = c("Topographic", "Streets","Imagery"),
        overlayGroups= c(overlay_grp_names, "User shapefile"),
        options = layersControlOptions(collapsed=FALSE)
      ) %>%
      hideGroup("Farm portions") %>%
      clearControls() %>%
      addLegend("bottomright",
                colors = c(layer_cols,"#912CEE"),
                labels = c(overlay_grp_names, "User shapefile"),
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

    req(input$user_shape)

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
        filter(PRCL_KEY == farm_int$PRCL_KEY)

      nogo_user_int <- st_intersection(farm_int, high_sens_all)
      df <- compile_species_table(nogo_user_int)
      df <- draw_gt(df)


    } else if (nrow(erf_int) > 0){

      erf_int <- erf_all %>%
        filter(PRCL_KEY == erf_int$PRCL_KEY)

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
      df <- draw_gt_property(df, Farm_field)

    } else if (nrow(erf_int) > 0){
      df <- compile_property_table(erf_int,"ERF_field")
      df <- draw_gt_property(df, ERF_field)

    }


  })

  ### User point and farm/erf layer ----
  prop_df_point <- reactive({

    req(user_point())

    farm_int <- st_intersection(farms, user_point())
    erf_int <- st_intersection(erf_all, user_point())

    if (nrow(farm_int) > 0) {
      df <- compile_property_table(farm_int,"Farm_field")
      df <- draw_gt_property(df, Farm_field)
    } else if (nrow(erf_int) > 0) {
      df <- compile_property_table(erf_int,"ERF_field")
      df <- draw_gt_property(df, ERF_field)
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
      df <- draw_gt_property(df, Farm_field)
    } else if (nrow(erf_int) > 0){
      df <- compile_property_table(erf_int,"ERF_field")
      df <- draw_gt_property(df, ERF_field)
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

  ### EST point data table ----
  output$sens_feat_table_point <-render_gt(
    sens_df_point(),
    align = "left"
  )

  ### EST SG data table ----
  output$sens_feat_table_sg <- render_gt(
    expr = sens_df_sg(),
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
      require(prop_extract())
      df <- compile_property_table(prop_extract(), "Farm_field")
      df <- draw_gt_property(df, prop_type = Farm_field)
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

