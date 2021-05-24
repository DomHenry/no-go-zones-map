## UI ----
header <- dashboardHeader(
  title = "No-Go Zones Map"
)

## Sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome", tabName = "welcome", icon = icon("info-circle"),
             selected = TRUE),
    menuItem("Interactive map", tabName = "int_map", icon = icon("map-marked-alt"),
             selected = FALSE),
    menuItem("Data output: Shapefile or KML", tabName = "data_output_01", icon = icon("table")),
    menuItem("Data output: SG key", tabName = "data_output_02",icon = icon("table")),
    menuItem("Data output: Lat/Long point", tabName = "data_output_03",icon = icon("table")),
    menuItem("Data output: Hand-drawn polygon", tabName = "data_output_04",icon = icon("table")),
    menuItem("Help", tabName = "help", icon = icon("question-circle"))
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
      fluidRow(
        column(
          width = 9,
          tabBox(
            id='tabvals',
            width=NULL,
            tabPanel(title = "What is a no-go area?",
                     includeMarkdown("data_input/Nogo_background_text.Rmd"),
                     img(src = "ewt_01.png", height = "100px"),
                     value=1),
            tabPanel(title ="How to use the interactive map",
                     "Add text",
                     value=2),
            tabPanel(title ="More resources",
                     "Add more text",
                     value=3)
          )
        )
      )
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
            fileInput("user_shape",
                      HTML("Upload shapefile or KML: <br/> <em> (.shp, .shx, .dbf,
                           and .prj files are all <br/> required for shapefile upload)</em>"),
                      multiple = TRUE,
                      accept = c(
                        ".kml",
                        ".shx", ".shp", ".sbn", ".sbx", ".dbf", ".prj"
                      )
            ),
            actionButton("plot_footprint", "Plot shapefile or KML", style = blue_button),
            tags$hr(),
            selectizeInput("sg_key", "Search with 21 digit SG key:",
                           choices = c("Enter SG key" = "", sgdata),
                           options=list(create=FALSE, selectOnTab = TRUE)
            ),
            actionButton("search_prop", "Plot property", style = blue_button),
            br(),
            tags$hr(),
            tags$b("Enter latitude and longitude:"),
            tags$p(),
            numericInput("lat", "Latitude", value = -30.375),
            numericInput("long", "Longitude", value = 30.6858),
            actionButton("add_point", "Add point",
                         style = blue_button
            )
          )
        ),
        column(
          width = 9,
          box(
            title = NULL, width = NULL, solidHeader = TRUE,
            div(
              id = "nogomapdiv",
              leafglOutput("nogomap", width = "100%", height = 620) %>%
                withSpinner(type = 1, size = 1.5)
            ),
            div(
              id = "clearcontroldiv",
              absolutePanel(
                id = "clear_control", class = "panel panel-default",
                fixed = TRUE, draggable = FALSE,
                top = 350, right = 30, left = "auto", bottom = "auto",
                width = "auto", height = "auto",
                actionButton("map_reset", "Clear map inputs")
              )
            ),
            shinyjs::hidden(
              div(
                id = "downloaddiv",
                absolutePanel(
                  id = "download_shapefile", class = "panel panel-default",
                  fixed = TRUE, draggable = FALSE,
                  top = 350, right = 720, left = "auto", bottom = "auto",
                  width = "auto", height = "auto",
                  downloadButton("downloadData", "Download hand-drawn shape")
                )
              )
            )
          )
        )
      )
    ),
    tabItem(
      tabName = "data_output_01",
      fluidRow(
        box(
          # title = "Species data - shapefile or KML",
          title = "Species data",
          width = 8, solidHeader = TRUE, status = "primary",
          gt_output(outputId = "sens_feat_table_user"),
          collapsible = TRUE,
          br(),
          downloadButton(outputId = "download_species_01", label = "Download species data")
        )
      ),
      fluidRow(
        box(
          # title = "Farm/ERF property data - shapefile or KML",
          title = "Property data",
          width = 12, solidHeader = TRUE, status = "primary",
          collapsible = TRUE,
          gt_output("property_table_user"),
          br(),
          downloadButton(outputId = "download_property_01", label = "Download property data")
        )
      )
    ),

    tabItem(
      tabName = "data_output_02",
      fluidRow(
        box(
          # title = "Species data - SG code",
          title = "Species data",
          width = 8, solidHeader = TRUE, status = "success",
          collapsible = TRUE,
          gt_output("sens_feat_table_sg"),
          br(),
          downloadButton(outputId = "download_species_02", label = "Download species data")
        )
      ),
      fluidRow(
        box(
          # title = "Farm/ERF property data - SG code",
          title = "Property data",
          width = 12, solidHeader = TRUE, status = "success",
          collapsible = TRUE,
          gt_output("property_table_sg"),
          br(),
          downloadButton(outputId = "download_property_02", label = "Download property data")
        )
      )
    ),

    tabItem(
      tabName = "data_output_03",
      fluidRow(
        box(
          # title = "Species data - Lat/Long point",
          title = "Species data",
          width = 8, solidHeader = TRUE, status = "warning",
          collapsible = TRUE,
          gt_output("sens_feat_table_point"),
          br(),
          downloadButton(outputId = "download_species_03", label = "Download species data")
        )
      ),
      fluidRow(
        box(
          # title = "Farm/ERF property data - Lat/Long point",
          title = "Property data",
          width = 12, solidHeader = TRUE, status = "warning",
          collapsible = TRUE,
          gt_output("property_table_point"),
          br(),
          downloadButton(outputId = "download_property_03", label = "Download property data")
        )
      )
    ),

    tabItem(
      tabName = "data_output_04",
      fluidRow(
        box(
          # title = "Species data - hand drawn polygon",
          title = "Species data",
          width = 8, solidHeader = TRUE, status = "danger",
          # height = 200,
          collapsible = TRUE,
          gt_output("sens_feat_table_hand"),
          br(),
          downloadButton(outputId = "download_species_04", label = "Download species data")
        )
      ),
      fluidRow(
        box(
          # title = "Farm/ERF property data - hand drawn polygon",
          title = "Property data",
          width = 12, solidHeader = TRUE, status = "danger",
          collapsible = TRUE,
          gt_output("property_table_hand"),
          br(),
          downloadButton(outputId = "download_property_04", label = "Download property data")
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
