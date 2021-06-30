## UI ----
header <- dashboardHeader(
  title = "Threatend Species No-Go Map",
  titleWidth = 320
)

## Sidebar ----
sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem("Welcome",
      tabName = "welcome", icon = icon("info-circle"),
      selected = TRUE    ),
    menuItem("Interactive map",
      tabName = "int_map", icon = icon("map-marked-alt"),
      selected = FALSE
    ),
    menuItem("Data table outputs", tabName = "tables", icon = icon("table")),
    menuItem("Help", tabName = "help", icon = icon("question-circle"))
  )
)

blue_button <- "color: #fff; background-color: #337ab7; border-color: #2e6da4"

# More buttons options
# https://getbootstrap.com/docs/4.0/components/buttons/

tab_font <- "font-size:16px"

## Body ----
body <- dashboardBody(
  tags$head(
    tags$style(type='text/css',
               ".nav-tabs {font-size: 15px; font-weight: bold}
               .main-sidebar {font-size: 16px;}")),
  tabItems(
    tabItem(
      tabName = "welcome",
      fluidRow(
        column(
          width = 10,
          tabBox(
            id = "tabvals",
            width = NULL,
            tabPanel(
              title = "Background",
              style = tab_font,
              br(),
              includeMarkdown("data_input/01_background.Rmd"),
              # img(src = "ewt_01.png", height = "100px"),
              value = 1
            ),
            tabPanel(
              title = "Purpose",
              style = tab_font,
              br(),
              includeMarkdown("data_input/02_purpose.Rmd"),
              value = 2
            ),
            tabPanel(
              title = "How are no-go areas defined?",
              style = tab_font,
              br(),
              includeMarkdown("data_input/03_defined.Rmd"),
              img(src = "Infographic_clipped.png", height = "500px"),
              value = 2
            ),
            tabPanel(
              title = "Species summaries",
              style = tab_font,
              br(),
              includeMarkdown("data_input/04_species.Rmd"),
              gt_output("spp_summary_table"),
              br(),
              downloadButton("download_species_list", "Download species list"),
              value = 3
            ),
            tabPanel(
              title = "How it works",
              style = tab_font,
              br(),
              includeMarkdown("data_input/05_how it works.Rmd"),
              value = 4
            ),
            tabPanel(
              title = "How can you contribute?",
              style = tab_font,
              br(),
              includeMarkdown("data_input/06_contribute.Rmd"),
              value = 5
            ),
            tabPanel(
              title = "Resources",
              style = tab_font,
              br(),
              includeMarkdown("data_input/07_resources.Rmd"),
              value = 6
            )
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
              options = list(create = FALSE, selectOnTab = TRUE)
            ),
            actionButton("search_prop", "Plot property", style = blue_button),
            br(),
            tags$hr(),
            tags$b("Enter point (decimal degrees):"),
            tags$p(),
            fillRow(
              height = "100%", width = "100%", flex = 1,
              numericInput("lat", "Latitude", value = -30.375),
              numericInput("long", "Longitude", value = 30.6858)
              ),
            br(),
            br(),
            br(),
            br(),
            actionButton("add_point", "Add point",
              style = blue_button
            ),
            selectizeInput(
              inputId = "species_choice", label = "Species",
              choices = list(
                `Amphibia` = spp_list %>% filter(CLASS == "Amphibia") %>% pull(SENSFEAT),
                `Arachnida` = spp_list %>% filter(CLASS == "Arachnida") %>% pull(SENSFEAT),
                `Aves` = spp_list %>% filter(CLASS == "Aves") %>% pull(SENSFEAT),
                `Insecta` = spp_list %>% filter(CLASS == "Insecta") %>% pull(SENSFEAT),
                `Invertebrate` = spp_list %>% filter(CLASS == "Invertebrate") %>% pull(SENSFEAT),
                `Mammalia` = spp_list %>% filter(CLASS == "Mammalia") %>% pull(SENSFEAT)
              ),
              selected = NULL,
              multiple = FALSE,
              options = list(
                placeholder = "Make you choice"
              )
            )
          )
        ),
        column(
          width = 9,
          box(
            title = NULL, width = NULL, solidHeader = TRUE,
            div(
              id = "nogomapdiv",
              leafletOutput("nogomap", width = "100%", height = 620) %>%
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
      tabName = "tables",
      fluidRow(
        column(
          width = 9,
          tabBox(
            id = "output_vals",
            width = NULL,
            tabPanel(
              title = "Shapefile/KML",
              value = 1,
              fluidRow(
                box(
                  title = "Species", width = 12, collapsible = TRUE,
                  solidHeader = TRUE, status = "success",
                  gt_output(outputId = "sens_feat_table_user"),
                  br(),
                  downloadButton(outputId = "download_species_01", label = "Download species data")
                )
              ),
              fluidRow(
                box(
                title = "Property", width = 12, collapsible = TRUE,
                solidHeader = TRUE, status = "success",
                gt_output("property_table_user"),
                br(),
                downloadButton(outputId = "download_property_01", label = "Download property data")
              ))
            ),
            tabPanel(
              title = "SG key",
              value = 2,
              fluidRow(
                box(
                  title = "Species", width = 12, collapsible = TRUE,
                  solidHeader = TRUE, status = "success",
                  gt_output("sens_feat_table_sg"),
                  br(),
                  downloadButton(outputId = "download_species_02", label = "Download species data")
                )
              ),
              fluidRow(
                box(
                  title = "Property", width = 12, collapsible = TRUE,
                  solidHeader = TRUE, status = "success",
                  gt_output("property_table_sg"),
                  br(),
                  downloadButton(outputId = "download_property_02", label = "Download property data")
                )
              )
            ),
            tabPanel(
              title = "Point (latitude & longitude)",
              value = 3,
              fluidRow(
                box(
                  title = "Species", width = 12, collapsible = TRUE,
                  solidHeader = TRUE, status = "success",
                  gt_output("sens_feat_table_point"),
                  br(),
                  downloadButton(outputId = "download_species_03", label = "Download species data")
                )
              ),
              fluidRow(
                box(
                  title = "Properties", width = 12, collapsible = TRUE,
                  solidHeader = TRUE, status = "success",
                  gt_output("property_table_point"),
                  br(),
                  downloadButton(outputId = "download_property_03", label = "Download property data")
                )
              )
            ),
            tabPanel(
              title = "Hand drawn polygon",
              value = 4,
              fluidRow(
                box(
                  title = "Species", width = 12, collapsible = TRUE,
                  solidHeader = TRUE, status = "success",
                  gt_output("sens_feat_table_hand"),
                  br(),
                  downloadButton(outputId = "download_species_04", label = "Download species data")
                )
              ),
              fluidRow(
                box(
                  title = "Property", width = 12, collapsible = TRUE,
                  solidHeader = TRUE, status = "success",
                  gt_output("property_table_hand"),
                  br(),
                  downloadButton(outputId = "download_property_04", label = "Download property data")
                )
              )
            )
          )
        )
      )
    ),

    tabItem(
      tabName = "help",
      h2("Suggestions? Queries? Bugs? Need help?"),
      h3("Contact us on science@ewt.org.za")
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
