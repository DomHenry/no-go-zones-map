## UI ----
header <- dashboardHeader(
  title = "Threatened Species No-Go Map",
  titleWidth = 350
)

blue_button <- "color: #fff; background-color: #337ab7; border-color: #2e6da4"

# Basically what happens is, when you insert other input elements into a menuItem, it loses the data-toggle and data-value attributes. Because of this, tabItems in dashboardBody can't link with the menuItems anymore and thus the app can't display the content in the body.

# Fix: https://stackoverflow.com/questions/50245925/tabitem-cannot-show-the-content-when-more-functions-in-menuitem-using-shiny-and

# See also: https://rstudio.github.io/shinydashboard/behavior.html

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}


## Sidebar ----
sidebar <- dashboardSidebar(
  width = 350,
  useShinyjs(), # Add this to allow shinyjs functions to work in server
  tags$head(
    tags$style(HTML("
                      .sidebar { height: 90vh; overflow-y: auto; }
                      "))
  ),
  sidebarMenu(
    convertMenuItem(
      menuItem("Interactive map",
      tabName = "int_map", icon = icon("map-marked-alt"),
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
      tags$b(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"Enter point (decimal degrees):"),
      tags$p(),
      fillRow(
        height = "100%", width = "100%", flex = 1,
        numericInput("lat", "Latitude", value = NULL, max = 0), #-30.375
        numericInput("long", "Longitude", value = NULL) #30.6858
      ),
      br(),
      br(),
      br(),
      br(),
      actionButton("add_point", "Add point",
                   style = blue_button
      ),
      tags$hr(),
      selectizeInput(
        inputId = "spp_choice", label = "Species",
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
        multiple = FALSE,
        options = list(
          placeholder = "Start typing or select from dropdown",
          onInitialize = I('function() { this.setValue("a"); }')
        )
      ),
      actionButton("plot_spp", "Plot species", style = blue_button),

      br(),
      br()

    ),
    tabName = "int_map"),

    menuItem("Data table outputs", tabName = "tables", icon = icon("table")),
    menuItem("Help", tabName = "help", icon = icon("question-circle"))
        )
)


# More buttons options
# https://getbootstrap.com/docs/4.0/components/buttons/

tab_font <- "font-size:16px"

## Body ----
body <- dashboardBody(
  tags$head(
    tags$style(type='text/css',
               ".nav-tabs {font-size: 15px; font-weight: bold}
               .main-sidebar {font-size: 14px; font-weight: normal}")),
  tabItems(
    tabItem(
      tabName = "int_map",
      fluidRow(
        column(
          width = 12,
          box(
            title = NULL, width = NULL, solidHeader = TRUE,
            div(
              id = "nogomapdiv",
              leafletOutput("nogomap", width = "100%", height = 600) %>%
                withSpinner(type = 1, size = 1.5)
            ),
            # div(
            #   id = "clearcontroldiv",
            #   absolutePanel(
            #     id = "clear_control", class = "panel panel-default",
            #     fixed = TRUE, draggable = FALSE,
            #     top = 350, right = 30, left = "auto", bottom = "auto",
            #     width = "auto", height = "auto",
            #     actionButton("map_reset", "Clear map inputs")
            #   )
            # ),
            shinyjs::hidden(
              div(
                id = "downloaddiv",
                absolutePanel(
                  id = "download_shapefile", class = "panel panel-default",
                  fixed = TRUE, draggable = FALSE,
                  top = 350, right = 920, left = "auto", bottom = "auto",
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
                  title = "Species", width = 12, collapsible = TRUE, collapsed = FALSE,
                  solidHeader = TRUE, status = "success",
                  gt_output(outputId = "sens_feat_table_user"),
                  br(),
                  downloadButton(outputId = "download_species_01", label = "Download species data")
                )
              ),
              fluidRow(
                box(
                title = "Property", width = 12, collapsible = TRUE, collapsed = TRUE,
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
                  title = "Species", width = 12, collapsible = TRUE, collapsed = FALSE,
                  solidHeader = TRUE, status = "success",
                  gt_output("sens_feat_table_sg"),
                  br(),
                  downloadButton(outputId = "download_species_02", label = "Download species data")
                )
              ),
              fluidRow(
                box(
                  title = "Property", width = 12, collapsible = TRUE, collapsed = TRUE,
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
                  title = "Species", width = 12, collapsible = TRUE, collapsed = FALSE,
                  solidHeader = TRUE, status = "success",
                  gt_output("sens_feat_table_point"),
                  br(),
                  downloadButton(outputId = "download_species_03", label = "Download species data")
                )
              ),
              fluidRow(
                box(
                  title = "Property", width = 12, collapsible = TRUE, collapsed = TRUE,
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
                  title = "Species", width = 12, collapsible = TRUE, collapsed = FALSE,
                  solidHeader = TRUE, status = "success",
                  gt_output("sens_feat_table_hand"),
                  br(),
                  downloadButton(outputId = "download_species_04", label = "Download species data")
                )
              ),
              fluidRow(
                box(
                  title = "Property", width = 12, collapsible = TRUE, collapsed = TRUE,
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
      h2(tags$b("Help")),
      h3("Visit the EWT No-Go Map companion",
         tags$a("webpage", href = "https://www.ewt.org.za/resources/no-go-screening-tool/"),
         "for more information about the project and the data used in the map."),
      h3("Click",
         tags$a("here",
                href = "https://www.youtube.com/watch?v=98ynFNbRZ_A&t=1s"),
         "to watch a short demo of how to use the No-go map."),
      br(),
      h2(tags$b("Feedback")),
      h3("Do you have suggestions or queries, or would you like to report something that is not
         working in the app?"),
      h3("If so, please contact us at science@ewt.org.za"),
      h3("Thank you!")
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
