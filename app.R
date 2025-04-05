library(shiny)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(vroom)
library(DT)

reactlog::reactlog_enable()

source("load_vl_data.R")
source("uiVars.R")       # the filter 'dictionary' which contains the variable uiVars
source("filterModule.R") # filter module

ui <-
  fluidPage(
    titlePanel(""),
    sidebarLayout(
      sidebarPanel(
        width = 2,
        filterUI("filterID")),
      mainPanel(
        tabsetPanel(type = "pills",
                    tabPanel("Map",
                             leafletOutput("map",
                                           width = "125%",
                                           height = 850)),
                    tabPanel("Data explorer",
                             DTOutput(outputId = "table"),
                             awesomeRadio("table_select",
                               label = "Each row corresponds to unique", 
                               choices = c("study/site/drug" = "ssd", "study/site" = "ss", "study" = "s"),
                               selected = "ssd",
                               inline = TRUE, 
                               checkbox = TRUE),
                             verbatimTextOutput("debug")),
                    tabPanel("Timeline"))
        )
    ))

server <- function(input, output, session) {
  
  # the filter module outputs a reactive logical vector (length of vl_data)
  # updating of the selectInput choices is done within the module server (and much pain was suffered)
  filter_combined <- filterServer("filterID") 

  # update table according to switch
  vl_table <- reactive({
    if (input$table_select == "ssd") {
      vl_data[filter_combined(),]
    } else if (input$table_select == "ss") {
      vl_data[filter_combined(),] %>% select(-c("drugs")) %>% distinct()
    } else if (input$table_select == "s") {
      vl_data[filter_combined(),] %>% select(-c("country", "drugs", "lat", "study_no", "site_name", "region", "lon", "other_drugs", "otherCountries")) %>% distinct()
    }
  })

  # output table in the table panel
  output$table <- renderDT(
    vl_table(),
    class = "stripe nowrap",
    rownames = FALSE,
    options = list(
      lengthChange = FALSE,
      pageLength = 15)
    )
  
  output$debug <- renderPrint(input$table_select)
  
  # create filter for unique map markers - ("study_no" in the original dataset, which we will call map_id here)
  # so, map_id is just a vector of numbers, each number identifies the location
  
  filter_history <- reactiveValues(
    previous = rep(TRUE, nrow(vl_data)), 
    current = rep(TRUE, nrow(vl_data)))
  
  observeEvent(filter_combined(), {
               filter_history$previous <- filter_history$current
               filter_history$current <- filter_combined()
  })
  
  study_no_current <- reactive({
    unique(vl_map$study_no[filter_history$current])
  })
  
  study_no_previous <- reactive({
    unique(vl_map$study_no[filter_history$previous])
  })
  
  # list of study_no to add
  add_markers <- reactive({
    study_no_current()[!(study_no_current() %in% study_no_previous())]
  })
  
  # list of study_no to remove
  remove_markers <- reactive({
    study_no_previous()[!(study_no_previous() %in% study_no_current())]
  })
  
  
  # create map first time with markers
  output$map <- renderLeaflet({
    status_pal <- colorFactor("Set1", vl_map$status)
    distinct(vl_map[isolate(filter_combined()), ]) %>% 
      leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(lat = ~ lat, 
                 lng = ~ lon, 
                 layerId = ~ as.character(study_no),
                 color = "black",
                 weight = 1,
                 #clusterOptions = markerClusterOptions(),
                 fillColor =   ~ status_pal(status),
                 fillOpacity = 1,
                 popup = ~popup) %>% 
      addLegend("bottomright", pal = status_pal, values = ~status,
                title = "Study status",
                opacity = 0.9)
  })

  # remove markers from map (only runs when markers are removed)
  observe({
    req(remove_markers())
    leafletProxy("map") %>%
      removeMarker(layerId = as.character(remove_markers()))
  })

  # add markers from map (only runs when markers are added)
  observe({
    req(add_markers())
    status_pal <- colorFactor("Set1", vl_map$status)
    leafletProxy("map") %>%
      addCircleMarkers(
                 data =      distinct(vl_map[vl_map$study_no %in% add_markers(),]),
                 lng =     ~ lon,
                 lat =     ~ lat,
                 layerId = ~ as.character(study_no),
                 color = "black",
                 weight = 1,
                 fillColor =   ~ status_pal(status),
                 fillOpacity = 0.9,
                 popup = ~popup)
  })
}

shinyApp(ui, server)

