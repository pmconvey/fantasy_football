# readme ---------------------------------------------------------------------

# This is a shiny module that goes with my Fantasy Football shiny app.
# This module retrieves loads data from csv files and is meant to act as a
# demonstration of what the app can do for people who are not in a fantasy
# football league.

# Load r packages
require(here)
require(shiny)
require(magrittr)
require(data.table)

# shiny module ---------------------------------------------------------------

# UI module that provides a button that displays the message that data was
# successfully retrieved
demo_ui <- function(id) {
  tagList(
    actionButton(NS(id, "get_data"), "Get Data"),
    tags$h2(textOutput(NS(id, "success")), style = "color:green")
  )
}

# Server module that loads data from csv files and stores it in a list
# where each element is a a reactive data frame that can then be used by
# other shiny modules.
demo_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    demo_settings <- fread(here("demo_data", "demo_settings_league.csv"))
    demo_settings_schedule <- fread(here("demo_data", "demo_settings_schedule.csv"))
    demo_settings_roster <- fread(here("demo_data", "demo_settings_rosters.csv"))
    demo_settings_scoring <- fread(here("demo_data", "demo_settings_scoring.csv"))
    demo_users <- fread(here("demo_data", "demo_teams.csv"))
    demo_drafts <- fread(here("demo_data", "demo_drafts.csv"))
    demo_schedules <- fread(here("demo_data", "demo_schedules.csv"))
    demo_rosters <- fread(here("demo_data", "demo_rosters.csv"))
    
    ds <- reactive({demo_settings}) %>%
      bindEvent(input$get_data)
    dss <- reactive({demo_settings_schedule}) %>%
      bindEvent(input$get_data)
    dsr <- reactive({demo_settings_roster}) %>%
      bindEvent(input$get_data)
    dsp <- reactive({demo_settings_scoring}) %>%
      bindEvent(input$get_data)
    dt <- reactive({demo_users}) %>%
      bindEvent(input$get_data)
    dd <- reactive({demo_drafts}) %>%
      bindEvent(input$get_data)
    dm <- reactive({demo_schedules}) %>%
      bindEvent(input$get_data)
    dr <- reactive({demo_rosters}) %>%
      bindEvent(input$get_data)
    
    # Message displaying that data was successfully loaded
    output$success <- renderText({
      league_name <- tail(demo_settings$LeagueName, 1)
      
      paste(league_name, "data successfully retrieved")
    }) %>%
      bindEvent(input$get_data)
    
    # Create list that will hold all of the Demo data so that it can be used
    # by other shiny modules
    list(settings_league = ds,
         settings_schedule = dss,
         settings_rosters = dsr,
         settings_scoring = dsp,
         teams = dt,
         drafts = dd,
         schedules = dm,
         rosters = dr)
  })
}

# test the module ------------------------------------------------------------

# uncomment the code below to test the module in isolation from the rest of
# the app

# test_app <- function() {
#   ui <- fluidPage(
#     demo_ui("demo"),
#     verbatimTextOutput("test")
#   )
# 
#   server <- function(input, output, session) {
#     test_data <- demo_server("demo")
#     output$test <- renderPrint({
#      head(test_data$rosters(), 10)
#     })
#   }
# 
#   shinyApp(ui, server)
# }
# 
# test_app()
