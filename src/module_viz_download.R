# readme ---------------------------------------------------------------------

# This is a shiny module that goes with my Fantasy Football shiny app.
# This module allows users to download the data that is retrieved by the app.

# Load r packages
require(here)
require(shiny)
require(gt)

# https://developer.mozilla.org/en-US/docs/Learn/CSS/Building_blocks/Styling_tables

# shiny module ---------------------------------------------------------------

data_dictionary <- read.csv(here("data-dictionary.csv"))

single_download_ui <- function(id) {
  tagList(
    tags$div(downloadButton(NS(id, "button"), "Download the Data"),
             br(),
             gt_output(NS(id, "datadictionary"))
    )
  )
}

single_download_server <- function(id, name, data) {
    moduleServer(id, function(input, output, session) {
      
      output$button <- downloadHandler(
        filename = paste(name, ".csv", sep = ""),
        content = function (file) {
          write.csv(data(), file, row.names = FALSE, na = "")
        }
      )
      
      output$datadictionary <- render_gt(
        data_dictionary[data_dictionary$dataset == name,
                        c("column", "definition")] |>
          gt() |>
          cols_label(column = "Column", definition = "Definition") |>
          tab_style(style = list(cell_text(weight = "bold")),
                    locations = cells_body(columns = column))
      )
  })
}

download_ui <- function(id) {
  tagList(
    navlistPanel("Select Data to Download",
                 header = tags$h4("Click the button below to download data",
                                  "from your fantasy football league"),
                 tabPanel("Team Week",
                          tags$p("The Team Week dataset has one row for each",
                                 "team, season, week combination. Refer to",
                                 "the table below for the fields that are",
                                 "included in the dataset and their",
                                 "definitions."),
                          br(),
                          single_download_ui(NS(id, "tw"))),
                 tabPanel("Team Season",
                          tags$p("The Team Season dataset has one row for",
                                 "each team and season combination. Refer to",
                                 "the table below for the fields that are",
                                 "included in the dataset and their",
                                 "definitions."),
                          br(),
                          single_download_ui(NS(id, "ts")))
    )
  )
}

download_server <- function(id, TeamWeek, TeamSeason) {
  moduleServer(id, function(input, output, session) {
    single_download_server("tw", "team_week", TeamWeek)
    single_download_server("ts", "team_season", TeamSeason)
  })
}

# test the module ------------------------------------------------------------

# uncomment the code below to test the module in isolation from the rest of
# the app

test_app <- function() {
  ui <- fluidPage(download_ui("test"))

  server <- function(input, output, session) {
    data1 <- reactive({read.csv(here("demo_data", "demo_teams.csv"))})
    data2 <- reactive({read.csv(here("demo_data", "demo_teams.csv"))})
    data3 <- reactive({read.csv(here("demo_data", "demo_teams.csv"))})
    data4 <- reactive({read.csv(here("demo_data", "demo_teams.csv"))})
    download_server("test", data1, data2)
  }

  shinyApp(ui, server)
}

test_app()
