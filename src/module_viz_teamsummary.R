# readme ---------------------------------------------------------------------

# This script is a shiny module that goes with my Fantasy Football Data
# Retriever shiny app. This module shows the end of the year standings for
# every year the league has been around with a heatmap where the y-axis is
# the teams in the league and the x-axis is the seasons. There is also a table
# that shows the number of championships and toilet bowls for each team with
# additional detail for each season if the table is expanded.

# Load r packages
require(here)
require(shiny)
require(magrittr)
require(tidyverse)
require(reactable)
require(htmltools)

source(here("src", "functions_viz.R"))

# shiny module ---------------------------------------------------------------

teamsummary_ui <- function(id) {
  tagList(
    plotOutput(NS(id, "league_outcomes"),
                 height = "600px"),
    br(),
    fluidRow(column(width = 10, offset = 1,
                    reactableOutput(NS(id, "team_summary")))
    )
  )
}

teamsummary_server <- function(id, viz_colors, data) {
  moduleServer(id, function(input, output, session) {
    teams_season <- reactive({
      data() %>%
        mutate(StandingsRank = uwf_format_ordinal(as.integer(StandingsRank)),
               TableLabel = case_when(Champion == 1 ~
                                        str_c(StandingsRank, " 🏆"),
                                      Loser == 1 ~
                                        str_c(StandingsRank, " 🚽"),
                                      TRUE ~ StandingsRank))
    })
    
    output$league_outcomes <- renderPlot({
      heatmap <-
        teams_season() %>%
        group_by(OwnerId) %>%
        mutate(outcome = case_when(MadePlayoffs == 1 ~ "Made Playoffs",
                                   MadePlayoffs == 0 ~ "Missed Playoffs",
                                   TRUE ~ NA_character_),
               first_season = min(FantasySeason),
               last_season = max(FantasySeason)) %>%
        ungroup() %>%
        arrange(desc(last_season), first_season, OwnerName) %>%
        mutate(owner_order = row_number()) %>%
        group_by(OwnerId) %>%
        mutate(owner_order = min(owner_order)) %>%
        ungroup() %>%
        ggplot(aes(x = FantasySeason,
                   y = reorder(OwnerName, -owner_order),
                   label = TableLabel,
                   fill = outcome)) +
        geom_tile(colour = "#FFFFFF") +
        geom_text() +
        scale_fill_manual(values = c(viz_colors$col5(),
                                     viz_colors$col6(),
                                     viz_colors$col5())) +
        scale_x_continuous(breaks = seq(min(teams_season()$FantasySeason),
                                        max(teams_season()$FantasySeason),
                                        1L)) +
        coord_cartesian(expand = FALSE) +
        labs(title = "Final Standings by Year",
             x = NULL,
             y = NULL) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor.y = element_line(colour = "#FFFFFF"),
              plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
              legend.title = element_blank(),
              legend.position = "right")
      
      heatmap
    })
    
    output$team_summary <- renderReactable({
      teams_agg <-
        teams_season() %>%
        group_by(OwnerName) %>%
        summarise(Seasons = n_distinct(FantasySeason),
                  last_season = max(FantasySeason),
                  Championships = sum(Champion, na.rm = TRUE),
                  Punishments = sum(Loser, na.rm = TRUE),
                  .groups = "drop") %>%
        arrange(desc(last_season), OwnerName) %>%
        select(-last_season)
      
      teams_detail <-
        teams_season() %>%
        mutate(outcome = case_when(Champion == 1 ~ "Won Championship",
                                   Loser == 1 ~ "Last Place",
                                   MadePlayoffs == 1 ~ "Made Playoffs",
                                   MadePlayoffs == 0 ~ "Missed Playoffs",
                                   TRUE ~ NA_character_),
               outcome2 = case_when(Champion == 1 ~ "trophy",
                                    Loser == 1 ~ "toilet",
                                    MadePlayoffs == 1 ~ "award",
                                    MadePlayoffs == 0 ~ "exclamation-circle",
                                    TRUE ~ NA_character_)) %>%
        select(OwnerName, FantasySeason, outcome2, outcome, StandingsRank,
               SeasonWins, SeasonPointsFor, SeasonPointsAgainst) %>%
        rename("Season" = "FantasySeason",
               "Outcome" = "outcome",
               "Place" = "StandingsRank",
               "Wins" = "SeasonWins",
               "Points For" = "SeasonPointsFor",
               "Points Against" = "SeasonPointsAgainst")
      
      reactable(teams_agg,
                columns =
                  list(OwnerName = colDef(name = "",
                                          minWidth = 200,
                                          maxWidth = 200),
                       Seasons = colDef(minWidth = 125,
                                        maxWidth = 125,
                                        align = "center"),
                       Championships =
                         colDef(cell = function(value) table_icons(value,
                                                                   "trophy"),
                                minWidth = 200,
                                maxWidth = 200,
                                align = "left"),
                       Punishments =
                         colDef(name = "Last Place Finishes",
                                minWidth = 200,
                                maxWidth = 200,
                                cell = function(value) table_icons(value,
                                                                   "toilet"),
                                align = "left")),
                details = function(index) {
                  tbl <-
                    filter(teams_detail,
                           OwnerName == teams_agg$OwnerName[index]) %>%
                    select(-OwnerName) %>%
                    reactable(columns = 
                                list(outcome2 =
                                       colDef(name = "",
                                              cell = function(value) {
                                                table_icons(1, value)
                                              },
                                              align = "right",
                                              maxWidth = 50),
                                     Outcome = colDef(maxWidth = 200,
                                                      align = "left")),
                              defaultColDef = colDef(maxWidth = 115,
                                                     align = "center"),
                              outlined = TRUE,
                              highlight = TRUE)
                  htmltools::div(style = list(margin = "12px 45px"), tbl)
                },
                highlight = TRUE,
                defaultPageSize = 25)
    })
  })
}

# test the module ------------------------------------------------------------

# uncomment the code below to test the module in isolation from the rest of
# the app

# test_app <- function() {
#   ui <-
#     fluidPage(
#       teamsummary_ui("viz")
#     )
# 
#   server <- function(input, output, session) {
#     color_list <-
#       list(
#         col1 = reactive({'#29ba1f'}),
#         col2 = reactive({'#dcdcdc'}),
#         col3 = reactive({'#646464'}),
#         col4 = reactive({'#29ba1f'}),
#         col5 = reactive({'#dcdcdc'}),
#         col6 = reactive({'#f93c4a'})
#       )
# 
#     test_data <- reactive({
#       read_csv(here("demo_data", "team_season.csv"),
#                col_types = cols())
#     })
# 
#     teamsummary_server("viz", color_list, test_data)
#   }
# 
#   shinyApp(ui, server)
# }
# 
# test_app()
