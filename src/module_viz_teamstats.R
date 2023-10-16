# readme ---------------------------------------------------------------------

# This is a shiny module that goes with my Fantasy Football shiny app.
# This module visualizes stats for each team in the league aggregated across
# multiple seasons.

# Load r packages
require(here)
require(shiny)
require(magrittr)
require(data.table)
require(tidyverse)
require(reactable)

source(here("src", "functions_viz.R"))

# shiny module ---------------------------------------------------------------

teamstats_ui <- function (id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3L,
        uiOutput(NS(id, "team_list")),
        uiOutput(NS(id, "season_list")),
        checkboxInput(NS(id, "playoffs"), "Include Playoffs?"),
        checkboxInput(NS(id, "active_teams"), "Only Show Current Teams?",
                      value = TRUE),
        tags$hr(),
        selectInput(NS(id, "metric"),
                    "Metric",
                    choices = c("Winning Percentage",
                                "Points For Per Game",
                                "Points Against Per Game",
                                "Strength of Record",
                                "Schedule Difficulty"),
                    selected = "Winning Percentage",
                    width = 200L),
        tags$p(tags$strong("Winning Percentage:"),
               "number of wins divided by number of games played",
               "; ties count as half a win"),
        tags$p(tags$strong("Points For Per Game:"),
               "average (mean) of the points scored for each week"),
        tags$p(tags$strong("Points Against Per Game:"),
               "average (mean) of the points scored by the person's opponent",
               "for each week"),
        tags$p(tags$strong("Strength of Record:"),
               "average of the team's win probability for each week. Win",
               "probability is calculated by ranking teams in ascending",
               "order by how many points they scored during the week. The",
               "calculation is then based on the number of teams",
               "in the league -- ",
               "WinProb = ([team week rank] - 1) / ([# of teams] - 1).",
               "For example, in a 10 team league the team that scored the",
               "most points has a win probability of 100% (9/9); the 2nd",
               "most, 89% (8/9); down to the team that scored the fewest",
               "points, 0% (0/9)."),
        tags$p(tags$strong("Schedule Difficulty:"),
               "average of the team's loss probability for each week. Loss",
               "probability is calculated by ranking teams in ascending",
               "order by how many points their opponent scored during the",
               "week. The calculation is then based on the",
               "number of teams in the league and whether the team won or",
               "lost (this is done to remove the effect of that team,",
               "otherwise the better teams would consistently be rated as",
               "having easier schedules).",
               "[If win] LossProb = ([team opponent rank] - 1) /",
               "([# of teams] - 2); [If loss/tie] LossProb =",
               "([team opponent rank] - 2) / ([# of teams] - 2)")
      ),
      mainPanel(
        width = 9L,
        plotOutput(NS(id, "ts_plot"), height = "550px"),
        tags$div(style = "margin-bottom: 15px;"),
        reactableOutput(NS(id, "ts_table"))
      )
    )
  )
}

teamstats_server <- function(id, viz_colors, viz_highlight, data) {
  moduleServer(id, function(input, output, session) {
    output$season_list <- renderUI({
      sliderInput(NS(id, "season_select"),
                  "Seasons to include",
                  min = min(data()$FantasySeason),
                  max = max(data()$FantasySeason),
                  value = c(min(data()$FantasySeason),
                            max(data()$FantasySeason)),
                  step = 1L,
                  ticks = FALSE,
                  sep = "",
                  width = 200L)
    })
    outputOptions(output, "season_list", suspendWhenHidden = FALSE)
    
    output$team_list <- renderUI({
      selectInput(NS(id, "team_select"),
                  "Team to highlight",
                  choices = c("Select team" = "",
                              sort(unique(data()$OwnerName))),
                  selected = viz_highlight(),
                  width = 200L)
    })
    
    teams_filtered <- reactive({
      temp_df <- data()
      setDT(temp_df)
      
      active_teams <-
        unique(temp_df[FantasySeason == max(FantasySeason), OwnerId])
      
      temp_df <-
        temp_df[FantasySeason >= min(input$season_select) &
                FantasySeason <= max(input$season_select) &
                (WeekType == "Regular" | input$playoffs) &
                (OwnerId %in% active_teams | !input$active_teams),
                .(GP = .N,
                  WP = mean(fcase(WinTieLoss == "Win", 1,
                                  WinTieLoss == "Tie", 0.5,
                                  WinTieLoss == "Loss", 0),
                            na.rm = TRUE) * 100,
                  PPG = mean(PointsFor, na.rm = TRUE),
                  PAPG = mean(PointsAgainst, na.rm = TRUE),
                  SOR = mean(WinProb, na.rm = TRUE) * 100,
                  SOS = mean(LossProb, na.rm = TRUE) * 100),
                by = .(OwnerName, TeamName, TeamId, FantasySeason)
        ][, ':=' (Metric = switch(input$metric,
                              "Winning Percentage" = WP,
                              "Points For Per Game" = PPG,
                              "Points Against Per Game" = PAPG,
                              "Strength of Record" = SOR,
                              "Schedule Difficulty" = SOS))]
      
      return(temp_df)
    })
    
    output$ts_plot <- renderPlot({
      req(input$season_select)
      
      temp_df <- teams_filtered()
      setDT(temp_df)
      
      temp_df <-
        temp_df[,
                .(SelectedTeam = OwnerName == input$team_select,
                  Metric = weighted.mean(Metric, GP),
                  MetricFormat =
                    fcase(input$metric == "Winning Percentage",
                          format_number(weighted.mean(Metric, GP),
                                        digits = 1, end_sym = "%"),
                          input$metric == "Points For Per Game",
                          format_number(weighted.mean(Metric, GP),
                                        digits = 2),
                          input$metric == "Points Against Per Game",
                          format_number(weighted.mean(Metric, GP),
                                        digits = 2),
                          input$metric == "Strength of Record",
                          format_number(weighted.mean(Metric, GP) / 100,
                                        digits = 3),
                          input$metric == "Schedule Difficulty",
                          format_number(weighted.mean(Metric, GP) / 100,
                                        digits = 3))),
                by = OwnerName]
      
      bar_ylim <-
        switch(input$metric,
               "Winning Percentage" = c(0, 102),
               "Points For Per Game" = c(0, ceiling(max(temp_df$Metric) + 2L)),
               "Points Against Per Game" = c(0, ceiling(max(temp_df$Metric) + 2L)),
               "Strength of Record" = c(0, 102),
               "Schedule Difficulty" = c(0, 102))
      
      ggplot(temp_df,
             aes(x = reorder(OwnerName, Metric),
                 y = Metric,
                 fill = SelectedTeam,
                 label = MetricFormat,
                 hjust = 1.1)) +
        geom_col() +
        geom_text(color = "#FFFFFF", fontface = "bold") +
        scale_fill_manual(values = c(viz_colors$col2(), viz_colors$col1())) +
        scale_y_continuous(breaks = seq(0L, 200L, 10L)) +
        labs(title = input$metric,
             x = NULL,
             y = NULL) +
        coord_flip(ylim = bar_ylim, expand = FALSE) +
        theme(legend.position = "none",
              plot.title = element_text(face = "bold",
                                        size = 16))
    })
    
    output$ts_table <- renderReactable({
      req(input$season_select)
      
      row_highlight_function <-
        str_c("function(rowInfo) {
                    if (rowInfo.row['SelectedTeam']) {
                      return { borderLeft: '3px solid", viz_colors$col1(), "' }
                    }
                  }")
      
      teams_filtered() %>%
        mutate(SelectedTeam = ifelse(isTruthy(input$team_select),
                                     OwnerName == input$team_select, FALSE),
               SOR = SOR / 100,
               SOS = SOS / 100) %>%
        select(SelectedTeam, OwnerName, TeamName, FantasySeason,
               WP, PPG, PAPG, SOR, SOS, Metric) %>%
        reactable(columns =
                    list(SelectedTeam = colDef(show = FALSE),
                         OwnerName = colDef(name = "Owner",
                                            filterable = TRUE),
                         TeamName = colDef(name = "Team"),
                         FantasySeason = colDef(name = "Season"),
                         WP = colDef(name = "Winning Percentage",
                                     format =
                                       colFormat(digits = 1L,
                                                 suffix = "%"),
                                     align = "center"),
                         PPG = colDef(name = "Points For Per Game",
                                      format = colFormat(digits = 2L),
                                      align = "center"),
                         PAPG = colDef(name = "Points Against Per Game",
                                       format = colFormat(digits = 2L),
                                       align = "center"),
                         SOR = colDef(name = "Strength of Record",
                                      format = colFormat(digits = 3L),
                                      align = "center"),
                         SOS = colDef(name = "Schedule Difficulty",
                                      format = colFormat(digits = 3L),
                                      align = "center"),
                         Metric = colDef(show = FALSE)),
                  rowStyle = JS(row_highlight_function),
                  defaultSorted = "Metric",
                  defaultSortOrder = "desc",
                  highlight = TRUE)
    })
    
    reactive({input$team_select})
  })
}

# test the module ------------------------------------------------------------

# uncomment the code below to test the module in isolation from the rest of
# the app

# test_app <- function() {
#   ui <- fluidPage(teamstats_ui("viz"))
#   
#   server <- function(input, output, session) {
#     data_r <-
#       reactiveValues(color_list = list(col1 = reactive('#29ba1f'),
#                                        col2 = reactive('#dcdcdc'),
#                                        col3 = reactive('#646464'),
#                                        col4 = reactive('#29ba1f'),
#                                        col5 = reactive('#dcdcdc'),
#                                        col6 = reactive('#f93c4a')),
#                      highlight = reactive(NA))
# 
#     teamstats_server("viz",
#                      data_r$color_list,
#                      data_r$highlight,
#                      reactive({fread(here("demo_data", "team_week.csv"))}))
#   }
#   
#   shinyApp(ui, server)
# }
# 
# test_app()
