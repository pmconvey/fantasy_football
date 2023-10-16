# readme ---------------------------------------------------------------------

# This script is a shiny module that goes with my Fantasy Football Data
# Retriever shiny app. This module creates a chart showing how many points
# the teams in the league scored each week during a single season.

# Load r packages
require(here)
require(shiny)
require(magrittr)
require(tidyverse)
require(ggbeeswarm)
require(plotly)

source(here("src", "functions_viz.R"))

# shiny module ---------------------------------------------------------------

weeklypoints_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        uiOutput(NS(id, "team_list")),
        uiOutput(NS(id, "season_list")),
        checkboxInput(NS(id, "playoffs"), "Include Playoffs?"),
        hr(),
        radioButtons(NS(id, "xaxis"), "X-Axis",
                     choices = c("Team", "Week"),
                     inline = TRUE),
        selectInput(NS(id, "metric"), "Metric",
                    choices = c("Points For",
                                "Points Against",
                                "Matchup Margin",
                                "Matchup Margin Percent",
                                "Week Rank",
                                "Opponent Week Rank"),
                    width = 200L),
        tags$p(tags$strong("Points For:"), "number of points the team",
               "scored for each week"),
        tags$p(tags$strong("Points Against:"), "number of points the",
               "opponent's team scored for each week"),
        tags$p(tags$strong("Matchup Margin:"), "the difference between the",
               "number of points the team scored and the number of points",
               "the opponent's team scored"),
        tags$p(tags$strong("Matchup Margin Percent:"), "the percentage",
               "difference between the number of points the team scored and",
               "the number of points the opponent's team scored"),
        tags$p(tags$strong("Week Rank:"), "the rank of the team by their",
               "points scored during the week. The team that scored the",
               "fewest points is 1, the 2nd fewest is 2, up to the team that",
               "scored the most points."),
        tags$p(tags$strong("Opponent Week Rank:"), "the rank of the team by",
               "the number of points their opponent scored during the week.",
               "The team that had the fewest points against is 1, the 2nd",
               "fewest is 2, up to the team that had the most points",
               "against.")
      ),
      mainPanel(
        width = 9,
        tags$h3(textOutput(NS(id, "plot_title"))),
        tags$p("each circle represents one team's points in a week"),
        plotlyOutput(NS(id, "points"), height = "650px")
      )
    )
  )
}

weeklypoints_server <- function(id, viz_colors, viz_highlight, data) {
  moduleServer(id, function(input, output, session) {

    output$team_list <- renderUI({
      selectInput(NS(id, "team_choose"),
                  "Team to highlight",
                  choices = c("Select team" = "",
                              sort(unique(data()$OwnerName))),
                  selected = viz_highlight(),
                  width = 150L)
    })
    
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
    
    plot_data <- reactive({
      data() %>%
        filter(FantasySeason >= min(input$season_select)
               & FantasySeason <= max(input$season_select)
               & (WeekType == "Regular" | input$playoffs)) %>%
        select(FantasySeason, FantasyWeek, TeamId, PointsFor, PointsAgainst,
               WinTieLoss, WeekRank, OppWeekRank, OwnerName, OppOwnerName) %>%
        mutate(selected_team = OwnerName == input$team_choose,
               margin_abs = PointsFor - PointsAgainst,
               margin_pct = 100 * (PointsFor - PointsAgainst) / PointsFor,
               value = case_when(input$metric == "Points For" ~ PointsFor,
                                 input$metric == "Points Against" ~ PointsAgainst,
                                 input$metric == "Matchup Margin" ~ margin_abs,
                                 input$metric == "Matchup Margin Percent" ~ margin_pct,
                                 input$metric == "Week Rank" ~ WeekRank,
                                 input$metric == "Opponent Week Rank" ~ OppWeekRank)) %>%
        arrange(FantasySeason, FantasyWeek, TeamId)
    })
    
    plot_week <- reactive({
      ggplot(plot_data(),
             aes(x = FantasyWeek,
                 y = value,
                 shape = WinTieLoss,
                 fill = selected_team,
                 color = selected_team,
                 text = paste0(OwnerName, "\n", PointsFor, " - ",
                               PointsAgainst, " (", OppOwnerName,
                               ")\nMargin: ", round(margin_abs, digits = 2),
                               " points (", round(margin_pct, digits = 1),
                               "%)"))) +
        geom_beeswarm(size = 2, method = "swarm") +
        scale_x_continuous(labels = seq(1:20), breaks = seq(1:20)) +
        scale_fill_manual(values = c(viz_colors$col2(), viz_colors$col1())) +
        scale_color_manual(values = c(viz_colors$col2(), viz_colors$col1())) +
        scale_shape_manual(values = c("Win" = 19, "Tie" = 1, "Loss" = 1)) +
        facet_grid(rows = vars(FantasySeason)) +
        labs(title = NULL,
             x = NULL,
             y = NULL,
             fill = NULL,
             color = NULL,
             shape = NULL) +
        # guides(fill = "none", color = "none") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_text(hjust = 0),
              legend.title = element_blank(),
              panel.grid.minor.x = element_blank())
    })
    
    plot_team <- reactive({
      ggplot(plot_data(),
             aes(x = OwnerName,
                 y = value,
                 shape = selected_team,
                 fill = WinTieLoss,
                 color = WinTieLoss,
                 text = paste0(OwnerName, " | ", FantasySeason, " Week ",
                               FantasyWeek, "\n", PointsFor, " - ",
                               PointsAgainst, " (", OppOwnerName,
                               ")\nMargin: ", round(margin_abs, digits = 2),
                               " points (", round(margin_pct, digits = 1),
                               "%)"))) +
        geom_beeswarm(size = 2, method = "swarm") +
        scale_fill_manual(values = c(viz_colors$col6(), viz_colors$col4())) +
        scale_color_manual(values = c(viz_colors$col6(), viz_colors$col4())) +
        scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 1)) +
        labs(title = NULL,
             x = NULL,
             y = NULL,
             fill = NULL,
             color = NULL,
             shape = NULL) +
        # guides(fill = "none", color = "none") +
        theme(legend.title = element_blank())
    })
    
    output$plot_title <- renderText({
      str_c(input$metric, " by ", input$xaxis)
    })
    
    output$points <- renderPlotly({
      req(input$season_select)
      
      # plot <-
      #   plot_ly(data = plot_data(),
      #           type = 'scatter',
      #           mode = 'markers',
      #           x = ~FantasyWeek,
      #           y = ~value,
      #           color = ~selected_team, colors = c(viz_colors$col2(),
      #                                              viz_colors$col1()),
      #           symbol = ~WinTieLoss, symbols = c('o', 'circle'),
      #           size = 7,
      #           text = ~paste0(OwnerName, ": ", PointsFor,
      #                          "\n", OppOwnerName, ": ", PointsAgainst,
      #                          "\nMargin: ", round(margin_abs, digits = 2),
      #                          " points (", round(margin_pct, digits = 1), "%)"),
      #           hoverinfo = 'text',
      #           showlegend = FALSE) %>%
      #   layout(yaxis = list(title = ""),
      #          xaxis = list(title = "Fantasy Week",
      #                       type = "category",
      #                       zeroline = FALSE,
      #                       tick0 = 1, dtick = 1,
      #                       tickson = "boundaries"))
      
      if (input$xaxis == "Week") {
        plot <- plot_week()
      } else if (input$xaxis == "Team") {
        plot <- plot_team()
      }
        
      ggplotly(plot, tooltip = c("text"))
    })
    
    reactive({input$team_choose})
  })
}

# test the module ------------------------------------------------------------

# uncomment the code below to test the module in isolation from the rest of
# the app

# test_app <- function() {
#   ui <-
#     fluidPage(
#       weeklypoints_ui("viz")
#     )
# 
#   server <- function(input, output, session) {
#     teams_week <- reactive({read_csv(here("demo_data", "team_week.csv"),
#                            col_types = cols())})
# 
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
#     weeklypoints_server("viz",
#                         color_list,
#                         viz_highlight = reactive({NA}),
#                         teams_week)
#   }
# 
#   shinyApp(ui, server)
# }
# 
# test_app()
