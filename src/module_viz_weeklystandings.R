# readme ---------------------------------------------------------------------

# This script is a shiny module that goes with my Fantasy Football Data
# Retriever shiny app. This module creates a bump chart showing how the league
# standings changed throughout a single season.

# Load r packages
require(here)
require(shiny)
require(magrittr)
require(tidyverse)
require(ggbump)
require(gt)
require(reactable)

source(here("src", "functions_viz.R"))

# shiny module ---------------------------------------------------------------

standings_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        uiOutput(NS(id, "team_list")),
        uiOutput(NS(id, "season_list"))
      ),
      mainPanel(
        width = 9,
        plotOutput(NS(id, "standings"),
                   height = "550px",
                   click = NS(id, "plot_click")),
        tags$br(),
        gt_output(NS(id, "standings_table"))
      )
    )
  )
}

standings_server <- function(id, viz_colors, viz_highlight, data) {
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
      selectInput(NS(id, "season_choose"),
                  "Season",
                  choices = unique(data()$FantasySeason),
                  selected = max(data()$FantasySeason),
                  width = 150L)
    })
    
    point_clicked <- reactiveVal()
    
    bump_df <- reactive({
      req(input$season_choose)
      
      point_clicked(NULL)
      
      data() %>%
        filter(FantasySeason == input$season_choose &
               WeekType == "Regular") %>%
        arrange(TeamId, FantasyWeek)
    })
    
    output$standings <- renderPlot({
      req(input$season_choose)
      
      bump_data <- bump_df()
      bump_colors <-
        bump_data %>%
        filter(FantasyWeek == 1) %>%
        mutate(line_color =
                 if_else(OwnerName == input$team_choose,
                        viz_colors$col1(), viz_colors$col2(),
                        missing = viz_colors$col2())) %$%
        set_names(line_color, OwnerName) %>%
        c(viz_colors$col2())
      last_week <- max(bump_data$FantasyWeek)
      left_labels <- filter(bump_data, FantasyWeek == 1L)
      right_labels <- filter(bump_data, FantasyWeek == last_week)
      playoff_line <-
        ifelse(length(unique(bump_data$TeamId)) > max(bump_data$PlayoffTeams),
               max(bump_data$PlayoffTeams) + 0.5, NA)
      
      bump_chart <-
        bump_data %>%
        ggplot(aes(x = FantasyWeek, y = StandingsRank, color = OwnerName)) +
        geom_bump(linewidth = 1) +
        geom_point(size = 4) +
        geom_segment(data = data.frame(a = "a"),
                     aes(color = "black", x = 0, xend = last_week + 1,
                         y = playoff_line, yend = playoff_line)) +
        geom_text(data = left_labels,
                  aes(x = FantasyWeek - 0.2, label = OwnerName),
                  size = 4, hjust = 1) +
        geom_text(data = right_labels,
                  aes(x = FantasyWeek + 0.2, label = OwnerName),
                  size = 4, hjust = 0) +
        geom_text(data = data.frame(a = "a"),
                  aes(x = last_week + 1, y = playoff_line,
                      label = "playoff cut line"),
                  vjust = 0.5, hjust = 0, color = "black") +
        scale_color_manual(values = bump_colors) +
        coord_cartesian(xlim = c(-1, last_week + 2)) +
        scale_x_continuous(breaks = seq(1L, last_week, 1L)) +
        scale_y_reverse() +
        labs(title = "League Standings Week-by-Week",
             subtitle = "click a circle on the plot to display more information",
             x = NULL,
             y = NULL) +
        theme(panel.background = element_blank(),
              panel.grid = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text.y = element_blank(),
              plot.title = element_text(hjust = 0.5,
                                        face = "bold",
                                        size = 16),
              plot.subtitle = element_text(hjust = 0.5,
                                           size = 12),
              legend.position = "none")
      
      bump_chart
    })
    
    observe({
      req(input$plot_click)
      
      clicked_week <-
        nearPoints(bump_df(), input$plot_click) %$%
        suppressWarnings(min(FantasyWeek))
      
      if(clicked_week >= 1 &
         clicked_week <= max(bump_df()$FantasyWeek)) {
        point_clicked(clicked_week)
      }
    })
    
    output$standings_table <- render_gt({
      req(point_clicked())
      
      table_df <-
        bump_df() %>%
        filter(FantasyWeek == point_clicked()) %>%
        mutate(StandingsRank = as.integer(StandingsRank),
               bar_color =
                 ifelse(OwnerName == input$team_choose,
                        viz_colors$col1(), viz_colors$col2()),
               wins_plot = NA,
               points_plot = NA) %>%
        select(OwnerName, TeamName, StandingsRank, SeasonWins, wins_plot,
               SeasonPointsFor, points_plot, bar_color) %>%
        arrange(StandingsRank)
      
      plot_wins <-
        uwf_table_bar(table_df, "SeasonWins", "OwnerName", "bar_color")
      plot_points <-
        uwf_table_bar(table_df, "SeasonPointsFor", "OwnerName",
                      "bar_color")
      
      table_df %>%
        gt() %>%
        tab_header(title = md(paste("League Standings at the end of week",
                                    point_clicked()))) %>%
        tab_style(style = cell_text(v_align = "top"), locations = cells_body()) %>%
        cols_label(OwnerName = "Owner", TeamName = "Team",
                   StandingsRank = "Place", wins_plot = "Wins", SeasonWins = "",
                   points_plot = "Points", SeasonPointsFor = "") %>%
        cols_align(align = "center", columns = StandingsRank) %>%
        cols_align(align = "left",
                   columns = c(OwnerName, TeamName, wins_plot, points_plot)) %>%
        cols_width(StandingsRank ~ px(75),
                   SeasonWins ~ px(50),
                   SeasonPointsFor ~ px(75)) %>%
        fmt(columns = StandingsRank, fns = uwf_format_ordinal) %>%
        text_transform(
          locations = cells_body(columns = wins_plot),
          fn = function (x) {map(plot_wins, ggplot_image, height = 25, aspect_ratio = 5)}
        ) %>%
        text_transform(
          locations = cells_body(columns = points_plot),
          fn = function (x) {map(plot_points, ggplot_image, height = 25, aspect_ratio = 7)}
        ) %>%
        cols_hide(bar_color)
      
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
#       standings_ui("viz")
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
#     standings_server("viz",
#                      color_list,
#                      viz_highlight = reactive({NA}),
#                      teams_week)
#   }
#   
#   shinyApp(ui, server)
# }
# 
# test_app()
