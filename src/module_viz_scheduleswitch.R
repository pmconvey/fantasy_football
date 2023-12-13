# readme ---------------------------------------------------------------------

# This script is a shiny module that goes with my Fantasy Football Data
# Retriever shiny app. This module shows how the final standings would be if
# teams switched schedules.

# Load r packages
require(here)
require(shiny)
require(magrittr)
require(tidyverse)
require(reactable)
require(htmltools)
require(ggbeeswarm)
require(plotly)

source(here("src", "functions_viz.R"))

uwf_switch_schedules <- function(original_data, possible_data, id1, id2) {
  alt <-
    original_data %>%
    mutate(altTeamId =
             case_when(TeamId == id1 & OpponentId == id2 ~ id1,
                       TeamId == id2 & OpponentId == id1 ~ id2,
                       TeamId == id1 ~ id2,
                       TeamId == id2 ~ id1,
                       TRUE ~ TeamId),
           altOpponentId =
             case_when(TeamId %in% c(id1, id2) ~ OpponentId,
                       OpponentId == id1 ~ id2,
                       OpponentId == id2 ~ id1,
                       TRUE ~ OpponentId)) %>%
    select(FantasyWeek, altTeamId, altOpponentId) %>%
    rename(TeamId = altTeamId,
           OpponentId = altOpponentId)
  
  switched <-
    inner_join(x = possible_data,
               y = alt,
               by = c("FantasyWeek", "TeamId", "OpponentId")) %>%
    mutate(idcol = paste(id1, id2, sep = "-"))
  
  return(switched)
}

uwf_standings <- function(data) {
  df <-
    data %>%
    group_by(idcol, TeamId, OwnerName, TeamName) %>%
    summarise(alt_Wins = sum(case_when(WinTieLoss == 'Win' ~ 1,
                                       WinTieLoss == 'Tie' ~ 0.5,
                                       WinTieLoss == 'Loss' ~ 0)),
              alt_Points = sum(PointsFor),
              .groups = 'drop') %>%
    group_by(idcol) %>%
    mutate(alt_Standings =
             data.table::frank(list(-alt_Wins, -alt_Points),
                               ties.method = "first")) %>%
    ungroup()
  
  return(df)
}

uwf_table_standings <- function(standings_data, weeks_data) {
  reactable(standings_data,
            details = function(index) {
              weeks <-
                filter(weeks_data,
                       TeamId == standings_data$TeamId[index]) %>%
                select(-TeamId) %>%
                arrange(FantasyWeek)
              uwf_table_weeks(weeks)
            },
            onClick = "expand",
            rowStyle = list(cursor = "pointer"),
            columns = list(
              idcol = colDef(show = FALSE),
              TeamId = colDef(show = FALSE),
              OwnerName = colDef(name = "Owner",
                                 width = 300,
                                 align = 'left',
                                 cell = function(value, index) {
                                   name <- standings_data$TeamName[index]
                                   div(
                                     div(value),
                                     div(style = "font-size: 1.1rem", name)
                                   )
                                 }),
              TeamName = colDef(show = FALSE),
              alt_Standings = colDef(name = "Standings",
                                     width = 100,
                                     cell = function(value, index) {
                                       old <-
                                         standings_data$StandingsChange[index]
                                       font_color <-
                                         case_when(old > 0 ~ viz_colors$col4(),
                                                   old == 0 ~ viz_colors$col5(),
                                                   old < 0 ~ viz_colors$col6())
                                       div(
                                         div(style = "font-weight: 600",
                                             uwf_format_ordinal(value)),
                                         div(style =
                                               str_c("font-size: 1.1rem; color:",
                                                     font_color),
                                             str_c(ifelse(old >= 0, '+ ', ' '),
                                                   old, ' places'))
                                       )
                                     }),
              StandingsChange = colDef(show = FALSE),
              alt_Wins = colDef(name = "Wins",
                                width = 200,
                                cell = function(value, index) {
                                  old <- standings_data$WinsChange[index]
                                  font_color <-
                                    case_when(old > 0 ~ viz_colors$col4(),
                                              old == 0 ~ viz_colors$col5(),
                                              old < 0 ~ viz_colors$col6())
                                  div(
                                    div(value),
                                    div(style = str_c("font-size: 1.1rem; color:", font_color),
                                        str_c(ifelse(old >= 0, '+ ', ' '),
                                              old, ' wins'))
                                  )
                                }),
              WinsChange = colDef(show = FALSE),
              SeasonPointsFor = colDef(name = "Points For",
                                       width = 200)
            ),
            defaultColDef = colDef(vAlign = 'center',
                                   align = 'center'),
            fullWidth = FALSE
  )
}

uwf_table_weeks <- function(data) {
  tbl <- reactable(weeks,
                   columns = list(
                     FantasyWeek = colDef(name = 'Week',
                                          align = 'center',
                                          width = 100),
                     PointsFor = colDef(name = 'Points For',
                                        align = 'right',
                                        width = 125),
                     altPointsAgainst = colDef(name = 'Points Against',
                                               width = 125,
                                               cell = function(value, index) {
                                                 if (weeks$OppOwnerName[index] !=
                                                     weeks$altOppOwnerName[index]) {
                                                   old <- weeks$PointsAgainst[index]
                                                   div(
                                                     div(value),
                                                     div(style = "font-size: 1.1rem; font-color: #3e3e3e", old)
                                                   )
                                                 } else {
                                                   div(value)
                                                 }
                                               }),
                     PointsAgainst = colDef(show = FALSE),
                     altWinTieLoss = colDef(name = 'Result',
                                            width = 125,
                                            cell = function(value, index) {
                                              if (weeks$OppOwnerName[index] !=
                                                  weeks$altOppOwnerName[index]) {
                                                old <- weeks$WinTieLoss[index]
                                                div(
                                                  div(value),
                                                  div(style = "font-size: 1.1rem; font-color: #3e3e3e", old)
                                                )
                                              } else {
                                                div(value)
                                              }
                                            }),
                     WinTieLoss = colDef(show = FALSE),
                     altOppOwnerName = colDef(name = 'Opponent',
                                              width = 200,
                                              cell = function(value, index) {
                                                if (weeks$OppOwnerName[index] !=
                                                    weeks$altOppOwnerName[index]) {
                                                  old <- weeks$OppOwnerName[index]
                                                  div(
                                                    div(value),
                                                    div(style = "font-size: 1.1rem; font-color: #3e3e3e", old)
                                                  )
                                                } else {
                                                  div(value)
                                                }
                                              }),
                     OppOwnerName = colDef(show = FALSE)
                   ),
                   rowStyle = function(index) {
                     if (weeks$OppOwnerName[index] ==
                         weeks$altOppOwnerName[index]) {
                       list(background = "rgba(0, 0, 0, 0.00)")
                     } else if (weeks$WinTieLoss[index] ==
                                weeks$altWinTieLoss[index]) {
                       list(background = "rgba(0, 0, 0, 0.05)")
                     } else if (weeks$altWinTieLoss[index] == 'Win') {
                       list(borderRight = str_c('4px solid ', viz_colors$col4()),
                            background = "rgba(0, 0, 0, 0.05)")
                     } else if (weeks$altWinTieLoss[index] == 'Loss') {
                       list(borderRight = str_c('4px solid ', viz_colors$col6()),
                            background = "rgba(0, 0, 0, 0.05)")
                     } else if (weeks$altWinTieLoss[index] == 'Tie') {
                       list(borderRight = str_c('4px solid ', viz_colors$col5()),
                            background = "rgba(0, 0, 0, 0.05)")
                     }
                   },
                   outlined = TRUE,
                   highlight = TRUE,
                   fullWidth = FALSE,
                   defaultPageSize = 15,
                   defaultColDef = colDef(vAlign = 'top',
                                          align = 'left'))
  htmltools::div(style = list(margin = "12px 45px"), tbl)
}

# shiny module ---------------------------------------------------------------

switch_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        uiOutput(NS(id, "season_list"))
      ),
      mainPanel(
        width = 9,
        h2("Switched Schedules"),
        p("The plot below shows where each team in the league would have",
          "finished if they switched schedules with the other teams in the",
          "league. Each circle represents a scenario in which two teams",
          "switched schedules as well as the actual schedule. Hover over a",
          "circle to show more information."),
        plotlyOutput(NS(id, "standings_plot"),
                     height = "600px")
      )
    )
  )
}

switch_server <- function(id, viz_colors, data_week, data_season) {
  moduleServer(id, function(input, output, session) {

    output$season_list <- renderUI({
      selectInput(NS(id, "season_choose"),
                  "Season",
                  choices = sort(unique(data_season()$FantasySeason)),
                  selected = max(data_season()$FantasySeason),
                  width = 150L)
    })
    
    selected_week <- reactive({
      data_week() %>%
      filter(FantasySeason == input$season_choose
             & WeekType == 'Regular')
    })
    
    selected_season <- reactive({
      data_season() %>%
        filter(FantasySeason == input$season_choose)
    })
    
    team_list <- reactive({
      selected_season() %>%
        distinct(TeamId, OwnerName, TeamName)
    })
    
    all_schedules <- reactive({
      expand_grid(FantasyWeek = sort(unique(selected_week()$FantasyWeek)),
                  TeamId = team_list()$TeamId,
                  OpponentId = team_list()$TeamId) %>%
        filter(TeamId != OpponentId) %>%
        inner_join(y = selected_week()[, c("FantasyWeek", "TeamId",
                                           "PointsFor")],
                   by = c("FantasyWeek", "OpponentId" = "TeamId")) %>%
        rename(PointsAgainst = PointsFor) %>%
        inner_join(y = selected_week()[, c("FantasyWeek", "TeamId",
                                           "PointsFor")],
                   by = c("FantasyWeek", "TeamId")) %>%
        mutate(WinTieLoss = case_when(PointsFor > PointsAgainst ~ 'Win',
                                      PointsFor == PointsAgainst ~ 'Tie',
                                      PointsFor < PointsAgainst ~ 'Loss'))
    })
    
    all_switched_schedules <- reactive({
      team_combos <-
        expand_grid(Team1 = team_list()$TeamId,
                    Team2 = team_list()$TeamId) %>%
        filter(Team1 < Team2)
      
      switched_schedules <-
        map2_df(team_combos$Team1, team_combos$Team2,
                ~uwf_switch_schedules(selected_week(), all_schedules(),
                                      .x, .y)) %>%
        select(-PointsFor) %>%
        inner_join(y = team_list(), by = c("OpponentId" = "TeamId")) %>%
        rename(altOppOwnerName = OwnerName,
               altOppTeamName = TeamName,
               altOpponentId = OpponentId,
               altPointsAgainst = PointsAgainst,
               altWinTieLoss = WinTieLoss) %>%
        inner_join(y = selected_week(), by = c("FantasyWeek", "TeamId")) %>%
        select(idcol, FantasyWeek, TeamId, TeamName, OwnerName, PointsFor,
               PointsAgainst, altPointsAgainst, altWinTieLoss, WinTieLoss,
               altOppOwnerName, OppOwnerName) %>%
        arrange(idcol, FantasyWeek, TeamId)
      
      return(switched_schedules)
    })
    
    all_standings <- reactive({
      all_switched_schedules() %>%
        select(idcol, TeamId, TeamName, OwnerName, PointsFor,
               altWinTieLoss) %>%
        rename(WinTieLoss = altWinTieLoss) %>%
        uwf_standings() %>%
        inner_join(y = selected_season()[, c("TeamId", "StandingsRank",
                                             "SeasonWins", "SeasonPointsFor")],
                   by = "TeamId") %>%
        mutate(StandingsChange = StandingsRank - alt_Standings,
               WinsChange = alt_Wins - SeasonWins) %>%
        arrange(idcol, alt_Standings) %>%
        select(idcol, TeamId, OwnerName, TeamName, alt_Standings,
               StandingsChange, alt_Wins, WinsChange, SeasonPointsFor)
    })
    
    switched_viz <- reactive({
      original_standings <-
        selected_season() %>%
        select(TeamId, OwnerName, StandingsRank, SeasonWins, SeasonPointsFor)
      
      switched_viz <-
        all_standings() %>%
        mutate(Team1 = gsub("([0-9]+)-[0-9]+", "\\1", idcol),
               Team2 = gsub("[0-9]+-([0-9]+)", "\\1", idcol)) %>%
        filter(TeamId == Team1 | TeamId == Team2) %>%
        bind_rows(original_standings) %>%
        mutate(StandingsRank = as.integer(ifelse(is.na(idcol),
                                                 StandingsRank,
                                                 alt_Standings)),
               Wins = ifelse(is.na(idcol), SeasonWins, alt_Wins),
               SwitchedWith =
                 case_when(TeamId == Team1 ~
                             team_list()$OwnerName[match(Team2, team_list()$TeamId)],
                           TeamId == Team2 ~
                             team_list()$OwnerName[match(Team1, team_list()$TeamId)]),
               SwitchedText =
                 ifelse(is.na(idcol), 'Actual Standings',
                        paste('If', OwnerName, 'switched schedules with',
                              SwitchedWith)),
               Change = case_when(StandingsChange > 0 ~ 'pos',
                                  StandingsChange < 0 ~ 'neg',
                                  StandingsChange == 0 ~ 'neu',
                                  is.na(StandingsChange) ~ 'neu'),
               idcol = ifelse(is.na(idcol), "0-0", idcol)) %>%
        select(idcol, OwnerName, TeamId, StandingsRank, Wins, SwitchedText, Change)
    })
    
    output$standings_plot <- renderPlotly({
      req(input$season_choose)
      
      bee_plot <-
        ggplot(switched_viz(),
               aes(x = StandingsRank, y = OwnerName, color = Change,
                   text = paste(SwitchedText, '<br>', OwnerName,
                                "would have finished in",
                                uwf_format_ordinal(StandingsRank), "place"))) +
        geom_beeswarm() +
        scale_x_reverse(breaks = 1:100) +
        scale_color_manual(values = c(viz_colors$col6(),
                                      viz_colors$col5(),
                                      viz_colors$col4())) +
        labs(x = "Place",
             y = NULL) 
      ggplotly(bee_plot, tooltip = "text") %>%
        layout(showlegend = FALSE)
    })
    
    # output$switched_standings <- renderReactable({
    #   req(input$team1, input$team2)
    #   
    #   reactable(standings(),
    #             details = function(index) {
    #               weeks <-
    #                 filter(detailed_data(), TeamId == standings()$TeamId[index]) %>%
    #                 select(-TeamId) %>%
    #                 arrange(FantasyWeek)
    #               tbl <- reactable(weeks,
    #                                columns = list(
    #                                  FantasyWeek = colDef(name = 'Week',
    #                                                       align = 'center',
    #                                                       width = 100),
    #                                  PointsFor = colDef(name = 'Points For',
    #                                                     align = 'right',
    #                                                     width = 125),
    #                                  altPointsAgainst = colDef(name = 'Points Against',
    #                                                            width = 125,
    #                                                            cell = function(value, index) {
    #                                                              if (weeks$OppOwnerName[index] !=
    #                                                                  weeks$altOppOwnerName[index]) {
    #                                                              old <- weeks$PointsAgainst[index]
    #                                                              div(
    #                                                                div(value),
    #                                                                div(style = "font-size: 1.1rem; font-color: #3e3e3e", old)
    #                                                              )
    #                                                              } else {
    #                                                                div(value)
    #                                                              }
    #                                                            }),
    #                                  PointsAgainst = colDef(show = FALSE),
    #                                  altWinTieLoss = colDef(name = 'Result',
    #                                                         width = 125,
    #                                                         cell = function(value, index) {
    #                                                           if (weeks$OppOwnerName[index] !=
    #                                                               weeks$altOppOwnerName[index]) {
    #                                                             old <- weeks$WinTieLoss[index]
    #                                                             div(
    #                                                               div(value),
    #                                                               div(style = "font-size: 1.1rem; font-color: #3e3e3e", old)
    #                                                             )
    #                                                           } else {
    #                                                             div(value)
    #                                                           }
    #                                                         }),
    #                                  WinTieLoss = colDef(show = FALSE),
    #                                  altOppOwnerName = colDef(name = 'Opponent',
    #                                                           width = 200,
    #                                                           cell = function(value, index) {
    #                                                             if (weeks$OppOwnerName[index] !=
    #                                                                 weeks$altOppOwnerName[index]) {
    #                                                               old <- weeks$OppOwnerName[index]
    #                                                               div(
    #                                                                 div(value),
    #                                                                 div(style = "font-size: 1.1rem; font-color: #3e3e3e", old)
    #                                                               )
    #                                                             } else {
    #                                                               div(value)
    #                                                             }
    #                                                           }),
    #                                  OppOwnerName = colDef(show = FALSE)
    #                                ),
    #                                rowStyle = function(index) {
    #                                  if (weeks$OppOwnerName[index] ==
    #                                      weeks$altOppOwnerName[index]) {
    #                                    list(background = "rgba(0, 0, 0, 0.00)")
    #                                  } else if (weeks$WinTieLoss[index] ==
    #                                             weeks$altWinTieLoss[index]) {
    #                                    list(background = "rgba(0, 0, 0, 0.05)")
    #                                  } else if (weeks$altWinTieLoss[index] == 'Win') {
    #                                    list(borderRight = str_c('4px solid ', viz_colors$col4()),
    #                                         background = "rgba(0, 0, 0, 0.05)")
    #                                  } else if (weeks$altWinTieLoss[index] == 'Loss') {
    #                                    list(borderRight = str_c('4px solid ', viz_colors$col6()),
    #                                         background = "rgba(0, 0, 0, 0.05)")
    #                                  } else if (weeks$altWinTieLoss[index] == 'Tie') {
    #                                    list(borderRight = str_c('4px solid ', viz_colors$col5()),
    #                                         background = "rgba(0, 0, 0, 0.05)")
    #                                  }
    #                                },
    #                                outlined = TRUE,
    #                                highlight = TRUE,
    #                                fullWidth = FALSE,
    #                                defaultPageSize = 15,
    #                                defaultColDef = colDef(vAlign = 'top',
    #                                                       align = 'left'))
    #               htmltools::div(style = list(margin = "12px 45px"), tbl)
    #             },
    #             onClick = "expand",
    #             rowStyle = list(cursor = "pointer"),
    #             columns = list(
    #               alt_Standings = colDef(name = "Standings",
    #                                      width = 100,
    #                                      cell = function(value, index) {
    #                                        old <- standings()$StandingsChange[index]
    #                                        font_color <- case_when(old > 0 ~ viz_colors$col4(),
    #                                                                old == 0 ~ viz_colors$col5(),
    #                                                                old < 0 ~ viz_colors$col6())
    #                                        div(
    #                                          div(style = "font-weight: 600", uwf_format_ordinal(value)),
    #                                          div(style = str_c("font-size: 1.1rem; color:", font_color),
    #                                              str_c(ifelse(old >= 0, '+ ', ' '),
    #                                                    old, ' places'))
    #                                        )
    #                                      }),
    #               TeamId = colDef(show = FALSE),
    #               TeamName = colDef(show = FALSE),
    #               OwnerName = colDef(name = "Owner",
    #                                  width = 300,
    #                                  align = 'left',
    #                                  cell = function(value, index) {
    #                                    name <- standings()$TeamName[index]
    #                                      div(
    #                                        div(value),
    #                                        div(style = "font-size: 1.1rem", name)
    #                                      )
    #                                    }),
    #               StandingsChange = colDef(show = FALSE),
    #               alt_Wins = colDef(name = "Wins",
    #                                 width = 200,
    #                                 cell = function(value, index) {
    #                                   old <- standings()$WinsChange[index]
    #                                   font_color <- case_when(old > 0 ~ viz_colors$col4(),
    #                                                           old == 0 ~ viz_colors$col5(),
    #                                                           old < 0 ~ viz_colors$col6())
    #                                   div(
    #                                     div(value),
    #                                     div(style = str_c("font-size: 1.1rem; color:", font_color),
    #                                         str_c(ifelse(old >= 0, '+ ', ' '),
    #                                               old, ' wins'))
    #                                   )
    #                                 }),
    #               WinsChange = colDef(show = FALSE),
    #               SeasonPointsFor = colDef(name = "Points For",
    #                                        width = 200)
    #             ),
    #             defaultColDef = colDef(vAlign = 'center',
    #                                    align = 'center'),
    #             fullWidth = FALSE
    #   )
    #               
    # })
  })
}

# test the module ------------------------------------------------------------
# uncomment the code below to test the module in isolation from the rest of
# the app

# test_app <- function() {
#   ui <-
#     fluidPage(
#       switch_ui("viz")
#     )
# 
#   server <- function(input, output, session) {
#     teams_week <- reactive({read_csv(here("demo_data", "team_week.csv"),
#                            col_types = cols())})
# 
#     teams_season <- reactive({read_csv(here("demo_data", "team_season.csv"),
#                                        col_types = cols())})
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
#     switch_server("viz",
#                   color_list,
#                   teams_week,
#                   teams_season)
#   }
# 
#   shinyApp(ui, server)
# }
# 
# test_app()
