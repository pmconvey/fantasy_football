# readme ---------------------------------------------------------------------

# This script is a shiny module that goes with my Fantasy Football Data
# Retriever shiny app. This module shows how the final standings would be if
# two teams switched schedules.

# Load r packages
require(here)
require(shiny)
require(magrittr)
require(tidyverse)
require(gt)
require(reactable)
require(htmltools)

source(here("src", "functions_viz.R"))

# shiny module ---------------------------------------------------------------

switch_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        uiOutput(NS(id, "season_list")),
        uiOutput(NS(id, "team_list1")),
        uiOutput(NS(id, "team_list2"))
      ),
      mainPanel(
        width = 9,
        h2("Schedule Switcher"),
        p("The table below shows the effect two teams",
          "switching schedules would have on the league standings"),
        reactableOutput(NS(id, "switched_standings"))
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
      data_week()[data_week()$FantasySeason == input$season_choose
                  & data_week()$WeekType == 'Regular', ]
    })
    
    selected_season <- reactive({
      data_season() %>%
        filter(FantasySeason == input$season_choose) %>%
        mutate(TeamId = as.character(TeamId))
    })
    
    team_list <- reactive({
      teams <- unique(selected_week()$TeamId)
      names(teams) <- unique(selected_week()$OwnerName)
      
      return(teams[sort(names(teams))])
    })
    
    output$team_list1 <- renderUI({
      req(input$season_choose)
      
      selectInput(NS(id, "team1"),
                  "First Team to Switch",
                  choices = c("Select team" = "",
                              team_list()),
                  width = 150L)
    })
    
    output$team_list2 <- renderUI({
      req(input$season_choose)
      
      selectInput(NS(id, "team2"),
                  "Second Team to Switch",
                  choices = c("Select team" = "",
                              team_list()),
                  width = 150L)
    })
    
    switched_data <- reactive({
      req(input$team1, input$team2)
      
      og <-
        selected_week() %>%
        select(FantasyWeek, OwnerName, TeamId, PointsFor)
      
      all_schedules <-
        og %>%
        rename(OppOwnerName = OwnerName,
               OpponentId = TeamId,
               PointsAgainst = PointsFor) %>%
        inner_join(y = og,
                   by = "FantasyWeek",
                   relationship = "many-to-many") %>%
        mutate(WinTieLoss = case_when(PointsFor > PointsAgainst ~ 'Win',
                                      PointsFor == PointsAgainst ~ 'Tie',
                                      PointsFor < PointsAgainst ~ 'Loss'),
               TeamId = as.character(TeamId),
               OpponentId = as.character(OpponentId)) %>%
        arrange(FantasyWeek, TeamId, OpponentId) %>%
        select(FantasyWeek, TeamId, OwnerName, PointsFor, OpponentId,
               OppOwnerName, PointsAgainst, WinTieLoss)

      alt <-
        selected_week() %>%
        mutate(altTeamId =
                 case_when(TeamId == input$team1 & OpponentId == input$team2
                           ~ as.integer(input$team1),
                           TeamId == input$team2 & OpponentId == input$team1
                           ~ as.integer(input$team2),
                           TeamId == input$team1 ~ as.integer(input$team2),
                           TeamId == input$team2 ~ as.integer(input$team1),
                           TRUE ~ as.integer(TeamId)),
               altOpponentId =
                 case_when(TeamId %in% c(input$team1, input$team2)
                           ~ as.integer(OpponentId),
                           OpponentId == input$team1 ~ as.integer(input$team2),
                           OpponentId == input$team2 ~ as.integer(input$team1),
                           TRUE ~ as.integer(OpponentId))) %>%
        select(FantasyWeek, altTeamId, altOpponentId) %>%
        rename(TeamId = altTeamId,
               OpponentId = altOpponentId) %>%
        mutate(TeamId = as.character(TeamId),
               OpponentId = as.character(OpponentId))
      
      switched <-
        inner_join(x = all_schedules,
                   y = alt,
                   by = c("FantasyWeek", "TeamId", "OpponentId"))

      return(switched)
    })
    
    detailed_data <- reactive({
      alt <-
        switched_data() %>%
        select(FantasyWeek, TeamId, PointsAgainst, WinTieLoss,
               OppOwnerName) %>%
        mutate(TeamId = as.character(TeamId)) %>%
        rename(altPointsAgainst = PointsAgainst,
               altWinTieLoss = WinTieLoss,
               altOppOwnerName = OppOwnerName)

      og <-
        selected_week() %>%
        mutate(TeamId = as.character(TeamId)) %>%
        select(FantasyWeek, OwnerName,
               TeamId, PointsFor, PointsAgainst, WinTieLoss, OppOwnerName)

      combined <-
        inner_join(x = og, y = alt,
                   by = c("FantasyWeek", "TeamId")) %>%
        select(TeamId, FantasyWeek, altOppOwnerName, OppOwnerName, PointsFor,
               altPointsAgainst, PointsAgainst, altWinTieLoss, WinTieLoss)
    })
    
    standings <- reactive({
      df <-
        switched_data() %>%
        group_by(TeamId) %>%
        summarise(alt_Wins = sum(case_when(WinTieLoss == 'Win' ~ 1,
                                           WinTieLoss == 'Tie' ~ 0.5,
                                           WinTieLoss == 'Loss' ~ 0)),
                  alt_Points = sum(PointsFor),
                  .groups = 'drop') %>%
        mutate(alt_Standings =
                 data.table::frank(list(-alt_Wins, -alt_Points),
                                   ties.method = "first"),
               TeamId = as.character(TeamId)) %>%
        inner_join(y = selected_season(),
                   by = c("TeamId")) %>%
        mutate(StandingsChange = StandingsRank - alt_Standings,
               WinsChange = alt_Wins - SeasonWins) %>%
        select(alt_Standings, OwnerName, TeamName, TeamId, StandingsChange,
               alt_Wins, WinsChange, SeasonPointsFor) %>%
        arrange(alt_Standings)
      
      return(df)
    })
    
    output$switched_standings <- renderReactable({
      req(input$team1, input$team2)
      
      reactable(standings(),
                details = function(index) {
                  weeks <-
                    filter(detailed_data(), TeamId == standings()$TeamId[index]) %>%
                    select(-TeamId) %>%
                    arrange(FantasyWeek)
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
                },
                onClick = "expand",
                rowStyle = list(cursor = "pointer"),
                columns = list(
                  alt_Standings = colDef(name = "Standings",
                                         width = 100,
                                         cell = function(value, index) {
                                           old <- standings()$StandingsChange[index]
                                           font_color <- case_when(old > 0 ~ viz_colors$col4(),
                                                                   old == 0 ~ viz_colors$col5(),
                                                                   old < 0 ~ viz_colors$col6())
                                           div(
                                             div(style = "font-weight: 600", uwf_format_ordinal(value)),
                                             div(style = str_c("font-size: 1.1rem; color:", font_color),
                                                 str_c(ifelse(old >= 0, '+ ', ' '),
                                                       old, ' places'))
                                           )
                                         }),
                  TeamId = colDef(show = FALSE),
                  TeamName = colDef(show = FALSE),
                  OwnerName = colDef(name = "Owner",
                                     width = 300,
                                     align = 'left',
                                     cell = function(value, index) {
                                       name <- standings()$TeamName[index]
                                         div(
                                           div(value),
                                           div(style = "font-size: 1.1rem", name)
                                         )
                                       }),
                  StandingsChange = colDef(show = FALSE),
                  alt_Wins = colDef(name = "Wins",
                                    width = 200,
                                    cell = function(value, index) {
                                      old <- standings()$WinsChange[index]
                                      font_color <- case_when(old > 0 ~ viz_colors$col4(),
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
                  
    })
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
