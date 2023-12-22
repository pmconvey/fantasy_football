# readme ---------------------------------------------------------------------

# This is a shiny module that goes with my Fantasy Football shiny app.
# This module retrieves data for users who have a Sleeper  fantasy football
# league. The data is retrieved using Sleeper's API.

# Load r packages
require(here)
require(shiny)
require(magrittr)
require(tidyverse)
require(httr)

# sleeper api functions ------------------------------------------------------

source(here("src", "functions_uwf.R"))

# These functions are used to get data from the Sleeper API. Pulling them
# up here into functions so they are easier to modify if the API changes.

get_sleeper_user <- function(username) {
  url <- paste0("https://api.sleeper.app/v1/user/", username)
  
  user_id <-
    url %>%
    GET() %>%
    content() %>%
    pluck('user_id')
  
  return(user_id)
}

get_sleeper_leagues <- function(user_id, year) {
  url <- paste0("https://api.sleeper.app/v1/user/", user_id,
                "/leagues/nfl/", year)
  
  leagues_data <-
    url %>%
    GET() %>%
    content()
  
  return(leagues_data)
}

get_sleeper_settings <- function(league_id) {
  url <- paste0("https://api.sleeper.app/v1/league/", league_id)
  
  league_settings <-
    url %>%
    GET() %>%
    content()
  
  return(league_settings)
}

get_sleeper_rosters <- function(league_id) {
  url <- paste0("https://api.sleeper.app/v1/league/", league_id, "/rosters")

  rosters_df <-
    url %>%
    GET() %>%
    content() %>%
    map_dfr(~data.frame(LeagueId = .x[["league_id"]],
                        OwnerId = .x[["owner_id"]],
                        TeamId = .x[["roster_id"]]
                        )
            )
  
  return(rosters_df)
}

get_sleeper_users <- function(league_id) {
  url <- paste0("https://api.sleeper.app/v1/league/", league_id, "/users")
  
  teams_df <-
    url %>%
    GET() %>%
    content() %>%
    map_dfr(~data.frame(LeagueId = .x[["league_id"]],
                        OwnerId = .x[["user_id"]],
                        OwnerName = .x[["display_name"]],
                        TeamName =
                          uwf_if_null(.x[["metadata"]][["team_name"]],
                                      paste0("Team ", .x[["display_name"]]))
                        )
            )
  
  return(teams_df)
}

get_sleeper_drafts <- function(draft_id) {
  url <- paste0("https://api.sleeper.app/v1/draft/", draft_id, "/picks")
  
  drafts_df <-
    url %>%
    GET() %>%
    content() %>%
    map_dfr(~data.frame(DraftId = .x[["draft_id"]],
                        PickNumber = .x[["pick_no"]],
                        TeamId = .x[["roster_id"]],
                        PlayerId = .x[["player_id"]],
                        Bid = uwf_if_null(.x[["metadata"]][["amount"]], 0L),
                        Keeper = uwf_if_null(.x[["is_keeper"]], FALSE)
                        )
            )
  
  return(drafts_df)
}

get_sleeper_matchups_and_rosters <- function(league_id, week) {
  url <- paste0("https://api.sleeper.app/v1/league/", league_id,
                "/matchups/", week)
  
  url_data  <-
    GET(url) %>%
    content()
  
  starters <-
    url_data %>%
    map(~.x[["starters"]]) %>%
    unlist()
  
  matchups_df <-
    url_data %>%
    map_dfr(~data.frame(url_info = url,
                        TeamId = .x[["roster_id"]],
                        MatchupId = uwf_if_null(.x[["matchup_id"]], NA),
                        PointsFor = .x[["points"]],
                        CustomPoints = uwf_if_null(.x[["custom_points"]], NA),
                        PlayerId = names(unlist(.x[["players_points"]])),
                        PlayerPoints = unlist(.x[["players_points"]]))) %>%
    mutate(Starter = PlayerId %in% starters,
           LeagueId = league_id,
           nflWeek = as.integer(week),
           PointsFor = ifelse(!is.na(CustomPoints),
                              CustomPoints, PointsFor)) %>%
    select(LeagueId, nflWeek, MatchupId, TeamId, PointsFor, PlayerId,
           PlayerPoints, Starter)
  
  return(matchups_df)
}

# shiny module ---------------------------------------------------------------

# This UI module displays the inputs needed to allow a user to specify
# which Sleeper league to get data from.
sleeper_ui <- function(id) {
  tagList(
    tags$head(tags$style(HTML(".shiny-output-error-validation {
                                color: #2A2A2A;
                                background-color: #FF000070;
                                }
                                "))),
    numericInput(NS(id, "ff_year"), "Year",
                 value = uwf_current_year(),
                 min = 2010,
                 max = uwf_current_year(),
                 step = 1),
    tags$p("Year controls what leagues are visible to you and the latest year",
           "for which data is retrieved. The most common reason to change the",
           "year is if you want to get data for a league that is no longer",
           "active."),
    textInput(NS(id, "sleeper_user"), "Enter your Sleeper username"),
    uiOutput(NS(id, "sleeper_league_list")),
    conditionalPanel(
      condition = 'input.sleeper_league != null && input.sleeper_league != ""',
      actionButton(NS(id, "get_data"), "Get Data"),
      ns = NS(id)
    ),
    tags$h2(textOutput(NS(id, "success")), style = "color:green")
  )
}

# This server module retrieves data from Sleeper's API and stores it in a list
# where each element is a a reactive data frame that can then be used by
# other shiny modules.
sleeper_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    server_env <- environment()
    
    # Getting the user's id so the app can then get data about their
    # fantasy football leagues
    sleeper_userid <- reactive({
      req(input$sleeper_user)
      
      user_id <- get_sleeper_user(input$sleeper_user)
      
      validate(need(!is.null(user_id),
                    str_c(" \n  ", input$sleeper_user,
                          " is not a valid Sleeper username\n ")))
      
      return(user_id)
      })
    
    sleeper_leagues <- reactive({
      validate(need(input$ff_year >= 2010 &
                    input$ff_year <= uwf_current_year(),
                    str_c(" \n  Year must be between 2010 and ",
                          uwf_current_year(), "\n ")))
      
      get_sleeper_leagues(sleeper_userid(), input$ff_year)
    })
    
    sleeper_league_names <- reactive({
      map_chr(sleeper_leagues(), magrittr::extract("league_id")) %>%
        set_names(map_chr(sleeper_leagues(), magrittr::extract("name")))
    })
    
    # Displays all of the user's Sleeper fantasy football leagues and lets
    # them select which league the app will get data from
    output$sleeper_league_list <- renderUI({
      selectInput(NS(id, "sleeper_league"),
                  "Select a league",
                  choices = sleeper_league_names())
    })
    outputOptions(output, "sleeper_league_list", suspendWhenHidden = FALSE)
    
    league_name <- reactive({
      names(sleeper_league_names()[sleeper_league_names() ==
                                     input$sleeper_league])
    })
    
    # For reasons I don't understand, Sleeper leagues have a new ID for
    # each season so looping through the selected league to get the IDs
    # for previous seasons for that league
    sleeper_league <- reactive({
      req(input$sleeper_league)
      
      assign(x = "progress",
             value = shiny::Progress$new(),
             envir = server_env)
      
      progress$set(message = league_name(),
                   detail = "retrieving settings...",
                   value = 0.01)
      
      league_settings <- vector(mode = "list", length = 100L)
      i <- 1L
      
      add_year <-
        sleeper_leagues()[[which(sleeper_league_names() ==
                                   input$sleeper_league)]]
      
      sleeper_previous_year_id <-
        uwf_if_null(add_year[["previous_league_id"]], 0)
      
      league_settings[i] <- list(add_year)
      
      while(sleeper_previous_year_id != 0 &
            !is.null(sleeper_previous_year_id)) {
        i <- i + 1L
        
        add_year <- get_sleeper_settings(sleeper_previous_year_id)
        
        sleeper_previous_year_id <-
          uwf_if_null(add_year[["previous_league_id"]], 0)
        
        if (add_year[["settings"]][["leg"]] > 1L) {
          league_settings[i] <- list(add_year)
        }
      }
      rm(i, add_year, sleeper_previous_year_id)
      
      league_settings[vapply(league_settings, is.null, logical(1L))] <- NULL
      
      return(league_settings)
    }) %>%
      bindEvent(input$get_data)
    
    # Pulling out the settings for the league that are needed for the data
    # visualizations that the app will display
    sleeper_settings <- reactive({
      req(sleeper_league())
      
      progress$set(message = league_name(),
                   detail = "retrieving settings...",
                   value = 0.05)
      
      map_dfr(sleeper_league(),
              ~data.frame(League = 'Sleeper',
                          LeagueId = .x[["league_id"]],
                          FantasySeason = .x[["season"]],
                          LeagueName = .x[["name"]],
                          NumberOfTeams = .x[["total_rosters"]],
                          PlayoffTeams = .x[["settings"]][["playoff_teams"]]
                          )
              )
    })
    
    sleeper_settings_roster <- reactive({
      req(sleeper_settings())
      
      progress$set(message = league_name(),
                   detail = "retrieving settings...",
                   value = 0.06)
      
      map_dfr(sleeper_league(),
              ~data.frame(League = 'Sleeper',
                          LeagueId = .x[["league_id"]],
                          FantasySeason = .x[["season"]],
                          Slot = as.character(.x[["roster_positions"]])
                          )
              ) %>%
        group_by(League, LeagueId, FantasySeason, Slot) %>%
        summarise(NumberOfSlots = n(),
                  .groups = "drop")
    })
    
    sleeper_settings_scoring <- reactive({
      req(sleeper_settings_roster())
      
      progress$set(message = league_name(),
                   detail = "retrieving settings...",
                   value = 0.07)
      
      map_dfr(sleeper_league(),
              ~data.frame(League = 'Sleeper',
                          LeagueId = .x[["league_id"]],
                          FantasySeason = .x[["season"]],
                          Stat = names(.x[["scoring_settings"]]),
                          FantasyPoints = as.double(.x[["scoring_settings"]])
                          )
              )
    })
    
    sleeper_settings_schedule <- reactive({
      req(sleeper_settings_scoring())
      
      progress$set(message = league_name(),
                   detail = "retrieving settings...",
                   value = 0.08)
      
      map_dfr(sleeper_league(),
              ~data.frame(League = 'Sleeper',
                          LeagueId = .x[["league_id"]],
                          FantasySeason = .x[["season"]],
                          FantasyWeek = seq.int(from = .x[["settings"]][["start_week"]], 
                                                to = .x[["settings"]][["last_scored_leg"]],
                                                by = 1),
                          PlayoffStart = .x[["settings"]][["playoff_week_start"]]
                          )
              ) %>%
        mutate(nflWeek = FantasyWeek,
               WeekType = ifelse(FantasyWeek < PlayoffStart,
                                 "Regular",
                                 "Playoffs")) %>%
        select(-PlayoffStart)
    })
    
    # Pulling data from the league -- the teams, drafts, schedules, and
    # rosters from each week
    sleeper_teams <- reactive({
      req(sleeper_settings_schedule())
      
      progress$set(message = league_name(),
                   detail = "retrieving users...",
                   value = 0.1)
      
      league_list <- sleeper_settings()
      
      rosters_df <-
        league_list %>%
        pull(LeagueId) %>%
        map_dfr(get_sleeper_rosters)
      
      teams_df <-
        league_list %>%
        pull(LeagueId) %>%
        map_dfr(get_sleeper_users) %>%
        left_join(rosters_df, by = c("LeagueId", "OwnerId")) %>%
        left_join(league_list, by = "LeagueId") %>%
        select(League, LeagueId, FantasySeason, OwnerId, OwnerName, TeamId,
               TeamName) %>%
        filter(!is.na(TeamId))
      
      return(teams_df)
    })
    
    sleeper_drafts <- reactive({
      req(sleeper_teams())
      progress$set(message = league_name(),
                   detail = "retrieving drafts...",
                   value = 0.15)
      
      league_list <-
        map_dfr(sleeper_league(),
                ~data.frame(League = 'Sleeper',
                            LeagueId = .x[["league_id"]],
                            FantasySeason = .x[["season"]],
                            DraftId = .x[["draft_id"]]))
      
      drafts_df <-
        league_list %>%
        pull(DraftId) %>%
        map_dfr(get_sleeper_drafts) %>%
        left_join(y = league_list, by = "DraftId") %>%
        select(League, LeagueId, FantasySeason, PickNumber, TeamId,
               PlayerId, Bid, Keeper)
      
      return(drafts_df)
      })
    
    sleeper_matchups_and_rosters <- reactive({
      req(sleeper_drafts())
      
      progress$set(message = league_name(),
                   detail = "retrieving schedules...",
                   value = 0.2)
      
      sleeper_settings_schedule() %$%
        map2_dfr(LeagueId, nflWeek,
                 .f = ~{
                   progress$set(message = league_name(),
                                detail = "retrieving rosters...",
                                value = progress$getValue() +
                                  (0.5 / nrow(sleeper_settings_schedule())))
                   get_sleeper_matchups_and_rosters(.x, .y)
                   })
    })
    
    sleeper_matchups <- reactive({
      sleeper_matchups_and_rosters() %>%
        filter(!is.na(MatchupId)) %>%
        distinct(LeagueId, nflWeek, MatchupId, TeamId, PointsFor) %>%
        full_join(x = ., y = .,
                  by = c("LeagueId", "nflWeek", "MatchupId"),
                  relationship = "many-to-many") %>%
        filter(TeamId.x != TeamId.y) %>%
        rename(TeamId = TeamId.x,
               OpponentId = TeamId.y,
               PointsFor = PointsFor.x,
               PointsAgainst = PointsFor.y) %>%
        left_join(y = sleeper_settings_schedule(),
                  by = c("LeagueId", "nflWeek")) %>%
        mutate(WinTieLoss = case_when(PointsFor > PointsAgainst ~ "Win",
                                      PointsFor == PointsAgainst ~ "Tie",
                                      PointsFor < PointsAgainst ~ "Loss",
                                      TRUE ~ "Unknown")) %>%
        select(League, LeagueId, FantasySeason, FantasyWeek, WeekType,
               TeamId, OpponentId, PointsFor, PointsAgainst, WinTieLoss)
    })
    
    sleeper_rosters <- reactive({
      req(sleeper_matchups())
      
      progress$set(message = league_name(),
                   detail = "retrieving rosters...",
                   value = 0.75)
      on.exit(progress$close())
      
      sleeper_matchups_and_rosters() %>%
        left_join(y = sleeper_settings(), by = "LeagueId") %>%
        select(League, LeagueId, FantasySeason, nflWeek, TeamId, PlayerId,
               Starter)
    })
    
    # Displaying message that data was successfully retrieved
    output$success <- renderText({
      req(sleeper_rosters())
      
      paste(league_name(), "data successfully retrieved")
    })

    # Create list that will hold all of the Sleeper data so that it can be used
    # by other shiny modules
    list(settings_league = sleeper_settings,
         settings_schedule = sleeper_settings_schedule,
         settings_rosters = sleeper_settings_roster,
         settings_scoring = sleeper_settings_scoring,
         teams = sleeper_teams,
         drafts = sleeper_drafts,
         schedules = sleeper_matchups,
         rosters = sleeper_rosters)
  })
}

# test the module ------------------------------------------------------------

# uncomment the code below to test the module in isolation from the rest of
# the app

# test_app <- function() {
#   ui <- fluidPage(
#     sleeper_ui("sleeper"),
#     verbatimTextOutput("test")
#   )
# 
#   server <- function(input, output, session) {
#     test_data <- sleeper_server("sleeper")
#     shiny_data <- reactive({
#       # write_csv(test_data$settings_league(),
#       #           here("sleeper_settings_league.csv"), na = "")
#       # write_csv(test_data$settings_schedule(),
#       #           here("sleeper_settings_schedule.csv"), na = "")
#       # write_csv(test_data$settings_rosters(),
#       #           here("sleeper_settings_rosters.csv"), na = "")
#       # write_csv(test_data$settings_scoring(),
#       #           here("sleeper_settings_scoring.csv"), na = "")
#       # write_csv(test_data$teams(),
#       #           here("sleeper_teams.csv"), na = "")
#       # write_csv(test_data$drafts(),
#       #           here("sleeper_drafts.csv"), na = "")
#       # write_csv(test_data$schedules(),
#       #           here("sleeper_schedules.csv"), na = "")
#       # write_csv(test_data$rosters(),
#       #           here("sleeper_rosters.csv"), na = "")
#       test_data$rosters()
#     })
#     output$test <- renderPrint({
#      head(shiny_data(), 10)
#     })
#   }
# 
#   shinyApp(ui, server)
# }
# 
# test_app()
