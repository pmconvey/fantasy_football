# readme ---------------------------------------------------------------------

# This is a shiny module that goes with my Fantasy Football shiny app.
# This module retrieves data for users who have an ESPN fantasy football
# league. The data is retrieved using ESPN's API.

# Load r packages
require(here)
require(shiny)
require(magrittr)
require(tidyverse)
require(httr)

# espn api functions ---------------------------------------------------------

source(here("src", "functions_uwf.R"))

# The three 'spelunk' functions are meant to be fast ways to extract a named
# element from a list, no matter how deeply it is nested in the list. They
# work similar to map(list, "element") except that the "element" can be many
# levels deep for the spelunk functions
spelunk <- function (x, element, names = FALSE, rm_null = FALSE) {
  if (element %in% names(x)) {
    vals <- x[element]
  } else {
    vals <- map(x, ~pluck(.x, element))
  }
  if (is_null(unlist(vals, recursive = FALSE)) & is_list(x)) {
    names(x) <- NULL
    x <- unlist(x, recursive = FALSE)
    vals <- spelunk(x, element, names = names)
  } else if (is.null(unlist(vals, recursive = FALSE))) {
    warning(element, " not found")
    vals <- NULL
  }
  return (
    if (!names & rm_null) {
      names(vals) <- NULL
      vals <- vals[!map_lgl(vals, is_null)]
      vals
    } else if (!names) {
      names(vals) <- NULL
      vals
    } else if (rm_null) {
      vals <- vals[!is_null(vals)]
      vals
    } else {vals}
  )
}

spelunk2 <- function (x, elements, names = FALSE, rm_na = FALSE) {
  for (i in elements) {
    x <- spelunk(x, i, names = names)
  }
  x <- uwf_if_null(x)
  x <- unlist(x, recursive = FALSE)
  if (rm_na) {
    x <- x[!is.na(x)]
  }
  return (x)
}

spelunk_df <- function (x, df_cols, names = FALSE, rm_na = FALSE) {
  map_df(df_cols, ~spelunk2(x, .x, names = names, rm_na = rm_na))
}

# The get_espn_* functions are meant to be simple ways to grab data from
# ESPN's fantasy football API. These rely on the spelunk functions above and
# will break if ESPN changes the API.
get_espn_settings <- function (league_id, year) {
  url <- ifelse(year > 2018L,
                paste0("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
                       year, "/segments/0/leagues/",
                       league_id, "?view=mSettings"),
                paste0("https://fantasy.espn.com/apis/v3/games/ffl/",
                       "leagueHistory/", league_id,
                       "?seasonId=", year,
                       "&view=mSettings"))
  
  league_settings <-
    url %>%
    GET() %>%
    content()
  
  if (year <= 2018L) {
    league_settings <- unlist(league_settings, recursive = FALSE)
  }
  
  return(league_settings)
}

get_espn_teams <- function (league_id, year) {
  url <- ifelse(year > 2018L,
                paste0("https://fantasy.espn.com/apis/v3/",
                       "games/ffl/seasons/", year,
                       "/segments/0/leagues/", league_id,
                       "?view=mTeam"),
                paste0("https://fantasy.espn.com/apis/v3/",
                       "games/ffl/leagueHistory/", league_id,
                       "?seasonId=", year,
                       "&view=mTeam"))
  
  GET(url) %>%
    content() %>% {
      right_join(x = tibble(spelunk_df(.,
                                       list(OwnerId = c("members", "id"),
                                            FirstName = c("members",
                                                          "firstName"),
                                            LastName = c("members",
                                                         "lastName")))),
                 y = tibble(spelunk_df(.,
                                       list(LeagueId = "id",
                                            FantasySeason = "seasonId")),
                            spelunk_df(.,
                                       list(OwnerId = c("teams",
                                                        "primaryOwner"),
                                            TeamId = c("teams", "id"),
                                            TeamLocation = c("teams",
                                                             "location"),
                                            TeamNickname = c("teams",
                                                             "nickname")))),
                 by = "OwnerId")
    }
}

get_espn_drafts <- function (league_id, year) {
  url <- ifelse(year > 2018L,
                paste0("https://fantasy.espn.com/apis/v3/games/ffl/",
                       "seasons/", year,
                       "/segments/0/leagues/", league_id,
                       "?view=mDraftDetail"),
                paste0("https://fantasy.espn.com/apis/v3/games/ffl/",
                       "leagueHistory/", league_id,
                       "?seasonId=", year,
                       "&view=mDraftDetail"))
  
  GET(url) %>%
    content() %>%
    spelunk_df(list(LeagueId = "id",
                    FantasySeason = "seasonId",
                    PickNumber = c("picks", "overallPickNumber"),
                    TeamId = c("picks", "teamId"),
                    PlayerId = c("picks", "playerId"),
                    Bid = c("picks", "bidAmount"),
                    Keeper = c("picks", "keeper")))
}

get_espn_schedules <- function (league_id, year) {
  url <- ifelse(year > 2018L,
                paste0("https://fantasy.espn.com/apis/v3/",
                       "games/ffl/seasons/", year,
                       "/segments/0/leagues/", league_id,
                       "?view=mMatchupScore"),
                paste0("https://fantasy.espn.com/apis/v3/",
                       "games/ffl/leagueHistory/", league_id,
                       "?seasonId=", year,
                       "&view=mMatchupScore"))
  
  GET(url) %>%
    content() %>%
    spelunk_df(list(LeagueId = "id",
                    FantasySeason = "seasonId",
                    FantasyWeek = c("schedule", "matchupPeriodId"),
                    HomeTeamId = c("schedule", "home", "teamId"),
                    HomeTeamPoints = c("schedule", "home", "totalPoints"),
                    AwayTeamId = c("schedule", "away", "teamId"),
                    AwayTeamPoints = c("schedule", "away", "totalPoints"),
                    Winner = c("schedule", "winner"),
                    PlayoffType = c("schedule", "playoffTierType")))
}

get_espn_rosters <- function (league_id, year, week) {
  if (year < 2019L) {
    stop("Year must be 2019 or later. ESPN's api does not work for ",
         "2018 and earlier")
  }
  
  url <- paste0("https://fantasy.espn.com/apis/v3/games/ffl/",
                "seasons/", year,
                "/segments/0/leagues/", league_id,
                "?view=mRoster",
                "&scoringPeriodId=", week)
  
  url_data <-
    GET(url) %>%
    content()
  
  rosters_df <-
    tibble(spelunk_df(url_data,
                      list(LeagueId = "id",
                           FantasySeason = "seasonId",
                           nflWeek = "scoringPeriodId")),
           map_df(spelunk2(url_data, "teams"),
                  ~spelunk_df(.x,
                              list(TeamId = "id",
                                   SlotId = c("entries", "lineupSlotId"),
                                   PlayerId = c("entries", "playerId"))))) %>%
    mutate(League = "ESPN",
           Starter = !SlotId %in% c("20", "21")) %>%
    select(League, LeagueId, FantasySeason, nflWeek, TeamId, PlayerId,
           Starter)
}

get_espn_players <- function (league_id, year, week, slot = NULL) {
  slot <- ifelse(is.null(slot), paste(0L:24L, collapse = ","), slot)
  
  url <- ifelse(year > 2018L,
                paste0("https://fantasy.espn.com/apis/v3/",
                       "games/ffl/seasons/", year,
                       "/segments/0/leagues/", league_id,
                       "?view=kona_player_info",
                       "&scoringPeriodId=", week),
                paste0("https://fantasy.espn.com/apis/v3/",
                       "games/ffl/leagueHistory/", league_id,
                       "?seasonId=", year,
                       "?view=kona_player_info",
                       "&scoringPeriodId=", week))
  
  url_header <- paste0('{"players":{"filterSlotIds":{"value":[', slot, ']},',
                       '"filterStatsForCurrentSeasonScoringPeriodId":',
                       '{"value":[', week, ']},"sortPercOwned":',
                       '{"sortPriority":3,"sortAsc":false},',
                       '"limit":5000,"offset":0,',
                       '"sortAppliedStatTotalForScoringPeriodId":',
                       '{"sortAsc":false,"sortPriority":1,"value":1},',
                       '"filterRanksForScoringPeriodIds":{"value":[1]},',
                       '"filterRanksForRankTypes":{"value":["STANDARD"]},',
                       '"filterRanksForSlotIds":{"value":[0,1,2,3,4,5,6,7,8,',
                       '9,10,11,12,13,14,15,16,17,18,19,23,24]}}}')
  
  url_data <-
    GET(url, add_headers('X-Fantasy-Filter' = url_header)) %>%
    content()
  
  players <-
    spelunk_df(url_data,
               list(PlayerId = c("players", "id"))) %>%
    mutate(Stats = map(spelunk2(url_data, "players"),
                       ~spelunk2(.x, "stats", rm_na = TRUE))) %>%
    select(PlayerId, Stats) %>%
    filter(map_lgl(Stats, is_list)) %>%
    unnest(Stats) %>%
    mutate(Season = spelunk2(Stats, "seasonId"),
           Week = spelunk2(Stats, "scoringPeriodId"),
           StatSource = spelunk2(Stats, "statSourceId"),
           SplitType = spelunk2(Stats, "statSplitTypeId"),
           StatType = case_when(Season == as.integer(year) &
                                  Week == as.integer(week) &
                                  StatSource == 0 &
                                  SplitType == 1 ~ "Actual",
                                Season == as.integer(year) &
                                  Week == as.integer(week) &
                                  StatSource == 1 &
                                  SplitType == 1 ~ "Projected")) %>%
    select(PlayerId, StatType, Stats)
  
  stats <-
    players %>%
    unnest(Stats) %>%
    mutate(stat_name = names(Stats)) %>%
    filter(stat_name == "stats") %>%
    select(-stat_name) %>%
    unnest(Stats) %>%
    mutate(StatId = names(Stats)) %>%
    rename(StatValue = Stats) %>%
    select(PlayerId, StatType, StatId, StatValue)
  
  
  rm(slot, url, url_header, url_data, players)
  
  return(stats)
}

# shiny module ---------------------------------------------------------------

# This UI module displays the inputs needed to allow a user to specify
# which ESPN league to get data from.
espn_ui <- function(id) {
  tagList(
    tags$head(tags$style(HTML(".shiny-output-error-validation {
                                color: #2A2A2A;
                                background-color: #FF000070;
                                }
                                "))),
    numericInput(NS(id, "ff_year"), "Year",
                 value = uwf_current_year(),
                 min = 2019,
                 max = uwf_current_year(),
                 step = 1),
    tags$p("Year controls the latest year that the app gets data for"),
    textInput(NS(id, "espn_id"), "Enter your ESPN league ID"),
    uiOutput(NS(id, "create_get")),
    tags$h2(textOutput(NS(id, "success")), style = "color:green")
    )
}

# This server module retrieves data from ESPN's API and stores it in a list
# where each element is a a reactive data frame that can then be used by
# other shiny modules.
espn_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    server_env <- environment()
    
    # Check that user inputs are valid
    input_check <- reactive({
      req(input$ff_year, input$espn_id)
      
      validate(need(input$ff_year >= 2019 &
                    input$ff_year <= uwf_current_year(),
                    str_c(" \n  Year must be between 2019 and ",
                          uwf_current_year(), "\n ")))
      
      api_response <-
        paste0("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
               input$ff_year,
               "/segments/0/leagues/",
               input$espn_id,
               "?view=mSettings") %>%
        GET()
      
      api_cat <- httr::http_status(api_response)
      api_reason <-
        case_when(api_cat$reason == "Not Found" ~
                    paste("The League ID you entered was not found; please",
                          "check that you entered the ID correctly. The",
                          "League ID can be found in the URL when looking at",
                          "your fantasy football league (usually it will be",
                          "all numbers and start after 'league/')"),
                  api_cat$reason == "This League has been deleted." ~
                    paste("The league for the ID you entered has been",
                          "deleted and data can no longer be retrieved."),
                  api_cat$reason == "You are not authorized to view this League." ~
                    paste("The league for the ID you entered is set to",
                          "private. If this is your league, ask your league",
                          "manager to set the league to public."),
                  TRUE ~ paste("There was an error retrieving data for the",
                               "league ID you entered.", api_cat$reason))
      validate(need(api_cat$category == "Success", api_reason))
      
      return(content(api_response))
    })
    
    current_week <- reactive({
      week <- as.integer(spelunk(input_check(), "scoringPeriodId"))
      
      validate(need(week != 0,
                    paste(" \n There is no data available in",
                          input$ff_year, "for", input$espn_id, "-",
                          "Please choose a different season and try again.",
                          "\n ")))
      
      return(week)
    })
    
    # Creating action button to allow users to submit information to ESPN's
    # API
    output$create_get <- renderUI({
      req(current_week())
      
      actionButton(NS(id, "get_data"), "Get Data")
    })
    
    # Retrieve data from ESPN's API and store them in reactives
    league_seasons <- reactive({
      req(input$espn_id)
      
      assign(x = "progress",
             value = shiny::Progress$new(),
             envir = server_env)

      progress$set(message = input$espn_id,
                   detail = "retrieving settings...",
                   value = 0.01)
      
      tibble(spelunk_df(input_check(),
                        list(LeagueId = "id",
                             CurrentSeason = "seasonId",
                             IsActive = c("status", "isActive"))),
             FantasySeason =
               unlist(spelunk2(input_check(), c("status",
                                                "previousSeasons")))) %>%
        rbind(spelunk_df(input_check(),
                         list(LeagueId = "id",
                              CurrentSeason = "seasonId",
                              IsActive = c("status", "isActive"),
                              FantasySeason = "seasonId"))) %>%
        filter(IsActive | CurrentSeason != FantasySeason)
    }) %>%
      bindEvent(input$get_data)
    
    espn_league <- reactive({
      req(league_seasons())
      progress$set(message = input$espn_id,
                   detail = "retrieving settings...",
                   value = 0.02)
      
      df_settings <-
        league_seasons() %$%
        map2(LeagueId, FantasySeason, get_espn_settings)
      
      return(df_settings)
    })
    
    espn_settings <- reactive({
      req(espn_league())
      progress$set(message = input$espn_id,
                   detail = "retrieving settings...",
                   value = 0.03)
      
      df_settings <-
        spelunk_df(espn_league(),
                   list(LeagueId = "id",
                        FantasySeason = "seasonId",
                        LeagueName = c("settings", "name"),
                        NumberOfTeams = c("settings", "size"),
                        PlayoffTeams = c("scheduleSettings",
                                         "playoffTeamCount")),
                   rm_na = TRUE) %>%
        mutate(League = "ESPN") %>%
        select(League, everything())
      
      return(df_settings)
    })
    
    espn_settings_roster <- reactive({
      req(espn_settings())
      progress$set(message = input$espn_id,
                   detail = "retrieving settings...",
                   value = 0.04)
      
      df_rosters <- data.frame()
      for (i in seq_along(espn_league())) {
        league_settings <- espn_league()[[i]]
        league_season <- spelunk(league_settings, "seasonId")
        df_roster <-
          spelunk(league_settings, "lineupSlotCounts") %>%
          unlist() %>% {
            tibble(Slot = as.integer(rep(names(.), lengths(.))),
                   NumberOfSlots = unname(.))
          } %>%
          mutate(League = "ESPN",
                 LeagueId = input$espn_id,
                 FantasySeason = as.integer(league_season)) %>%
          select(League, LeagueId, FantasySeason, Slot, NumberOfSlots)
        df_rosters <- rbind(df_rosters, df_roster)
        rm(i, league_settings, league_season, df_roster)
      }
      
      return(df_rosters)  
    })
    
    espn_settings_scoring <- reactive({
      req(espn_settings_roster())
      progress$set(message = input$espn_id,
                   detail = "retrieving settings...",
                   value = 0.05)
      
      df_scorings <- data.frame()
      for (i in seq_along(espn_league())) {
        league_settings <- espn_league()[[i]]
        league_season <- spelunk(league_settings, "seasonId")
        df_scoring <-
          spelunk_df(league_settings,
                     list(Stat = c("scoringItems", "statId"),
                          FantasyPoints = c("scoringItems", "points"),
                          AlternatePoints = c("scoringItems",
                                              "pointsOverrides",
                                              "16"))) %>%
          mutate(FantasyPoints = ifelse(!is.na(AlternatePoints),
                                        AlternatePoints, FantasyPoints)) %>%
          select(Stat, FantasyPoints) %>%
          mutate(League = "ESPN",
                 LeagueId = input$espn_id,
                 FantasySeason = as.integer(league_season)) %>%
          select(League, LeagueId, FantasySeason, Stat, FantasyPoints)
        df_scorings <- rbind(df_scorings, df_scoring)
        rm(i, league_settings, league_season, df_scoring)
      }
      
      return(df_scorings)
    })
    
    espn_settings_schedule <- reactive({
      req(espn_settings_scoring())
      progress$set(message = input$espn_id,
                   detail = "retrieving settings...",
                   value = 0.06)
      
      df_schedules <- data.frame()
      for (i in seq_along(espn_league())) {
        league_settings <- espn_league()[[i]]
        league_season <- spelunk(league_settings, "seasonId")
        playoff_start <- spelunk2(league_settings,
                                  c("scheduleSettings", "matchupPeriodCount"),
                                  rm_na = TRUE)
        df_schedule <-
          spelunk2(league_settings,
                   c("scheduleSettings", "matchupPeriods"),
                   rm_na = TRUE) %>% {
                     map_df(seq_along(.),
                            function (.x) tibble(FantasyWeek = .x,
                                                 nflWeek = unlist(.[[.x]])))
                   } %>%
          mutate(League = "ESPN",
                 LeagueId = input$espn_id,
                 FantasySeason = as.integer(league_season),
                 WeekType = ifelse(FantasyWeek <= playoff_start,
                                   "Regular", "Playoffs")) %>%
          select(League, LeagueId, FantasySeason, FantasyWeek, nflWeek,
                 WeekType)
        df_schedules <- rbind(df_schedules, df_schedule)
        rm(i, league_settings, league_season, df_schedule)
      }
      
      return(df_schedules)
    })
    
    espn_teams <- reactive({
      req(espn_settings_schedule())
      progress$set(message = input$espn_id,
                   detail = "retrieving owners...",
                   value = 0.1)

      df_teams <-
        league_seasons() %$%
        map2_df(LeagueId, FantasySeason, get_espn_teams) %>%
        mutate(League = "ESPN",
               OwnerName = ifelse(!is.na(LastName),
                                  paste(FirstName, LastName),
                                  substr(OwnerId, 1, 6)),
               TeamName = str_c(TeamLocation, TeamNickname, sep = " ")) %>%
        select(League, LeagueId, FantasySeason, OwnerId, OwnerName, TeamId,
               TeamName)
      
      return(df_teams)
    })

    espn_drafts <- reactive({
      req(espn_teams())
      progress$set(message = input$espn_id,
                   detail = "retrieving drafts...",
                   value = 0.15)

      df_drafts <-
        league_seasons() %$%
        map2_df(LeagueId, FantasySeason, get_espn_drafts) %>%
        mutate(League = "ESPN") %>%
        select(League, LeagueId, FantasySeason, PickNumber, TeamId, PlayerId,
                Bid, Keeper)
      
      return(df_drafts)
    })

    espn_schedules <- reactive({
      req(espn_drafts())
      progress$set(message = input$espn_id,
                   detail = "retrieving schedules...",
                   value = 0.2)

      api_schedules <-
        league_seasons() %$%
        map2_df(LeagueId, FantasySeason, get_espn_schedules) %>%
        mutate(Team = "HOME")
      
      df_schedules <-
        api_schedules %>%
        mutate(Team = "AWAY") %>%
        rbind(api_schedules) %>%
        filter(Winner != "UNDECIDED") %>%
        mutate(League = "ESPN",
               WeekType = ifelse(PlayoffType == "NONE",
                                 "Regular", "Playoffs"),
               TeamId = ifelse(Team == "HOME", HomeTeamId, AwayTeamId),
               OpponentId = ifelse(Team == "HOME", AwayTeamId, HomeTeamId),
               PointsFor = ifelse(Team == "HOME",
                                  HomeTeamPoints, AwayTeamPoints),
               PointsAgainst = ifelse(Team == "HOME",
                                      AwayTeamPoints, HomeTeamPoints),
               WinTieLoss = case_when(Winner == "TIE" ~ "Tie",
                                      Team == Winner ~ "Win",
                                      Winner %in% c("HOME", "AWAY") ~ "Loss",
                                      TRUE ~ "Unknown")) %>%
        select(League, LeagueId, FantasySeason, FantasyWeek, WeekType,
               TeamId, OpponentId, PointsFor, PointsAgainst, WinTieLoss)
      
      return(df_schedules)
    })

    espn_rosters <- reactive({
      req(espn_schedules())
      progress$set(message = input$espn_id,
                   detail = "retrieving rosters...",
                   value = 0.25)
      on.exit(progress$close())

      api_urls <-
        espn_settings_schedule() %>%
        filter((FantasySeason > 2018L & FantasySeason < input$ff_year) |
               (FantasySeason == input$ff_year &
                FantasyWeek < current_week())) %>%
        distinct(LeagueId, FantasySeason, nflWeek)

      df_rosters <-
        pmap_df(api_urls,
               .f = function(LeagueId, FantasySeason, nflWeek) {
                 progress$set(message = input$espn_id,
                              detail = "retrieving rosters...",
                              value = progress$getValue() +
                                (0.5 / nrow(api_urls)))
                 get_espn_rosters(LeagueId, FantasySeason, nflWeek)
               })

      rm(api_urls)

      return (df_rosters)
    })
    
    # Displaying message that data was successfully retrieved
    output$success <- renderText({
      req(espn_rosters())
      
      paste(tail(espn_settings()$LeagueName, 1),
            "data successfully retrieved")
    })
    
    # Create list that will hold all of the demo data so that it can be used
    # by other shiny modules
    list(settings_league = espn_settings,
         settings_schedule = espn_settings_schedule,
         settings_rosters = espn_settings_roster,
         settings_scoring = espn_settings_scoring,
         teams = espn_teams,
         drafts = espn_drafts,
         schedules = espn_schedules,
         rosters = espn_rosters)
  })
}

# test the module ------------------------------------------------------------

# uncomment the code below to test the module in isolation from the rest of
# the app

# Create app from the ui and server elements so this can be tested separate
# from the other modules of the complete app

# test_app <- function() {
#   ui <- fluidPage(
#     espn_ui("espn"),
#     verbatimTextOutput("test")
#   )
#   
#   server <- function(input, output, session) {
#     test_data <- espn_server("espn")
#     shiny_data <- reactive({
#       # write_csv(test_data$settings_league(),
#       #           here("espn_settings_league.csv"), na = "")
#       # write_csv(test_data$settings_schedule(),
#       #           here("espn_settings_schedule.csv"), na = "")
#       # write_csv(test_data$settings_rosters(),
#       #           here("espn_settings_rosters.csv"), na = "")
#       # write_csv(test_data$settings_scoring(),
#       #           here("espn_settings_scoring.csv"), na = "")
#       # write_csv(test_data$teams(),
#       #           here("espn_teams.csv"), na = "")
#       # write_csv(test_data$drafts(),
#       #           here("espn_drafts.csv"), na = "")
#       # write_csv(test_data$schedules(),
#       #           here("espn_schedules.csv"), na = "")
#       # write_csv(test_data$rosters(),
#       #           here("espn_rosters.csv"), na = "")
#       test_data$rosters()
#     })
#     output$test <- renderPrint({
#       head(shiny_data(), 10)
#     })
#   }
#   
#   shinyApp(ui, server)
# }
# 
# test_app()
