# readme ---------------------------------------------------------------------

# This script goes with my Fantasy Football shiny app. It creates functions
# to transform the data returned from the modules that access the APIs to
# formats that are easier to create tables, graphs, and charts from.

require(here)
require(data.table)

# load data ------------------------------------------------------------------

# This section is for testing and loads demo data that is in the same format
# as what would be returned by different modules that get data.

# ff_settings <- fread(here("demo_data", "demo_settings_league.csv"))
# ff_settings_schedule <- fread(here("demo_data", "demo_settings_schedule.csv"))
# ff_settings_roster <- fread(here("demo_data", "demo_settings_rosters.csv"))
# ff_settings_scoring <- fread(here("demo_data", "demo_settings_scoring.csv"))
# ff_users <- fread(here("demo_data", "demo_teams.csv"))
# ff_drafts <- fread(here("demo_data", "demo_drafts.csv"))
# ff_schedules <- fread(here("demo_data", "demo_schedules.csv"))
# ff_rosters <- fread(here("demo_data", "demo_rosters.csv"))

# create functions to transform the data -------------------------------------

# The typecast functions ensure that columns are coerced to a specific
# data type so that there are not an unexpected type mismatch errors with
# other functions or in the shiny modules.

typecast_settings_league <- function(df) {
  setDT(df)
  df[, ':=' (League = as.character(League),
             LeagueId = as.character(LeagueId),
             FantasySeason = as.integer(FantasySeason),
             LeagueName = as.character(LeagueName),
             NumberOfTeams = as.integer(NumberOfTeams),
             PlayoffTeams = as.integer(PlayoffTeams))
  ]
}

typecast_settings_roster <- function(df) {
  setDT(df)
  df[, ':=' (League = as.character(League),
             LeagueId = as.character(LeagueId),
             FantasySeason = as.integer(FantasySeason),
             Slot = as.character(Slot),
             NumberOfSlots = as.integer(NumberOfSlots))
  ]
}

typecast_settings_schedule <- function(df) {
  setDT(df)
  df[, ':=' (League = as.character(League),
             LeagueId = as.character(LeagueId),
             FantasySeason = as.integer(FantasySeason),
             FantasyWeek = as.integer(FantasyWeek),
             nflWeek = as.integer(nflWeek),
             WeekType = as.character(WeekType))
  ]
}

typecast_settings_scoring <- function(df) {
  setDT(df)
  df[, ':=' (League = as.character(League),
             LeagueId = as.character(LeagueId),
             FantasySeason = as.integer(FantasySeason),
             Stat = as.character(Stat),
             FantasyPoints = as.numeric(FantasyPoints))
  ]
}

typecast_teams <- function(df) {
  setDT(df)
  df[, ':=' (League = as.character(League),
             LeagueId = as.character(LeagueId),
             FantasySeason = as.integer(FantasySeason),
             OwnerId = as.character(OwnerId),
             OwnerName = as.character(OwnerName),
             TeamId = as.character(TeamId),
             TeamName = as.character(TeamName))
  ]
}

typecast_drafts <- function(df) {
  setDT(df)
  df[, ':=' (League = as.character(League),
             LeagueId = as.character(LeagueId),
             FantasySeason = as.integer(FantasySeason),
             PickNumber = as.integer(PickNumber),
             TeamId = as.character(TeamId),
             PlayerId = as.character(PlayerId),
             Bid = as.numeric(Bid),
             Keeper = as.logical(Keeper))
  ]
}

typecast_schedules <- function(df) {
  setDT(df)
  df[, ':=' (League = as.character(League),
             LeagueId = as.character(LeagueId),
             FantasySeason = as.integer(FantasySeason),
             FantasyWeek = as.integer(FantasyWeek),
             WeekType = as.character(WeekType),
             TeamId = as.character(TeamId),
             OpponentId = as.character(OpponentId),
             PointsFor = as.numeric(PointsFor),
             PointsAgainst = as.numeric(PointsAgainst),
             WinTieLoss = as.character(WinTieLoss))
  ]
}

typecast_rosters <- function(df) {
  setDT(df)
  df[, ':=' (League = as.character(League),
             LeagueId = as.character(LeagueId),
             FantasySeason = as.integer(FantasySeason),
             nflWeek = as.integer(nflWeek),
             TeamId = as.character(TeamId),
             PlayerId = as.character(PlayerId),
             Starter = as.logical(Starter))
  ]
}

# Creates data.frame where the unit of analysis is league, season, week, team
transform_team_week <- function (schedule, settings, teams) {
  temp_df <- typecast_schedules(schedule)
  settings <- typecast_settings_league(settings)
  owners <- typecast_teams(teams)
  
  # adding cumulative wins, points scored, and points against
  setorder(temp_df, League, LeagueId, FantasySeason, TeamId, FantasyWeek)
  temp_df[,
          ':=' (SeasonWins = cumsum(fcase(WinTieLoss == "Win", 1,
                                          WinTieLoss == "Tie", 0.5,
                                          WinTieLoss == "Loss", 0)),
                SeasonPointsFor = cumsum(PointsFor),
                SeasonPointsAgainst = cumsum(PointsAgainst)),
          by = .(League, LeagueId, FantasySeason, TeamId)
  ]
  
  # adding team week rank, opponent week rank, and season rank
  temp_df[,
          ':=' (WeekRank = frank(PointsFor, ties.method = "average"),
                OppWeekRank = frank(PointsAgainst, ties.method = "average"),
                StandingsRank = frank(list(-SeasonWins, -SeasonPointsFor),
                                      ties.method = "first")),
          by = .(League, LeagueId, FantasySeason, FantasyWeek)
  ]
  
  # adding win probability and loss probablity
  temp_df[,
          ':=' (WinProb = (WeekRank - 1L) / (.N - 1L),
                LossProb = fifelse(WinTieLoss == "Win",
                                   (OppWeekRank - 1L) / (.N - 2L),
                                   (OppWeekRank - 2L) / (.N - 2L))),
          by = .(League, LeagueId, FantasySeason, FantasyWeek)
  ]
  
  # adding cumulative win and loss probabilities
  setorder(temp_df, League, LeagueId, FantasySeason, TeamId, FantasyWeek)
  temp_df[,
          ':=' (SeasonWinProb = dplyr::cummean(WinProb),
                SeasonLossProb = dplyr::cummean(LossProb)),
          by = .(League, LeagueId, FantasySeason, TeamId)
  ]
  
  # adding league and owner info
  temp_df <-
    temp_df[settings,
            on = .(League, LeagueId, FantasySeason)
    ]
  temp_df[, ':=' (NumberOfTeams = NULL)
  ]
  
  temp_df <-
    temp_df[owners,
            on = .(League, LeagueId, FantasySeason, OpponentId=TeamId)
    ]
  
  setnames(temp_df,
           c("OwnerId", "OwnerName", "TeamName"),
           c("OppOwnerId", "OppOwnerName", "OppTeamName"),
           skip_absent = TRUE)
  
  temp_df <-
    temp_df[owners,
            on = .(League, LeagueId, FantasySeason, TeamId)
    ]
  
  setcolorder(temp_df,
              c("League", "LeagueId", "LeagueName", "FantasySeason",
                "FantasyWeek", "WeekType", "OwnerName", "WinTieLoss",
                "PointsFor", "PointsAgainst", "WeekRank", "WinProb",
                "OppWeekRank","LossProb", "StandingsRank", "SeasonWins",
                "SeasonPointsFor", "SeasonPointsAgainst", "SeasonWinProb",
                "SeasonLossProb", "OwnerId", "TeamId", "TeamName",
                "OpponentId", "OppOwnerId", "OppOwnerName", "OppTeamName",
                "PlayoffTeams"))
  
  return(temp_df)
}

# Creates data.frame where the unit of analysis is league, season, team
transform_team_season <- function (team_week) {
  temp_df <- team_week
  setDT(temp_df)
  
  regular_season <-
    temp_df[WeekType == "Regular"
    ][,
      ':=' (LastRegWeek = max(FantasyWeek),
            MadePlayoffs = (StandingsRank <= PlayoffTeams)),
      by = .(League, LeagueId, FantasySeason)
    ][FantasyWeek == LastRegWeek
    ][,
      .(League, LeagueId, LeagueName, FantasySeason, OwnerName, StandingsRank,
        MadePlayoffs, SeasonWins, SeasonPointsFor, SeasonPointsAgainst,
        SeasonWinProb, SeasonLossProb, OwnerId, TeamId, TeamName)
    ]
  
  playoffs <-
    regular_season[, .(League, LeagueId, FantasySeason, TeamId, MadePlayoffs)
    ][temp_df, on = .(League, LeagueId, FantasySeason, TeamId)
    ][WeekType == "Playoffs",
      .(League, LeagueId, FantasySeason, FantasyWeek, TeamId, MadePlayoffs,
        WinTieLoss)
    ]
  
  playoffs[,
           ':=' (Removed = fcase(MadePlayoffs & WinTieLoss == "Loss", 1L,
                                 !MadePlayoffs & WinTieLoss == "Win", 1L,
                                 default = 0L))
  ]
  
  setorder(playoffs, League, LeagueId, FantasySeason, FantasyWeek, TeamId)
  playoffs[,
           ':=' (InContention = cumsum(Removed)),
           by = .(League, LeagueId, FantasySeason, TeamId)
  ]
  
  playoffs <- playoffs[InContention == 0L
                       # Removing the League Loser from my league in 2014
                       # because we started keeping track of it in 2015
                       & !(FantasySeason == 2014 & TeamId == 10
                           & League %in% c('ESPN', 'DEMO')
                           & LeagueId %in% c(1265101, 'demo'))]
  playoffs[,
           ':=' (MaxWeek = max(FantasyWeek)),
           by = .(League, LeagueId, FantasySeason, MadePlayoffs)
  ]
  playoffs <-
    playoffs[FantasyWeek == MaxWeek
             , .(League, LeagueId, FantasySeason, TeamId, Award = TRUE)]
  
  temp_df <-
    playoffs[regular_season, on = .(League, LeagueId, FantasySeason, TeamId)
    ][, ':=' (Champion = fifelse(MadePlayoffs & Award, TRUE, FALSE, FALSE),
              Loser = fifelse(!MadePlayoffs & Award, TRUE, FALSE, FALSE),
              Award = NULL)
    ]
  
  setcolorder(temp_df,
              c("League", "LeagueId", "LeagueName", "FantasySeason",
                "OwnerName", "TeamName", "Champion", "Loser", "MadePlayoffs",
                "StandingsRank", "SeasonWins", "SeasonPointsFor",
                "SeasonPointsAgainst", "SeasonWinProb", "SeasonLossProb",
                "OwnerId", "TeamId"))
  return(temp_df)
}
