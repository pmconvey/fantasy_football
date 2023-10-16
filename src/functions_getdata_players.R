# readme ---------------------------------------------------------------------

# This script creates a function that gets NFL player data that can be used
# in my fantasy football shiny app. Because it doesn't rely on user inputs
# from the shiny app (the function will always use its default inputs),
# I created it as a function rather than a shiny module.

# Make sure necessary r packages are loaded
require(here)
require(tidyverse)
require(nflreadr)

# function to return player stats --------------------------------------------

player_data <- function(ff_seasons = nflreadr::most_recent_season(),
                        force_update = FALSE) {
  # Load saved player stats
  old_stats <- read_csv("player_data.csv",
                        col_types = cols(fg_blocked_list = col_character()))
  
  # Check saved player stats to see if there is more recent data available
  up_to_date <-
    old_stats %>%
    filter(season == nflreadr::get_current_season()) %>%
    pull(week) %>% {
      nflreadr::get_current_week() %in% .
    }
  
  # Get more recent data if available, if not use what is saved
  if (up_to_date & !force_update) {
    new_player_stats <- old_stats
    player_ids <- read_csv("player_ids.csv")
  } else {
    # Getting data using nflreadr package
    players_position <-
      nflreadr::load_player_stats(seasons = ff_seasons)
    
    players_kickers <-
      nflreadr::load_player_stats(seasons = ff_seasons,
                                  stat_type = "kicking") %>%
      mutate(position = "K")
    
    # The functions above usually will only get the current / most recent
    # season so combining with the saved data and then deduplicating
    new_player_stats <-
      bind_rows(players_position, players_kickers) %>%
      bind_rows(old_stats) %>%
      distinct(player_id, season, week, .keep_all = TRUE)
    
    # Concordance to facilitate joining players with data from fantasy
    # leagues
    player_ids <- nflreadr::load_ff_playerids()
    
    # Overwriting saved data to be up-to-date
    write_csv(new_player_stats, here("player_data.csv"))
    write_csv(player_ids, here("player_ids.csv"))
  }
  
  # Removing columns that are not used in the join in the next step, but
  # are present in the new_player_stats dataframe
  player_ids <- select(player_ids, -position, -team)
  
  new_player_stats <-
    left_join(x = new_player_stats,
              y = player_ids,
              by = c("player_id" = "gsis_id", "season" = "db_season"))
  
  rm(old_stats, up_to_date, player_ids)
  return(new_player_stats)
}
