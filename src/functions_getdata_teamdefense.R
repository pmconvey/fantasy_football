# readme ---------------------------------------------------------------------

# This script creates a function that gets NFL team defensive data that can be
# used in my fantasy football shiny app. Because it doesn't rely on user
# inputs from the shiny app (the function will take its inputs from a stored
# csv), I created it as a function rather than a shiny module.

# Make sure necessary r packages are loaded
require(here)
require(tidyverse)
require(httr)
require(rvest)
require(xml2)

# function to return team defense stats --------------------------------------

source(here("r_scripts", "module_getdata_espn.R"))

team_defense_stats <-
  map2(.x = 2022, .y = 1:5,
       ~get_espn_players(1265101, .x, .y, 16) %>%
         mutate(FantasySeason = .x, nflWeek = .y)) %>%
  list_rbind() %>%
  filter(StatType == "Actual")
