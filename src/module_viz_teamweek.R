# readme ---------------------------------------------------------------------

require(here)
require(shiny)
require(tidyverse)
require(ggbeeswarm)
require(plotly)

# load data ------------------------------------------------------------------

team_week <- read_csv(here("demo_data", "team_week.csv"))

# shiny app ------------------------------------------------------------------

og <-
  team_week %>%
  group_by(OwnerName) %>%
  mutate(lastseason = max(FantasySeason)) %>%
  ungroup() %>%
  filter(lastseason == 2022 | OwnerName == "Ethan") %>%
  filter(FantasySeason == 2022 & WeekType == "Regular") %>%
  ggplot(aes(x = FantasyWeek, y = PointsFor, color = WinTieLoss,
             text = paste0(OwnerName, "\n", PointsFor,
                           " - ", PointsAgainst, " (", OppOwnerName,
                           ")\nMargin: ", round(100, digits = 2),
                           " points (", round(100, digits = 1),
                           "%)"))) +
  geom_beeswarm(size = 2, method = "hex") +
  scale_x_continuous(labels = seq(1:20),
                     breaks = seq(from = 0.5, to = 20, by = 1),
                     guide = "none") +
  facet_grid(rows = vars(FantasySeason)) +
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(hjust = 0),
        panel.grid.minor.x = element_blank())

og

ggplotly(og, tooltip = c("text"), dynamicTicks = TRUE)

