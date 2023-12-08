# readme ---------------------------------------------------------------------

# This Shiny App allows people to get data for their fantasy football league
# and visualize and download that data.
# (currently only ESPN and Sleeper)

# Load and, if necessary, install r packages
r_packages <- c("here",         # navigate file directories more easily
                "shiny",        # build interactive apps
                "htmltools",    # 
                "httr",         # GET and content functions
                "data.table",   # fast data manipulation
                "magrittr",     # pipe functions (specifically %$%)
                "tidyverse",    # tidy data more easily
                "ggbump",       # create bump charts with geom_bump
                "ggbeeswarm",   # create beeswarm charts with geom_beeswarm
                "plotly",       # interactive data visualizations
                "colourpicker", # allow end users to choose colors
                "patchwork",    # combine separate ggplots
                "reactable",    # interactive html tables
                "gt") 

if (any(!r_packages %in% installed.packages())) {
  install.packages(r_packages[!r_packages %in% installed.packages()])
}
lapply(r_packages, library, character.only = TRUE)
rm(r_packages)

# load shiny modules and functions -------------------------------------------

addResourcePath('www', here('www'))

# Functions I've written to make building the app easier
source(here("src", "functions_uwf.R"))
source(here("src", "functions_transform_data.R"))
source(here("src", "functions_viz.R"))

# Modules to get data from the various fantasy leagues. If a module for a new
# league is added, the code needs to be updated here, in the popup_ui
# function, and in the ffdata_server function (in the transformed data
# reactive to use the data).
source(here("src", "module_getdata_demo.R"))
source(here("src", "module_getdata_sleeper.R"))
source(here("src", "module_getdata_espn.R"))

# Modules to visualize the fantasy football data. If a new module is added,
# the code needs to be updated to add a column to the components_ui, add a
# tabPanelBody to ffdata_ui, and add an observer that is triggered by
# bindEvent to ffdata_server
source(here("src", "module_viz_teamsummary.R"))
source(here("src", "module_viz_teamstats.R"))
source(here("src", "module_viz_weeklystandings.R"))
source(here("src", "module_viz_weeklypoints.R"))
source(here("src", "module_viz_scheduleswitch.R"))
source(here("src", "module_viz_download.R"))
source(here("src", "module_viz_colors.R"))

# shiny app ------------------------------------------------------------------

# Pop-up that displays when Shiny is launched. Shows the inputs so users can
# select the fantasy football league for which they want to get data.
popup_ui <- function () {
  tagList(
    tags$p("This shiny app allows you to download data for your fantasy",
           "football league. Select which company hosts your league",
           "(only ESPN and Sleeper are currently options) and input the",
           "necessary information."),
    tags$p(tags$strong("If you do not have a league, leave the selection",
                       "as 'Demo' and click 'Get Data'.")),
    tags$br(),
    radioButtons("fantasy_league",
                 "League",
                 choices = c("Demo",
                             "ESPN",
                             "Sleeper"),
                 width = 175L),
    conditionalPanel(condition = 'input.fantasy_league == "Demo"',
                     demo_ui("demo")),
    conditionalPanel(condition = 'input.fantasy_league == "ESPN"',
                     espn_ui("espn")),
    conditionalPanel(condition = 'input.fantasy_league == "Sleeper"',
                     sleeper_ui("sleeper"))
  )
}

# Main page -- shows an image button for each visualization module that can
# be clicked to launch that module. Separating from the header and footer that
# I always want visible (this is basically another Shiny UI module that I
# can't figure out how to modularize).
components_ui <- function () {
  tagList(
    tags$head(tags$style(HTML(".imageFormat {
                                display: block;
                                margin-left: auto;
                                margin-right: auto;
                                width: 100%;
                                height: 150px;
                                }
                                "))),
    tags$div(
      style = "text-align: center;",
      tags$p(style = "margin-left:10%; margin-right:10%;",
             tags$span(style = "color:green;",
                       textOutput("close_dialog", inline = TRUE)),
             "Click the images below to open pages that have different",
             "visualizations. If you want to get information for a different",
             "league, you will need to reload the page."),
      # Because I don't really understand html to have the images be the same
      # width, they all have to have the same aspect ratio. To create the
      # images I used, I took screenshots and then edited them in powerpoint
      # to be 5.5 x 13.33
      fluidRow(column(width = 6L,
                      tags$h3("League History"),
                      actionButton("go_seasons",
                                   label = tags$img(src = "www/seasons.png",
                                                    class = "imageFormat")),
                      tags$p("see how teams have done each year")),
               column(width = 6L,
                      tags$h3("Team Stats"),
                      actionButton("go_teams",
                                   label = tags$img(src = "www/teams.png",
                                                    class = "imageFormat")),
                      tags$p("see aggregate stats for teams for all seasons"))
      ),
      tags$br(),
      fluidRow(column(width = 6L,
                      tags$h3("Weekly Standings"),
                      actionButton("go_standings",
                                   label = tags$img(src = "www/standings.png",
                                                    class = "imageFormat")),
                      tags$p("see how the standings changed week-by-week")),
               column(width = 6L,
                      tags$h3("Weekly Points"),
                      actionButton("go_weeklypoints",
                                   label = tags$img(src = "www/weeklypoints.png",
                                                    class = "imageFormat")),
                      tags$p("see how teams scored during each week"))
      ),
      fluidRow(column(width = 6L,
                      tags$h3("Schedule Switcher"),
                      actionButton("go_switch",
                                   label = tags$img(src = "www/switch.png",
                                                    class = "imageFormat")),
                      tags$p("see how teams would do with different schedules"))
      )
    )
  )
}

ffdata_ui <- function () {
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/text.css")
    ),
    tags$div(style = "text-align:center", class = "darkbg",
             tags$br(),
             tags$h1("fantasy football data visualization tool"),
             tags$span(
               conditionalPanel(condition = "input.pages != 'home'",
                                actionButton("home",
                                             label = "Main Page",
                                             icon = icon("house")))
             ),
             tags$br()
    ),
    tags$br(),
    tabsetPanel(
      id = "pages",
      type = "hidden",
      selected = "home",
      tabPanelBody("home", components_ui()),
      tabPanelBody("season_outcomes", teamsummary_ui("seasons")),
      tabPanelBody("team_stats", teamstats_ui("teams")),
      tabPanelBody("weekly_standings", standings_ui("standings")),
      tabPanelBody("weekly_points", weeklypoints_ui("weekly_points")),
      tabPanelBody("switch", switch_ui("schedule_switch")),
      tabPanelBody("colors", color_ui("color")),
      tabPanelBody("download", download_ui("download"))
    ),
    tags$hr(),
    tags$span(style = "text-align:left", "made with ",
           tags$a("shiny",
                  href = "https://shiny.posit.co/",
                  target = "_blank"),
           tags$span(style = "float:right",
                     actionLink("go_download",
                                "download the data",
                                icon = icon("download")))),
    tags$p(style = "text-align:left", "code available on ",
           tags$a("github",
                  href = "https://github.com/pmconvey/fantasy_football",
                  target = "_blank"),
           tags$span(style = "float:right",
                     actionLink("go_colors",
                                "change the colors",
                                icon = icon("paint-brush"))))
  )
}

ffdata_server <- function(input, output, session) {
  # Call modules that get data from the various fantasy league APIs
  # and display pop-up that has the user controls for defining what league
  # to get data for
  showModal(modalDialog(popup_ui(),
                        footer = NULL,
                        size = "l"))
  
  output$close_dialog <- renderText({
    # This output needs to be on the main page of the app because it triggers
    # closing the modal dialog, which is set up so it can't be closed any
    # other way
    req(input$fantasy_league, team_season())
    removeModal()
    return(paste0("Data successfully retrieved and transformed for ",
                  tail(team_season()$LeagueName, 1), "."))
  })
  
  # Depending on the selection by the user, transform the data for that
  # fantasy league and then separate the parts out into their own reactives
  # to make them easier to use in other modules
  api_data <- reactive({
    req(input$fantasy_league)
    
    if (input$fantasy_league == "Sleeper") {
      api_data <- sleeper_server("sleeper")
    } else if (input$fantasy_league == "ESPN") {
      api_data <- espn_server("espn")
    } else if (input$fantasy_league == "Demo") {
      api_data <- demo_server("demo")
    }
    
    return(api_data)
  })
  team_week <- reactive({
    transform_team_week(api_data()$schedules(),
                        api_data()$settings_league(),
                        api_data()$teams())
  })
  team_season <- reactive({
    transform_team_season(team_week())
  })
  
  # Visualization modules
  download_server("download", team_week, team_season)
  color_list <- color_server("color")
  data_r <- reactiveValues(highlight = reactive(NA),
                           data = NA,
                           name = NA,
                           prv_page = NA)

  data_r$highlight <- reactive({
    if (!isTruthy(data_r$prv_page)) {
      NA
    } else if (data_r$prv_page == "team_stats") {
      teamstats_highlight()
    } else if (data_r$prv_page == "weekly_standings") {
      standings_highlight()
    } else if (data_r$prv_page == "weekly_points") {
      weeklypoints_highlight()
    }
  })

  teamsummary_server("seasons", color_list, team_season)
  switch_server("schedule_switch", color_list, team_week, team_season)

  teamstats_highlight <-
    teamstats_server("teams", color_list, data_r$highlight, team_week)

  standings_highlight <-
    standings_server("standings", color_list, data_r$highlight, team_week)
  
  weeklypoints_highlight <-
    weeklypoints_server("weekly_points", color_list, data_r$highlight, team_week)
  
  # Observers that change the page that is shown when one of the image buttons
  # is clicked
  observe({
    if(input$pages != "home") {
      data_r$prv_page <- input$pages
    }
    
    updateTabsetPanel(session, "pages", selected = "home")
  }) %>%
    bindEvent(input$home)
  
  observe(updateTabsetPanel(session, "pages",
                            selected = "season_outcomes")) %>%
    bindEvent(input$go_seasons)
  
  observe(updateTabsetPanel(session, "pages",
                            selected = "team_stats")) %>%
    bindEvent(input$go_teams)
  
  observe(updateTabsetPanel(session, "pages",
                            selected = "weekly_standings")) %>%
    bindEvent(input$go_standings)
  
  observe(updateTabsetPanel(session, "pages",
                            selected = "weekly_points")) %>%
    bindEvent(input$go_weeklypoints)
  
  observe(updateTabsetPanel(session, "pages",
                            selected = "switch")) %>%
    bindEvent(input$go_switch)
  
  observe(updateTabsetPanel(session, "pages",
                            selected = "colors")) %>%
    bindEvent(input$go_colors)
  
  observe(updateTabsetPanel(session, "pages",
                            selected = "download")) %>%
    bindEvent(input$go_download)
}

# Put the user interface and server into one function
ffdata_app <- function () {
  shinyApp(ffdata_ui(), ffdata_server)
}

# Run the shiny app
ffdata_app()
