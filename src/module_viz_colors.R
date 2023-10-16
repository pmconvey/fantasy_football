# readme ---------------------------------------------------------------------

# This script is a shiny module that goes with my Fantasy Football Data
# Retriever shiny app. This module allows users to choose what colors the
# graphs will be.

# Load r packages
require(here)
require(shiny)
require(tidyverse)
require(patchwork)
require(colourpicker)

# shiny module ---------------------------------------------------------------

# Create sample dataset to show users how the colors will be used in the app
set.seed(500)
demo <-
  data.frame(a = rep(c("A", "B", "C", "D", "E", "F", "G", "H"), 4),
             b = rep(2018:2021, each = 8),
             c = sample.int(n = 100, size = 32, replace = TRUE),
             d = as.character(sample.int(n = 3, size = 32, replace = TRUE))
  )

# If the default colors need to be updated, update them here
colors <- c('#4fb13e', '#d1d1d1', '#646464', '#4fb13e', '#d1d1d1', '#ed6167')

color_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3,
                   colourInput(NS(id, "col1"), "Selected", colors[1]),
                   colourInput(NS(id, "col2"), "Unselected", colors[2]),
                   colourInput(NS(id, "col3"),
                               "Unselected (secondary)", colors[3]),
                   colourInput(NS(id, "col4"), "Positive Outcome", colors[4]),
                   colourInput(NS(id, "col5"), "Neutral Outcome", colors[5]),
                   colourInput(NS(id, "col6"), "Negative Outcome", colors[6]),
                   actionButton(style = "text-align:center;",
                                NS(id, "default"),
                                "Restore Default Colors",
                                width = 200L)
      ),
      mainPanel(width = 9,
                tags$h4("Graphs showing 'Selected,' 'Unselected,' and",
                        "'Unselected (secondary)' colors"),
                plotOutput(NS(id, "col_selected"), height = "300px"),
                tags$h4("Graphs showing 'Positive Outcome', 'Neutral",
                        "Outcome' and 'Negative Outcome' colors."),
                plotOutput(NS(id, "col_posneg"), height = "300px")
      )
    )
  )
}

color_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Graphs using the selected and unselected colors
    output$col_selected <- renderPlot({
      add_on <- demo$c[demo$b == 2021] + 8L
      
      bar <-
        ggplot(demo[demo$b == 2021, ],
               aes(x = reorder(a, c),
                   y = c,
                   label = c,
                   hjust = 1.2,
                   fill = c(F, F, F, T, F, F, F, F))) +
        geom_col(aes(y = add_on),
                 fill = input$col3) +
        geom_text(aes(y = add_on,
                      label = add_on),
                  color = "#FFFFFF", fontface = "bold") +
        geom_col() +
        geom_text(color = "#FFFFFF", fontface = "bold") +
        scale_fill_manual(values = c(input$col2, input$col1)) +
        labs(title = NULL,
             x = NULL,
             y = NULL) +
        coord_flip(expand = FALSE) +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.position = "none")
      
      line <-
        ggplot(demo,
               aes(x = b,
                   y = c,
                   group = a,
                   colour = rep(c(F, F, F, T, F, F, F, F), 4))) +
        geom_line(linewidth = 2) +
        scale_colour_manual(values = c(input$col2, input$col1)) +
        labs(title = NULL,
             x = NULL,
             y = NULL) +
        theme(panel.background = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.position = "none")
      
      bar + line
    })
    
    # Graphs using the positive and negative outcome colors
    output$col_posneg <- renderPlot({
      bar <-
        ggplot(demo[demo$b == 2020, ],
               aes(x = reorder(a, c),
                   y = c,
                   label = c,
                   hjust = 1.2,
                   fill = c(F, T, T, T, F, T, F, F))) +
        geom_col() +
        geom_text(color = "#FFFFFF", fontface = "bold") +
        scale_fill_manual(values = c(input$col6, input$col4)) +
        labs(title = NULL,
             x = NULL,
             y = NULL) +
        coord_flip(expand = FALSE) +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.position = "none")
      
      heatmap <-
        ggplot(demo,
               aes(x = b,
                   y = a,
                   fill = c)) +
        geom_tile(colour = "#FFFFFF") +
        scale_fill_gradient2(low = input$col6,
                             mid = input$col5,
                             high = input$col4,
                             midpoint = 50) +
        scale_x_continuous(breaks = 2018:2021) +
        coord_cartesian(expand = FALSE) +
        labs(title = NULL,
             x = NULL,
             y = NULL) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor.y = element_line(colour = "#FFFFFF"),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.title = element_blank())
      
      bar + heatmap
    })
    
    # Restore default colors when button is clicked
    observeEvent(input$default, {
      updateColourInput(session, inputId = "col1", value = colors[1])
      updateColourInput(session, inputId = "col2", value = colors[2])
      updateColourInput(session, inputId = "col3", value = colors[3])
      updateColourInput(session, inputId = "col4", value = colors[4])
      updateColourInput(session, inputId = "col5", value = colors[5])
      updateColourInput(session, inputId = "col6", value = colors[6])
    })
    
    # Create list that will hold all of the colors so the output of this
    # module can be used by other shiny modules to set the colors of those
    # graphs
    list(col1 = reactive(input$col1),
         col2 = reactive(input$col2),
         col3 = reactive(input$col3),
         col4 = reactive(input$col4),
         col5 = reactive(input$col5),
         col6 = reactive(input$col6)
    )
  })
}

# test the module ------------------------------------------------------------

# uncomment the code below to test the module in isolation from the rest of
# the app

# test_app <- function() {
#   ui <-
#     fluidPage(
#       color_ui("color")
#       )
# 
#   server <- function(input, output, session) {
#     color_server("color")
#   }
# 
#   shinyApp(ui, server)
# }
# 
# test_app()
