# readme ---------------------------------------------------------------------

# This script goes with my Fantasy Football shiny app. It creates functions
# that are used in multiple modules to make creating visualizations easier.

# create plot functions ------------------------------------------------------

# Prettily format numbers for use as labels in visualizations
format_number <- function (x, digits = 0, start_sym = NULL, end_sym = NULL) {
  prettyNum(paste0(start_sym,
                   floor(round(x, digits)),
                   substr(formatC(round(x, digits) - floor(round(x, digits)),
                                  width = digits,
                                  flag = "#"),
                          2L,
                          digits + 2L),
                   end_sym),
            big.mark = ",")
}

# Format numbers to be written with ordinal indicators
uwf_format_ordinal <- function (x) {
  stopifnot(is.integer(x))
  
  vapply(X = x,
         FUN = function(x) {
           if (x %% 100 %in% c(11, 12, 13)) {
             fmt_ordinal <- paste0(x, "th")
           } else if (x %% 10 == 1) {
             fmt_ordinal <- paste0(x, "st")
           } else if (x %% 10 == 2) {
             fmt_ordinal <- paste0(x, "nd")
           } else if (x %% 10 == 3) {
             fmt_ordinal <- paste0(x, "rd")
           } else {
             fmt_ordinal <- paste0(x, "th")
           }
         },
         FUN.VALUE = character(1L))
}

# table icons to user in a reactable table
table_icons <- function(value, img_icon, color = NULL) {
  team_icon <- function(img_icon) {
    tagAppendAttributes(shiny::icon(img_icon),
                        style = paste("color:",
                                      if (!is.null(color)) {
                                        color
                                      } else if (img_icon == "trophy") {
                                        "orange"
                                      } else if (img_icon == "wine-bottle") {
                                        "red"
                                      } else if (img_icon == "toilet") {
                                        "red"
                                      } else if (img_icon == "award") {
                                        "blue"
                                      } else if (img_icon == "exclamation-circle") {
                                        "red"
                                      } else {"black"}),
                        "aria-hidden" = "true"
    )
  }
  num_icons <- lapply(seq_len(value), function(i) {
    if (i <= value) team_icon(img_icon)
  })
  label <- sprintf("%s", value)
  div(title = label, role = "img", num_icons)
}

# Create a bar chart that can be used in a gt table (the gt package)
uwf_table_bar <- function (df, x, y, color) {
  map(seq_along(df[[x]]),
      ~ ggplot(mapping = aes(x = df[[x]][.x],
                             y = df[[y]][.x],
                             fill = df[[y]][.x])) +
        geom_col() +
        scale_fill_manual(values = df[[color]][.x]) +
        coord_cartesian(xlim = c(0, max(df[[x]], na.rm = TRUE)),
                        expand = FALSE) +
        theme(axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              legend.position = "none",
              panel.background = element_blank())
  )
}
