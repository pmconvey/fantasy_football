# readme ---------------------------------------------------------------------

# This script goes with my Fantasy Football shiny app. It creates functions
# that can be used by the different modules.

# functions ------------------------------------------------------------------

# if a value is null, replaces with a selected value
uwf_if_null <- function(x, replacement = NA) {
  if (is.null(x)) {
    return (replacement)
  } else {
    x[vapply(x, is.null, logical(1))] <- replacement
    return (x)
  }
}

# sets the year, based on the month -- if January through August,
# the previous year; if September through December, the current year
uwf_current_year <- function(date = Sys.Date()) {
  current_date <- as.POSIXlt(date)
  
  if (current_date$mon %in% c(8:11)) {
    current_year <- current_date$year + 1900
  } else {
    current_year <- current_date$year + 1899
  }
  
  return(current_year)
}