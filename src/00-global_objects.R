# Global constants and functions

# Global constants ------------------------------------------------

# glob <- list()
# glob <- within(glob, {
# 
#   # iso-week which starts epi-year
#   start_of_epi_year_iso_week = 27
#   # iso-week which starts test/prediction period
#   start_of_test_iso_week = 8
#   # weeks to forecast
#   forecast_n_weeks = 52*3
#   
# })

# Figure specification --------------------------------------------

# fonts
library(showtext)
font_add_google('Roboto', 'roboto')
font_add_google('Roboto Condensed', 'robotocondensed')
showtext_auto()

figspec <- list()
figspec <- within(figspec, {
  
  # color coding
  colors = list(
    sample =
      c(
        training = "grey30",
        test = "red"
      ),
    sex =
      c(
        `Male` = "#004B87",
        `Female` = "#c60c30"
      )
  )
  
  # figure dimensions in mm
  fig_dims = list(width = 180)
  
  # ggplot theme
  # ggplot theme by Jonas Schöley
  MyGGplotTheme <-
    function (
      size = 8,
      family = 'roboto',
      scaler = 1,
      axis = 'x',
      panel_border = FALSE,
      grid = 'y',
      minor_grid = '',
      show_legend = TRUE,
      ar = NA,
      axis_title_just = 'rt',
      axis_ticks = TRUE
    ) {
      
      size_med = size*scaler
      size_sml = round(size*0.7)*scaler
      base_linesize = 0.3*scaler
      
      # justification of axis titles
      xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, 
                   l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
      yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, 
                   l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
      
      list(
        theme_minimal(base_size = size_med, base_family = family),
        theme(
          # basic
          text = element_text(color = 'black'),
          line = element_line(size = base_linesize, lineend = 'square'),
          # axis
          axis.title = element_text(size = size_med, face = 'bold'),
          axis.title.x = element_text(hjust = xj),
          axis.title.y = element_text(hjust = yj),
          axis.title.y.right = element_text(hjust = yj, angle = 90),
          axis.text = element_text(size = size_med, color = 'black'),
          # strips
          strip.text = element_text(color = 'black', size = size_med),
          strip.background = element_blank(),
          # plot
          title = element_text(face = 'bold'),
          plot.subtitle = element_text(color = 'black', size = size_med, face = 'bold'),
          plot.caption = element_text(color = 'black', size = size_sml, face = 'plain'),
          plot.background = element_blank(),
          panel.background = element_blank(),
          #plot.margin = unit(c(1, 0.1, 0.5, 0.5), units = 'mm'),
          # grid
          panel.grid = element_blank()
        ),
        if (isTRUE(axis_ticks)) {
          theme(axis.ticks = element_line(size = rel(0.5), color = 'black'))
        },
        if (identical(grid, 'y')) {
          theme(panel.grid.major.y =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'))
        },
        if (identical(grid, 'x')) {
          theme(panel.grid.major.x =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'))
        },
        if (identical(grid, 'xy') | identical(grid, 'yx')) {
          theme(panel.grid.major.y =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'),
                panel.grid.major.x =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'))
        },
        if (identical(minor_grid, 'y')) {
          theme(panel.grid.minor.y =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'))
        },
        if (identical(minor_grid, 'x')) {
          theme(panel.grid.minor.x =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'))
        },
        if (identical(minor_grid, 'xy') | identical(grid, 'yx')) {
          theme(panel.grid.minor.y =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'),
                panel.grid.minor.x =
                  element_line(size = base_linesize, linetype = 3, color = 'grey80'))
        },
        if (isTRUE(panel_border)) {
          theme(
            panel.border =
              element_rect(fill = NA)
          )
        },
        if (!isTRUE(show_legend)) {
          theme(legend.position = 'none')
        },
        if (axis == 'x') {
          theme(
            axis.line.x = element_line(linetype = 1, color = 'black')
          )
        },
        if (axis == 'y') {
          theme(
            axis.line.y = element_line(linetype = 1, color = 'black')
          )
        },
        if (axis == 'xy') {
          theme(
            axis.line = element_line(linetype = 1, color = 'black')
          )
        },
        if (!is.na(ar)) {
          theme(
            aspect.ratio = ar
          )
        }
      )
    }
  
})


# Global functions figures ----------------------------------------

#' Export ggplot
#' 
#' @author Jonas Schöley
ExportFigure <-
  function(figure,
           path,
           filename,
           width = 170,
           height = 100,
           scale = 1,
           device = 'png',
           dpi = 300,
           add_date = FALSE) {
    require(ggplot2)
    
    if (missing(filename)) {
      filename <- tolower(gsub('\\.', '_', make.names(deparse(substitute(figure)))))
    }
    if (isTRUE(add_date)) {
      filename <- paste0(Sys.Date(), '-', filename)
    }
    
    arguments <-
      list(
        filename = paste0(filename, '.', device),
        plot = figure,
        path = path,
        width = width,
        height = height,
        units = "mm",
        scale = scale,
        dpi = dpi,
        device = device
      )
    if (device == 'pdf') {
      arguments$useDingbats <- FALSE 
    }
    
    do.call(ggsave, arguments)
  }

#' Export ggplots Stored in List
#' 
#' @author Jonas Schöley
ExportFiguresFromList <- function(lst, path, ...) {
  figure_names <- tolower(gsub('\\.+', '_', make.names(names(lst))))
  Fun <- function (figure, filename, ...) {
    ExportFigure(figure = figure, filename = filename, ...)
  }
  purrr::pwalk(
    list(lst, figure_names),
    Fun, path = path, ...
  )
}

# Global functions date -------------------------------------------

#' Create Unique Row ID
#'
#' @param region_iso iso-3166-1 alpha 2 country code with optional
#'   iso-3166-2 region code added, separated by a hyphen.
#' @param sex 'Male' or 'Female'
#' @param age_start Positive Integer.
#' @param year Positive Integer.
#' @param week Positive Integer.
#'
#' @return
#' String with fixed length row ID constructed from input.
#'
#' @examples
#' GenerateRowID('DE-BW', 'Male', 0, 2020, 10)
GenerateRowID <- function(region_iso, sex, age_start, year, week) {
  region_id <- sapply(region_iso, function (x) {
    expanded_region <- '------'
    substr(expanded_region, 1, nchar(x)) <- x
    return(expanded_region)
  })
  sex_id <- as.character(factor(sex, c('Male', 'Female'), c('M', 'F')))
  age_id <- sprintf('%02d', age_start)
  year_id <- sprintf('%04d', year)
  week_id <- sprintf('%02d', week)
  
  row_id <- paste0(region_id, sex_id, age_id, year_id, week_id)
  
  return(row_id)
}

#' Calculate Weeks Since Some Origin Date
#'
#' @param date Date string.
#' @param origin_date Date string.
#' @param week_format Either 'integer' for completed weeks or
#' 'fractional' for completed fractional weeks.
#'
#' @return Time difference in weeks.
#'
#' @author Jonas Schöley
#'
#' @examples
#' # My age in completed weeks
#' WeeksSinceOrigin(Sys.Date(), '1987-07-03')
WeeksSinceOrigin <-
  function(date, origin_date, week_format = "integer") {
    require(ISOweek)
    fractional_weeks_since_origin <-
      as.double(difftime(
        as.Date(date),
        as.Date(origin_date),
        units = "weeks"
      ))
    switch(
      week_format,
      fractional = fractional_weeks_since_origin,
      integer = as.integer(fractional_weeks_since_origin)
    )
  }

#' Convert Week of Year to Date
#'
#' @param year Year integer.
#' @param week Week of year integer (1 to 53).
#' @param weekday Weekday integer (1, Monday to 7, Sunday).
#' @param offset Integer offset added to `week` before date calculation.
#'
#' @return A date object.
#' 
#' @source https://en.wikipedia.org/wiki/ISO_8601
#'
#' @author Jonas Schöley
#'
#' @examples
#' # the first Week of 2020 actually starts Monday, December 30th 2019
#' ISOWeekDateToDate(2020, 1, 1)
ISOWeekDateToDate <- function (year, week, weekday = 1, offset = 0) {
  require(ISOweek)
  isoweek_string <-
    paste0(
      year, '-W',
      formatC(
        week+offset,
        flag = '0',
        format = 'd',
        digits = 1
      ),
      '-', weekday
    )
  ISOweek2date(isoweek_string)
}

#' Convert ISO-weeks to Epi-weeks
#' 
#' @param iso_week ISO-week vector.
#' @param w_start ISO-week for which Epi-week is 0.
#' @param w_53 Assume a 53 week year (default = FALSE)? 
#' 
#' @return A vector of Epi-weeks corresponding to the ISO-week input.
#' 
#' @author Jonas Schöley
#' 
#' @examples
#' # Epi-week 0 aligned with ISO-week 1
#' IsoWeekToEpiWeek(1:52)
#' # Epi-week 0 aligned with ISO-week 10
#' IsoWeekToEpiWeek(1:52, w_start = 10)
#' # Epi-week 0 aligned with ISO-week 10; 53 week year
#' IsoWeekToEpiWeek(1:52, w_start = 10, w_53 = TRUE)
ISOWeekToEpiWeek <- function (iso_week, w_start = 1, w_53 = FALSE) {
  a <- iso_week - w_start
  ifelse(a < 0, 51 + a + 1 + w_53, a)
}

#' Convert Epi-weeks to ISO-weeks
#' 
#' @param epi_week Epi-week vector.
#' @param w_start ISO-week for which Epi-week is 0.
#' @param w_53 Assume a 53 week year (default = FALSE)? 
#' 
#' @return A vector of ISO-weeks corresponding to the Epi-week input.
#' 
#' @author Jonas Schöley
#' 
#' @examples
#' epi_weeks = 0:51
#' # convert to iso week
#' iso_weeks <- EpiWeekToIsoWeek(epi_weeks, w_start = 10)
#' # convert back to epi week
#' IsoWeekToEpiWeek(iso_weeks, w_start = 10)
EpiWeekToIsoWeek <- function (epi_week, w_start = 1, w_53 = FALSE) {
  a <- epi_week + w_start
  ifelse(a > (52 + w_53), a - (52 + w_53), a)
}
