#' Return data for one column in a data table with value stored in the column parameter
#' @export
#' @param column Name of the column
#' @param df a data.table or data.frame
#' @param all.lower Should column converted to lower case? Default = FALSE#
#' @param suffix Adding suffix to the column name
#' @param short Get only a given number of character, from left, of the column name
#' @return behave like column subsetting of data.table/data.frame
get_column_by_name <- function(column = NULL, df = NULL, suffix = "",
                             all.lower = F, short = 0L){
  if (all.lower) column <- tolower(column)
  if (is.integer(short) && short > 1) column <- substr(column, 1, short)
  if (!is.data.frame(df)) stop('df must be a data.frame or data.table')
  column_col <- paste(column, suffix, sep = "")
  all_cols <- colnames(df)
  if (!column_col %in% all_cols){
    warning('no column with name: ', column_col, ' in df')
    rel <- NULL
  } else{
    if(is.data.table(df)){
      rel <- df[, get(column_col)]
    } else{
      rel <-  df[, column_col]
    }
  }
  return(rel)
}


#' Plot hydrographs at different locations
#' @param case.list List of cases
#' @param case.desc Case naming according to NHWSP Standard
#' @param id.names Names to assign for the IDs
#' @param sobek.project Path to sobek project
#' @param param Waterlevel/Discharge
#' @param compare.by Grouping the lines by linetype. Default 'zustand'
#' @param facet.by Facetted by, default 'hwe'
#' @param color.name Name of color legend
#' @param lt.name Name of linetype legend
#' @param peak.nday Number of days around the peak to limit to
#' @param peak.col Get peak on this column (id or id.names)
#' @param p.title Title of the plot
#' @param x.lab x-axis title
#' @param y.lab y-axis title
#' @param date.breaks x-axis breaks
#' @param date.labels Formatting of the date on x-axis
#' @param text.x.angle Angle of text on x-axis
#' @param text.size Size of all text
#' @param ... This is the ID parameter to be transferred to his_from_case function. It is normally idType = idList
#' @return A ggplot2 graphic
#' @export
#' @import data.table
plot_multi_lines <- function(
  case.list = NULL,
  case.desc = case.list,
  id.names = NULL,
  sobek.project = NULL,
  param = 'discharge',
  compare.by = 'zustand',
  facet.by = 'hwe',
  color.name = 'Farbe',
  lt.name = 'Linienart',
  peak.nday = NULL,
  peak.col = NULL,
  p.title = 'Ganglinien an der Orten',
  x.lab = 'Zeit',
  y.lab = NULL,
  date.breaks = '3 days',
  date.labels = "%d.%m.%Y",
  text.x.angle = 90L,
  text.size = 12L,
  ...
){
  stopifnot(!is.null(case.list), !is.null(sobek.project))
  case_type <- parse_case(case.desc = case.desc, orig.name = case.list)
  if (is.null(y.lab)){
    y.lab = switch(tolower(param),
                   'Value',
                   discharge = 'Abfluss m³/s',
                   waterlevel = 'Wasserstand (m+NHN)',
                   'crest level' = 'Crest level (m+NHN)'
                   )
  }
  qt <- his_from_case(case.list = case.list, sobek.project = sobek.project,
                      # mID = c('p_koeln','p_duesseldorf'),
                      param = param,...)
  if (!is.null(id.names)){
    if (length(id.names) == ncol(qt) - 2){
      colnames(qt) <- c('ts', id.names, 'case')
    } else{
      warning("id.names is not same length as id.list. Names were not changed")
    }
  }
  # cut table to peak.nday
  if(!is.null(peak.nday)){
    cols <- colnames(qt[, .SD, .SDcols = -c('ts', 'case')])
    if (is.null(peak.col)){
      qt[, value_max := max(.SD, na.rm = TRUE), .SDcols = -c('ts'), by = case]
      for (col_name in cols){
        qt[get(eval(col_name)) == value_max, ts_peak := ts]
      }
    } else{
      if (peak.col %in% cols){
        qt[, value_max := max(.SD, na.rm = TRUE),
           .SDcols = peak.col, by = case]
        qt[get(eval(peak.col)) == value_max, ts_peak := ts]
      } else {
        warning('There is no column with name: "', peak.col, '" in the data table',
                '. The peak is peak of column that has the max value')
        qt[, value_max := max(.SD, na.rm = TRUE), .SDcols = -c('ts'), by = case]
        for (col_name in cols){
          qt[get(eval(col_name)) == value_max, ts_peak := ts]
        }
      }
    }
    qt[, ts_peak := min(ts_peak, na.rm = TRUE), by = case]
    qt[, ts_min := ts_peak - peak.nday * 24 * 3600]
    qt[, ts_max := ts_peak + peak.nday * 24 * 3600]
    qt <- qt[ts >= ts_min & ts <= ts_max]
    qt[, c('ts_min', 'ts_max', 'ts_peak', 'value_max') :=
         list(rep(NULL, 4))]
  }
  # data transformation for graphic
  qt <- melt(qt, id.vars = c('ts', 'case'))
  qt[, variable := str_replace_all(variable, "_", " ")]
  qt[, variable := str_to_title(variable)]
  qt[, variable := str_replace_all(variable, " ", "_")]
  qt <- merge(qt, case_type, by = 'case', sort = FALSE)
  # make graphic
  g <- ggplot(qt,
              aes(x = ts, y = value,
                  color = variable,
                  # quasiquotation, that's a great option from tidyverse
                  linetype = !!ensym(compare.by)
              )
  ) +
    scale_x_datetime(
      date_breaks = date.breaks,
      date_labels = date.labels
    ) +
    geom_line(size = 1) +
    theme_bw()+
    theme(
      legend.position = 'bottom',
      text = element_text(
        size = text.size
      ),
      axis.text.x = element_text(
        angle = text.x.angle
      )
    ) +
    ggtitle(p.title)+
    xlab(x.lab) + ylab(y.lab)
  if (!is.null(facet.by)){
    g <- g + facet_grid(. ~ get(facet.by), scales = 'free_x')
  }
  g$labels$colour <- color.name
  g$labels$linetype <- lt.name
  g
}
