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
#' 
#' This function plots hydrographs for several IDs and cases
#' 
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
#' @param h.lines List of (name = value) to be displayed as horizontal line
#' @param y2.ids List of IDs, by their indexes, not names, to move to second y-axis
#' @param y.ntick Numbers of y-axis intervals. Acutally n-tick = n-interval - 1
#' @param y2.scale Scale for second axis,
#' @param y2.tick1 First value of the y2-axis, use this together with y2.scale to make it looks nice
#' @param y2.lab Label for y2-axis, default = y.lab
#' @param p.caption Caption for the graphic. The IDs that were moved to second axis will have a small start (*) at the end of their names, a caption is about to explain that.
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
  # group.by = 'vgf',
  facet.by = 'hwe',
  color.name = 'Farbe',
  lt.name = 'Linienart',
  peak.nday = NULL,
  peak.col = NULL,
  p.title = 'Ganglinien an der Orten',
  x.lab = 'Zeit',
  y.lab = NULL,
  y.ntick = 7L,
  y2.ids = NULL,
  y2.scale = 5,
  y2.tick1 = 0,
  y2.lab = y.lab,
  p.caption = NULL,
  date.breaks = '3 days',
  date.labels = "%d.%m.%Y",
  text.x.angle = 90L,
  text.size = 12L,
  h.lines = NULL,
  ...
){
  stopifnot(!is.null(case.list), !is.null(sobek.project))
  if (!is.null(y2.scale)) stopifnot(isTRUE(y2.scale > 0))
  if (!is.null(y2.ids)) {
    if (!is.numeric(y2.ids)) {
      stop('y2.ids must be a vector of intergers, indicating which IDs (by their indexes) should be moved to the second y-axis, not: ', typeof(y2.ids))
    }
    y2.ids <- y2.ids + 1 # the first column is 'ts'
  }

  case_type <- parse_case(case.desc = case.desc, orig.name = case.list)
  if (is.null(y.lab)) {
    y.lab = switch(tolower(param),
                   'Value',
                   discharge = 'Abfluss m³/s',
                   waterlevel = 'Wasserstand (m+NHN)',
                   'crest level' = 'Crest level (m+NHN)'
                   )
  }
  qt <- his_from_case(case.list = case.list, 
                      sobek.project = sobek.project,
                      ...,
                      # mID = c('p_worms','p_duesseldorf'),
                      param = param
                      )
  if (!is.null(id.names)) {
    if (length(id.names) == ncol(qt) - 2) {
      colnames(qt) <- c('ts', id.names, 'case')
    } else{
      warning("id.names is not same length as id.list. Names were not changed")
    }
  }
  y2_cols <- colnames(qt)[y2.ids]
  # cut table to peak.nday
  if (!is.null(peak.nday)) {
    cols <- colnames(qt[, .SD, .SDcols = -c('ts', 'case')])
    if (is.null(peak.col)) {
      qt[, value_max := max(.SD, na.rm = TRUE), .SDcols = cols, by = case]
      for (col_name in cols) {
        qt[get(eval(col_name)) == value_max, ts_peak := ts]
      }
    } else{
      if (peak.col %in% cols) {
        qt[, value_max := max(.SD, na.rm = TRUE), .SDcols = peak.col, by = case]
        qt[get(eval(peak.col)) == value_max, ts_peak := ts]
      } else {
        warning('There is no column with name: "', peak.col, '" in the data table',
                '. The peak is peak of column that has the max value')
        qt[, value_max := max(.SD, na.rm = TRUE), .SDcols = cols, by = case]
        for (col_name in cols) {
          qt[get(eval(col_name)) == value_max, ts_peak := ts]
        }
      }
    }
    qt[, ts_peak := max(ts_peak, na.rm = TRUE), by = case]
    qt[, ts_min := ts_peak - peak.nday * 24 * 3600]
    qt[, ts_max := ts_peak + peak.nday * 24 * 3600]
    qt <- qt[ts >= ts_min & ts <= ts_max]
    qt[, c('ts_min', 'ts_max', 'ts_peak', 'value_max') :=
         list(rep(NULL, 4))]
  }
  # data transformation for graphic
  qt <- melt(qt, id.vars = c('ts', 'case'))
  qt <- merge(qt, case_type, by = 'case', sort = FALSE)
  y1_min <- qt[!variable %in% y2_cols, min(value, na.rm = TRUE)]
  y1_max <- qt[!variable %in% y2_cols, max(value, na.rm = TRUE)]
  y1_pretty <- pretty(y1_min:y1_max, y.ntick, y.ntick)
  # graphic---------------------------------------------------------------------
  g <- ggplot(qt[!variable %in% y2_cols],
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
    theme_bw() +
    theme(
      legend.position = 'bottom',
      text = element_text(
        size = text.size
      ),
      axis.text.x = element_text(
        angle = text.x.angle
      )
    ) +
    ggtitle(p.title) +
    xlab(x.lab) + ylab(y.lab) +
    scale_y_continuous(breaks = y1_pretty)
  if (length(y2_cols) > 0) {
    if (is.null(y2.tick1)) {
      y2_shift <- y1_pretty[1]
    } else {
      y2_shift <- y1_pretty[1]  - y2.tick1 * y2.scale
    }
    # recalculate y1_min, y1_max to cover min, max range from both y-axes
    y2_max <- qt[variable %in% y2_cols, max(value, na.rm = TRUE)]
    y2_min <- qt[variable %in% y2_cols, min(value, na.rm = TRUE)]
    y1_min <- min(y1_min, y2_min * y2.scale + y2_shift, unlist(h.lines), na.rm = TRUE)
    y1_max <- max(y1_max, y2_max * y2.scale + y2_shift, unlist(h.lines), na.rm = TRUE)
    y1_pretty <- pretty(y1_min:y1_max, y.ntick, y.ntick)
    y2_pretty <- (y1_pretty - y2_shift) / y2.scale
    qt[variable %in% y2_cols, variable := paste(variable, '(*)') ]
    y2_cols <- paste(y2_cols, '(*)')
    g <- g + geom_line(data = qt[variable %in% y2_cols], size = 1,
                       mapping = aes(y = value * y2.scale + y2_shift)) +
      scale_y_continuous(
        breaks = y1_pretty,
        sec.axis =
          sec_axis(trans = ~./y2.scale - y2_shift/y2.scale,
                   breaks = y2_pretty,
                   #labels = round(y2_pretty, 2),
                   name = y2.lab)
      )
  }

# horizontal lines --------------------------------------------------------

  if (!is.null(h.lines)) {
    qt[, ts_min := min(ts), by = case]
    data_hline <- qt[ts == ts_min]
    for (i in seq_along(h.lines)) {
      new_col <- paste('hline', i, names(h.lines)[i], sep = "_")
      data_hline[, eval(new_col) := h.lines[[i]]]
      g <- g + geom_hline(yintercept = h.lines[[i]], linetype = 2)
    }
    data_hline <- melt(data_hline, 
                       measure.vars = patterns("hline_"),
                       variable.name = 'labels',
                       value.name = 'hlines'
                       )
    data_hline[, labels := str_replace(labels, 'hline_\\d_', '')]
    g <- g + 
      geom_text(aes(x = ts_min, y = hlines, label = labels), 
                data = data_hline, color = 'black',
                hjust = 0, vjust = 0)
  }
  if (!is.null(facet.by)) {
    g <- g + facet_wrap(c(facet.by), scales = 'free')
  }
  g$labels$colour <- color.name
  g$labels$linetype <- lt.name
  if (!is.null(p.caption)) {
    g <- g + labs(caption = p.caption)
  }
  return(g)
}
