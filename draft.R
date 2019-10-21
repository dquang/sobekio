library(data.table)
library(tidyverse)

#' Plot multiple timeseries together without concerning time
#' 
#' This function plots hydrographs for several IDs and cases together without
#' concerning time. The time will be converted to index
#' 
#' @param case.list List of cases
#' @param case.desc Case naming according to NHWSP Standard
#' @param id.names Names to assign for the IDs
#' @param sobek.project Path to sobek project
#' @param param Waterlevel/Discharge
#' @param color.by Grouping for color. Default by ID
#' @param lt.by Grouping for linetype
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
plot_lines <- function(
  case.list = NULL,
  case.desc = case.list,
  id.names = NULL,
  sobek.project = NULL,
  param = 'discharge',
  color.by = 'variable',
  lt.by = 'vgf',
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
                      # mID = c('p_koeln','p_duesseldorf2'),
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
  for (i in case.list) {
    qt[case == i, ts_id := .I]
  }
  qt[, ts := NULL]
  cols <- colnames(qt[, .SD, .SDcols = -c('ts_id', 'case')])
  qt <- melt(qt, id.vars = c('ts_id', 'case'))
  qt[, value_max := max(value, na.rm = TRUE), by = c('case', 'variable')]
  qt[is.infinite(value_max), value_max := NA]
  qt[value == value_max, ts_peak := as.numeric(ts_id)]
  qt[, ts_peak := max(ts_peak, na.rm = TRUE), by = c('case', 'variable')]
  qt[is.infinite(ts_peak), ts_peak := NA]
  ts_center <- qt[value_max == max(value_max, na.rm = TRUE), ts_peak][1]
  # moving center to only one center of the max
  qt[, ts_id := ts_id + ts_center - ts_peak]
  qt <- merge(qt, case_type, by = 'case', sort = FALSE)
  y1_min <- qt[!variable %in% y2_cols, min(value, na.rm = TRUE)]
  y1_max <- qt[!variable %in% y2_cols, max(value, na.rm = TRUE)]
  y1_pretty <- pretty(y1_min:y1_max, y.ntick, y.ntick)
  if (length(unlist(lt.by)) > 1) {
    qt[, Linientype := do.call(paste, c(.SD, sep = " ")), .SDcols = lt.by]
    lt.by <- 'Linientype'
  }
  if (length(unlist(color.by)) > 1) {
    qt[, Farbe := do.call(paste, c(.SD, sep = " ")), .SDcols = color.by]
    color.by <- 'Farbe'
  }
  # graphic---------------------------------------------------------------------
  g <- ggplot(qt[!variable %in% y2_cols],
              aes(x = ts_id, y = value,
                  color = !!ensym(color.by),
                  # quasiquotation, that's a great option from tidyverse
                  linetype = !!ensym(lt.by)
              )
  ) +
    geom_line(size = 1) +
    theme_bw() +
    theme(
      legend.key.width = unit(2, "cm"),
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
  
  # horizontal lines --------------------------------------------------------
  
  if (!is.null(h.lines)) {
    for (i in seq_along(h.lines)) {
      hline_label <- ifelse(is.null(names(h.lines[i])), h.lines[[i]],
                            names(h.lines[i]))
      g <- g + geom_hline(yintercept = h.lines[[i]], linetype = 2) +
        annotate('text', 
                 x = min(qt$ts_id, na.rm = TRUE), 
                 y = h.lines[[i]], color = 'black',
                 label = hline_label,
                 hjust = 0, vjust = 0)
    }
  }
  g$labels$colour <- color.name
  g$labels$linetype <- lt.name
  if (!is.null(p.caption)) {
    g <- g + labs(caption = p.caption)
  }
  return(g)
}
