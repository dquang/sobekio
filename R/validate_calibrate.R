#' Plot typical validation graphic for one Pegel
#' 
#' @param case.name Name of the case
#' @param ... ID_TYPE = ID
#' @param Parameter
#' @param sobek.project Path to sobek project
#' @param messung Table of measured values
#' @param date.breaks ggplot2 date_breaks
#' @param date.labels ggplot2 date_labels
#' @param x.lab x axis label
#' @param y.lab y axis label
#' @param xaxis.text element_text for x axis
#' @export
validate_pegel <- function(
  case.name,
  ...,
  param = 'discharge',
  sobek.project,
  messung,
  date.breaks = '1 day',
  date.labels = '%d.%m.%y',
  peak.nday = 10,
  x.lab = 'Zeit',
  y.lab = ifelse(param == 'discharge', 'Abfluss (m³/s)', 'Wasserstand (m+NHN)'),
  yaxis.text = element_text(size = 10),
  xaxis.text = element_text(angle = 90, size = 10)
) {
  mod_ts <- his_from_case(
    case.list = case.name,
    sobek.project = sobek.project,
    param = param,
    ...
    )
  mod_ts$case <- NULL
  colnames(mod_ts) <- c('ts', 'Modell')
  colnames(messung) <- c('ts', 'Messung')
  data_tbl <- merge(mod_ts, messung, by = 'ts', all.x = TRUE)
  data_tbl[, Delta := Modell - Messung]
  data_tbl <- melt(data_tbl, id.vars = 'ts')
  ts_peak <- data_tbl[value == max(value, na.rm = TRUE), ts]
  ts_min <- ts_peak - 3600 * 24 * peak.nday
  ts_max <- ts_peak + 3600 * 24 * peak.nday
  data_tbl <- data_tbl[ts >= ts_min & ts <= ts_max]
  y1_pretty <- pretty(data_tbl[variable != 'Delta', value])
  y1_pretty_delta <- pretty(data_tbl[variable == 'Delta', value])
  g1 <- ggplot(data_tbl[variable != 'Delta'], aes(x = ts, y = value, color = variable)) +
    scale_x_datetime(date_breaks = date.breaks, date_labels = date.labels) +
    xlab(x.lab) +
    ylab(y.lab) + 
    geom_line(size = 1) + 
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = 'bottom',
      axis.text.x.bottom = xaxis.text,
      axis.text.y = yaxis.text
    ) + 
    scale_y_continuous(breaks = y1_pretty, limits = range(y1_pretty))
  g1$labels$colour <- 'Farbe'
  g2 <- ggplot(data_tbl[variable == 'Delta'], aes(x = ts, y = value)) +
    scale_x_datetime(date_breaks = date.breaks, date_labels = date.labels) +
    xlab(x.lab) +
    ylab('Delta') + 
    geom_line(size = 1) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    annotate('rect', ymin = -200, ymax = 200,
              xmin = min(data_tbl$ts),
              xmax = max(data_tbl$ts),
              fill = 'red', alpha = 0.2) +
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = 'bottom',
      axis.text.x = element_blank(),
      axis.text.y = yaxis.text
    ) +
    scale_y_continuous(breaks = y1_pretty_delta, limits = range(y1_pretty_delta))
  g <- cowplot::plot_grid(g1, g2, align = 'v', axis = 'l', nrow = 2,
                          rel_heights = c(0.7, 0.3))
  g
}
