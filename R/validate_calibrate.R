#' Plot typical validation graphic for one Pegel
#'
#' @param case.name Name of the case
#' @param ... ID_TYPE = ID
#' @param param Parameter
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


#' Plot and compare hydrograph lines at a location between two cases
#'
#' This functions read data from sobek model at make plot the hydrograph and also the delta line between them
#'
#' @inheritParams plot_longprofile
#' @param lt DESCRIPTION.
#' @param color DESCRIPTION.
#'
#' @export
#' @return ggplot
plot_pegel <- function(
  case.list,
  case.desc = case.list,
  compare.by = "zustand",
  group.by = compare.by,
  lt.by = NULL,
  color.by = "variable",
  color.name = "Farbe",
  lt.name = "Linetype",
  facet.by = NULL,
  facet.scale = 'free',
  delta = TRUE,
  y2.scale = NULL,
  y2.shift = NULL,
  x.lab = "Zeit",
  y.lab = NULL,
  p.caption = ggplot2::waiver(),
  p.title = ggplot2::waiver(),
  date.breaks = '1 month',
  date.labels = "%b",
  sobek.project,
  param = "discharge",
  input.data = NULL,
  keep.data = FALSE,
  lt.size = 1.3,
  lt = NULL,
  color = col_bright_lines,
  txt.size = 20,
  txt.x.angle = 0,
  txt.font.fam = "serif",
  ...,
  id.names = NULL
) {
  eval({
    p.title
    p.caption
    lt.by
    color.by
  })
  if (is.null(y.lab)) y.lab <- ifelse(param == "discharge",
                                       "Ablfuss [m³/s]",
                                       "Wasserstand [m + NHN]")
  stopifnot(length(case.desc) == length(unique(case.desc)))
  n_case <- length(case.desc)
  if (is.null(input.data)) {
    tbl <- his_from_case(case.list = case.list,
                         case.desc = case.desc,
                         sobek.project = sobek.project,
                         ...,
                         param = param)
  } else {
    tbl <- copy(input.data)
  }
  if (keep.data && is.null(input.data)) tbl_org <- copy(tbl)
  tbl <- melt(tbl, id.vars = c("ts", "case"))
  if (n_case > 2) {
    case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
    case_tbl[, case := case_desc][, case_desc := NULL]
    case_tbl[, compare__by := do.call(paste, c(.SD, sep = " - ")),
              .SDcols = compare.by]
    case_tbl[, group__by := do.call(paste, c(.SD, sep = " - ")),
              .SDcols = group.by]
    cmp_vars <- case_tbl[, unique(compare__by)]
    grp_vars <- case_tbl[, unique(group__by)]
    if (!is.null(facet.by)) {
      case_tbl[, facet__by := do.call(paste, c(.SD, sep = " - ")),
                .SDcols = facet.by]
    } else {
      case_tbl[, facet__by := NA]
    }
    tbl <- merge(tbl, case_tbl, by = "case")
  } else {
    tbl[, compare__by := case]
    tbl[, group__by := case]
    cmp_vars <- case.desc
    grp_vars <- case.desc
    tbl[, c("facet__by", "zustand", "zielpegel", "vgf", "notiz") :=
          as.list(rep(NA, 5))]
    if (!is.null(facet.by)) tbl[, facet__by := case]
  }
  if (is.null(lt.by)) lt.by <- "group__by"
  if (delta && n_case > 1) {
    if (n_case == 2) {
      delta_tbl <- dcast(tbl, ts ~ case, value.var = "value")
      delta_tbl$Delta  <- delta_tbl[[cmp_vars[2]]] - delta_tbl[[cmp_vars[1]]]
      tbl <- merge(tbl, delta_tbl[, c("ts", "Delta")], by = "ts")
    } else {
      delta_tbl <- dcast(tbl, ts ~ group__by + compare__by, value.var = "value")
      for (a_grp in grp_vars) {
        col_1 <- paste(cmp_vars[1], a_grp, sep = '_')
        col_2 <- paste(cmp_vars[2], a_grp, sep = '_')
        delta_tbl$Delta  <- delta_tbl[[col_2]] - delta_tbl[[col_1]]
        delta_tbl[, eval(col_1) := NULL]
        delta_tbl[, eval(col_2) := NULL]
      }
      delta_tbl <- melt(delta_tbl, id.vars = 'ts',
                             variable.name = "group__by",
                             value.name = 'Delta',
                             sort = FALSE)
      tbl <- melt(tbl, delta_tbl, by = c("ts", "group__by"))
    }
  }

# graphic ----------------------------------------------------------------
  txt_def <- element_text(size = txt.size, family = txt.font.fam, hjust = 0.5)
  txt_def_angle <- element_text(size = txt.size, family = txt.font.fam,
                                angle = txt.x.angle)
  g <- ggplot() +
    scale_x_datetime(date_breaks = date.breaks,
                     date_labels = date.labels,
                     name = x.lab) +
    geom_line(aes(x = ts, y = value,
                  color = !!ensym(color.by),
                  linetype = !!ensym(lt.by)),
              tbl,
              size = lt.size
              ) +
    theme_bw(base_size = txt.size) +
    theme(
      title = txt_def,
      text = txt_def,
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(0.9, "cm"),
      legend.position = 'bottom',
      panel.grid.major = element_line(size = 0.3, color = "grey60"),
      panel.grid.minor = element_line(size = 0.15, color = "grey"),
      axis.text.y = txt_def,
      axis.text.x = txt_def_angle,
      axis.title = txt_def,
      legend.text = txt_def,
      axis.title.x = txt_def,
      axis.title.y = txt_def,
      strip.text = txt_def
    )
  if (delta) {
    if (!is.null(y2.scale)) {
      ggb <- ggplot_build(g)
      y_range <- range(ggb$layout$panel_params[[1]]$y.range)
      y_breaks <- pretty(y_range, 5, 5)
      y_breaks <- y_breaks[-length(y_breaks)]
      y_range <- range(pretty(y_range, 5, 5))
      y2_range <- tbl[, range(Delta, na.rm = TRUE)]
      y_length <- abs(y_range[2] - y_range[1])
      y2_length <- abs(y2_range[2] - y2_range[1])
      y2_scale_g <- pretty(y_length / y2_length)[1]
      if (y2_scale_g <= 0) y2_scale_g <- 1
      if (tolower(y2.scale) == "auto") y2.scale <- y2_scale_g / 2
      if (is.null(y2.shift)) y2.shift = y_breaks[2]
      tbl[, Delta := Delta * y2.scale + y2.shift]
      y2_name <- ifelse(param == "discharge",
                        "Differenz [m³/s]",
                        "Differenz [m]")
      g <- g + geom_line(
        mapping = aes(x = ts, y = Delta, color = "Delta", linetype = "Delta"),
        data = tbl[grepl(cmp_vars[1], compare__by)],
        size = lt.size
      ) +
        scale_y_continuous(
          breaks = y_breaks,
          # limits = y_range,
          sec.axis = dup_axis(trans = ~ (. - y2.shift) / y2.scale,
                              name = y2_name,
                              breaks = (y_breaks - y2.shift) / y2.scale),
          labels = function(x) stri_replace_all_fixed(as.character(x), ".", ",")
        )
    } else {
      g <- g + geom_line(
        mapping = aes(x = ts, y = Delta, color = "Delta", linetype = "Delta"),
        data = tbl[compare__by == cmp_vars[1]],
        size = lt.size
      ) +
        scale_y_continuous(
          labels = function(x) stri_replace_all_fixed(as.character(x), ".", ",")
        )
    }
  } else {
    g <- g + scale_y_continuous(
      labels = function(x) stri_replace_all_fixed(as.character(x), ".", ",")
    )
  }
  if (!is.null(lt)) {
    g <- g + scale_linetype_manual(values = lt)
    if (length(unique(lt) == 1)) g <- g + guides(linetype = FALSE)
  }
  if (!is.null(color)) g <- g + scale_color_manual(values = color)
  g <- g +
    labs(x = x.lab,
         y = y.lab,
         title =  p.title,
         color = color.name,
         linetype = lt.name,
         caption = p.caption)
}

