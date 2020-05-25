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
#' @param facet.scale Scale for facetting, default 'free'
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
#' @param input.data a data.table as input, instead of reading from sobek
#' @param p.caption Caption for the graphic. The IDs that were moved to second axis will have a small start (*) at the end of their names, a caption is about to explain that.
#' @param line.size Size for the lines
#' @param ... This is the ID parameter to be transferred to his_from_case function. It is normally idType = idList
#' @return A ggplot2 graphic
#' @export
#' @import data.table
plot_multi_lines <- function(
  case.list,
  case.desc = case.list,
  id.names = NULL,
  sobek.project,
  param = 'discharge',
  compare.by = 'zustand',
  group.by = compare.by,
  facet.by = NULL,
  facet.scale = 'free',
  delta = FALSE,
  color.name = 'Farbe',
  lt.name = 'Linienart',
  peak.nday = NULL,
  peak.col = NULL,
  p.title = ggplot2::waiver(),
  x.lab = 'Zeit',
  y.lab = NULL,
  y.ntick = 7L,
  y2.ids = NULL,
  y2.scale = 5,
  y2.tick1 = 0,
  y2.lab = y.lab,
  input.data = NULL,
  p.caption = ggplot2::waiver(),
  date.breaks = '3 days',
  date.labels = "%d.%m.%Y",
  text.x.angle = 90L,
  text.size = 20L,
  text.tbl.size = 5L,
  h.lines = NULL,
  line.size = 1.3,
  legend.nrow = 2,
  col_values = col_vibrant,
  ...
){
  eval({
    p.caption
    p.title
    })
  for (a_k in c(compare.by, group.by)) {
    if (!a_k %in% c("zustand", "zielpegel", "hwe", "vgf", "notiz")) {
      stop("group.by and compare.by must be a single or combination value of ",
           paste("zustand", "zielpegel", "hwe", "vgf", "notiz", sep = ", "))
    }
  }
  if (!is.null(y2.scale)) stopifnot(isTRUE(y2.scale > 0))
  if (!is.null(y2.ids)) {
    if (!is.numeric(y2.ids)) {
      stop('y2.ids must be a vector of intergers, indicating which IDs (by their indexes) should be moved to the second y-axis, not: ', typeof(y2.ids))
    }
    y2.ids <- y2.ids + 1 # the first column is 'ts'
  }
  if (!is.null(input.data)) {
    if (missing(case.list)) case.list <- unique(input.data$case)
    if (missing(case.desc)) case.desc <- case.list
  }
  cmp_grp_equal <- identical(sort(compare.by), sort(group.by))
  if (cmp_grp_equal) group.by <- compare.by
  case_type <- parse_case(case.desc = case.desc, orig.name = case.list)
  case_type[, compare__by := do.call(paste0, .SD), .SDcols = compare.by]
  case_type[, group__by := do.call(paste0, .SD), .SDcols = group.by]
  if (!is.null(facet.by)) {
    case_type[, facet__by := do.call(paste0, .SD), .SDcols = facet.by]
  } else {
    case_type[, facet__by := NA]
  }
  if (delta) {
    # check if compare.by and group.by make unique combination to case
    n_case <- length(unlist(case.list))
    cmp_vars <- case_type[, unique(compare__by)]
    grp_vars <- case_type[, unique(group__by)]
    stopifnot(length(cmp_vars) == 2)
    if (!is.null(facet.by)) {
      n_grp <- length(case_type[, unique(paste0(compare__by, group__by, facet__by))])
    } else {
      n_grp <- length(case_type[, unique(paste0(compare__by, group__by))])
    }
    # if (cmp_grp_equal) n_grp <- n_grp / 2
    stopifnot(n_case == n_grp)
  }
  if (is.null(y.lab)) {
    y.lab = switch(tolower(param),
                   'Value',
                   discharge = 'Abfluss [m³/s]',
                   waterlevel = 'Wasserstand [m+NHN]',
                   'crest level' = 'Crest level [m+NHN]'
                   )
  }
  if (is.null(input.data)) {
    qt <- his_from_case(case.list = case.list,
                        sobek.project = sobek.project,
                        ...,
                        param = param
    )
  } else {
    stopifnot(is.data.frame(input.data))
    if (is.data.table(input.data)) {
      qt <- copy(input.data)
    } else {
      qt <- as.data.table(input.data)
    }
  }
  if (!is.null(id.names)) {
    if (length(id.names) == ncol(qt) - 2) {
      setcolorder(qt, c("ts", "case"))
      colnames(qt) <- c('ts', 'case', id.names)
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
  lt_vars <- unique(c(compare.by, group.by))
  qt[, Linetype := do.call(paste0, .SD), .SDcols = lt_vars]
  y1_min <- qt[!variable %in% y2_cols, min(value, na.rm = TRUE)]
  y1_max <- qt[!variable %in% y2_cols, max(value, na.rm = TRUE)]
  y1_pretty <- pretty(y1_min:y1_max, y.ntick, y.ntick)

# Delta -------------------------------------------------------------------
  if (delta) {
    qt_max <- qt[, max(value, na.rm = TRUE), by = c("variable", "case")]
    qt_delta <- merge(qt_max,
                      case_type[, c("case", "compare__by", "group__by", "facet__by")],
                      by = "case", sort = FALSE)
    if (cmp_grp_equal) {
      qt_delta <- dcast(qt_delta, facet__by + variable ~ compare__by,
                        value.var = "V1")
      qt_delta[, group__by := NA]
    } else {
      qt_delta <- dcast(qt_delta, facet__by + variable + group__by ~ compare__by,
                        value.var = "V1")
    }
    qt_delta[, Delta := get(cmp_vars[2]) - get(cmp_vars[1])]
    n_round <- ifelse(param == "discharge", 0, 2)
    qt_delta[, Delta := round(Delta, n_round)]
    delta_col_nm <- ifelse(param == "discharge", "Delta [m³/s]", "Delta [m]")
    qt_delta <- qt_delta[, c("variable", "group__by", "Delta", "facet__by")]
    colnames(qt_delta)[1:3] <- c("Standort", "Group", delta_col_nm)
    lage_lvl <- levels(qt_delta$Standort)
    n_char_max <- max(nchar(as.character(qt_delta$Standort)), na.rm = TRUE)
    qt_delta[, Standort := stri_pad_right(str_trim(Standort), width = n_char_max)]
    lage_lvl <- stri_pad_right(str_trim(lage_lvl), width = n_char_max)
    qt_delta[, Standort := factor(Standort, lage_lvl)]
    setorder(qt_delta, Group, Standort)
  }

  # graphic---------------------------------------------------------------------
  g <- ggplot(qt[!variable %in% y2_cols],
              aes(x = ts, y = value,
                  color = variable,
                  linetype = Linetype
              )
  ) +
    scale_x_datetime(
      date_breaks = date.breaks,
      date_labels = date.labels,
      expand = expansion(0.02)
    ) +
    geom_line(size = line.size) +
    theme_bw(base_size = text.size) +
    theme(
      title = element_text(size = text.size),
      text = element_text(size = text.size),
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(0.9, "cm"),
      legend.position = 'bottom',
      panel.grid.major = element_line(size = 0.3, color = "grey60"),
      panel.grid.minor = element_line(size = 0.15, color = "grey"),
      axis.text = element_text(size = text.size - 2, angle = text.x.angle),
      axis.title = element_text(size = text.size -2, angle = text.x.angle),
      strip.text = element_text(size = text.size)
    ) +
    labs(x = x.lab, y = y.lab, title =  p.title, caption = p.caption) +
    scale_y_continuous(breaks = y1_pretty)
  if (length(y2_cols) > 0) {
    delta <- FALSE
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
    g <- g + geom_line(data = qt[variable %in% y2_cols], size = line.size,
                       mapping = aes(y = value * y2.scale + y2_shift)) +
      scale_y_continuous(
        breaks = y1_pretty,
        sec.axis =
          sec_axis(trans = ~./y2.scale - y2_shift/y2.scale,
                   breaks = y2_pretty,
                   name = y2.lab)
      )
  }
  if (delta) {
    if (!is.null(facet.by)) {
      tbl_lst <- list()
      for (a_facet in unique(qt_delta$facet__by)) {
        tbl_lst[[a_facet]] <- dplyr::filter(qt_delta, facet__by == a_facet)
        tbl_lst[[a_facet]][["facet__by"]] <- NULL
        rm_grp <- length(unique(tbl_lst[[a_facet]][["Group"]])) == 1
        if (rm_grp) tbl_lst[[a_facet]][["Group"]] <- NULL
      }
    } else {
      tbl_lst <- list(qt_delta)
      rm_grp <- length(unique(tbl_lst[[1]][["Group"]])) == 1
      if (rm_grp) tbl_lst[[1]][["Group"]] <- NULL
      tbl_lst[[1]][["facet__by"]] <- NULL
    }
    qt_lbl <- dplyr::tibble(
      x = rep(-Inf, length(tbl_lst)),
      y = rep(Inf, length(tbl_lst)),
      facet__by = unique(qt_delta$facet__by),
      lbl = tbl_lst
    )
    g <- g + ggpmisc::geom_table_npc(aes(npcx = x, npcy = y),
                            size = text.tbl.size,
                            label = tbl_lst,
                            data = qt_lbl)
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
                data = data_hline, color = 'black', check_overlap = TRUE,
                hjust = 0, vjust = 0)
  }
  if (!is.null(facet.by)) {
    # lt_vars <- unique(c(compare.by, group.by))
    # lt_vars <- lt_vars[!grepl(facet.by, lt_vars)]
    # qt[, Linetype := do.call(paste, .SD), .SDcols = lt_vars]
    # lt_values <- unique(qt[, c("Linetype", "facet__by")])
    # lt_values[, grp := 1:.N, by = facet__by]
    # ltv <- vector()
    # for (i in seq_along(lt_values$Linetype)) ltv[[lt_values$Linetype[i]]] <- lt_values$grp[i]
    g <- g + facet_grid(cols = vars(facet__by), scales = facet.scale)
  }
  g <- g + guides(
    color = guide_legend(title = color.name, nrow = legend.nrow),
    linetype = guide_legend(title = lt.name, nrow = legend.nrow)
  )
  if (!is.null(col_values)) g <- g + scale_color_manual(values = col_values)
  return(g)
}


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
#' @param color.by Giving this parameter one ore more variables for coloring the lines.
#' It is by default 'variable', meaning coloring by ID. Accept grouping for examples:
#' c('hwe', 'vgf)
#' @param lt.by Giving this parameter one ore more variables for making the linetypes of the lines.
#' It is by default 'hwe'. Accept grouping for examples: c('hwe', 'vgf)
#' @param color.name Name of color legend
#' @param lt.name Name of linetype legend
#' @param peak.nday Number of days around the peak to limit to
#' @param peak.col Get peak on this column (id or id.names)
#' @param p.title Title of the plot
#' @param x.lab x-axis title
#' @param y.lab y-axis title
#' @param text.size Size of all text
#' @param h.lines List of (name = value) to be displayed as horizontal line
#' @param y2.ids List of IDs, by their indexes, not names, to move to second y-axis
#' @param y.ntick Numbers of y-axis intervals. Acutally n-tick = n-interval - 1
#' @param y2.scale Scale for second axis,
#' @param y2.tick1 First value of the y2-axis, use this together with y2.scale to make it looks nice
#' @param y2.lab Label for y2-axis, default = y.lab
#' @param p.caption Caption for the graphic. The IDs that were moved to second axis will have a small start (*) at the end of their names, a caption is about to explain that.
#' @param line.size Size for the lines
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
  lt.by = 'hwe',
  color.name = 'Farbe',
  lt.name = 'Linienart',
  peak.nday = NULL,
  peak.col = NULL,
  p.title = 'Ganglinien an der Orten',
  x.lab = NULL,
  y.lab = NULL,
  y.ntick = 7L,
  y2.ids = NULL,
  y2.scale = 5,
  y2.tick1 = 0,
  y2.lab = y.lab,
  p.caption = NULL,
  text.size = 12L,
  h.lines = NULL,
  line.size = 1.0,
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
                   discharge = 'Abfluss [m³/s]',
                   waterlevel = 'Wasserstand [m+NHN]',
                   'crest level' = 'Crest level [m+NHN]'
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
  y1_length <- y1_max - y1_min
  if (y1_length < 10) {
    y1_min_1 <- y1_min * 100
    y1_max_1 <- y1_max * 100
    y1_pretty <- pretty(y1_min_1:y1_max_1, y.ntick, y.ntick)
    y1_pretty <- y1_pretty / 100
  }
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
    geom_line(size = line.size) +
    theme_bw(base_size = text.size) +
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = 'bottom',
      text = element_text(
        size = text.size
      ),
      axis.text.x = element_blank()
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
