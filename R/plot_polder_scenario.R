#' Plot hydrographs for a measure with comparing by scenario (zustand)
#' @param name Name of the measure (with/without the measure)
#' @param case.list List of cases
#' @param case.desc Correct (according to case naming standard in NHWSP) version of case.list, it will be used for legend
#' @param sobek.project Path to sobek project
#' @param param 'waterlevel', 'discharge', or 'both'.
#' If both, two graphics will be first procedured and then arrange vertically together
#' @param q.in Logical. Should discharge through the inlet be plotted?
#' @param q.out Logical. Should discharge through the outlet be plotted?
#' @param w.canal Logical. Should Wt line inside the measure be plotted?
#' @param ref.mID ID of Bezugspegel
#' @param y2.scale Scaling between main and secondary y-axes.
#' This is an important paramter. If the line for secondary axis is too big, try to change y2.scale
#' y2.scale with be automatically calculated so that the height of the graph on y2-axis is about 3/4 those on y1-axis
#' @param y2.tick1 The value of the first tick on the y2-axis. Using this and y2.scale to make y2-axis looks nice.
#' @param h.lines list of (name = value) to be displayed as horizontal line
#' @param peak.nday Should the plot limit to nday before and after the peak. Default is not (NULL). Otherwise please give a number of days
#' @param peak.pegel If the plot should be limit to the peak area, should it be the peak of the referenced location (ref.mID). Default is not.
#' @param delta.pegel Logical. Should peak delta at ref.mID be displayed
#' @param delta.measure Logical. Should peak delta at the measure be displayed
#' @param delta.line Logical. Should Delta lines be plotted extra, beneath the main plot? Default is FALSE.
#' @param rel.height Relative size of the main and the Delta plot. Default c(2, 0.7)
#' @param compare.by Should the line be compare by 'case' or by 'location'
#' @param cmp.sort Should comparing parameter be sorted. Default is FALSE
#' @param group.by Groupping for delta calculation
#' @param plot.title Title of the plot
#' @param lt.name Name of the linetype legend
#' @param color.name Name of the color legend
#' @param text.pos.x Position of the text block on x-axis, relative value
#' @param text.pos.y Position of the text block on y1-axis, absolute or relative value
#' @param date.break Breaks for x-axis ('n days', '12 hours'...)
#' @param date.label Label format for x-axis, default dd.mm.yy
#' @param text.size Text size for the whole graphic. Default 12
#' @param text.x.angle Angle of x-axis text. Default 0
#' @param polder.f Area of the measure, for calculating Volume
#' @param polder.z Bottom level of the measure for calculating Volume. Give 'auto' for getting the minimum waterlevel in canal value. In this case, make sure the canal is completely dry at T0
#' @param versbose Boring? Should some message be displayed?
#' @param master.tbl Table of ID Coding of the sobek network
#' @return A ggplot2 graphic
#' @export
plot_polder_scenario_old <- function(
  name = NULL,
  case.list = NULL,
  case.desc = case.list,
  sobek.project = NULL,
  master.tbl = NULL,
  param = 'discharge',
  q.in = FALSE,
  q.out = FALSE,
  w.canal = FALSE,
  ref.mID = NULL,
  ref.mID2 = NULL,
  y2.scale = NULL,
  y2.tick1 = NULL,
  h.lines = NULL,
  peak.nday = NULL,
  peak.pegel = FALSE,
  delta.pegel = TRUE,
  delta.measure = TRUE,
  delta.line = FALSE,
  rel.heights = c(2, 0.7),
  compare.by = 'zustand',
  group.by = compare.by,
  cmp.sort = FALSE,
  plot.title = NULL,
  lt.name = 'Linienart',
  color.name = 'Farbe',
  text.pos.x = 0,
  text.pos.y = 1,
  date.break = '3 days',
  date.label = '%d.%m.%y',
  text.size = 12,
  text.x.angle = 0L,
  polder.f = NULL,
  polder.z = NULL,
  verbose = TRUE) {

  #----checking input----
  # there should be only two cases
  stopifnot(length(unlist(case.list)) == 2)
  f_args <- as.list(match.call(expand.dots = FALSE))
  param <- match.arg(param, c('discharge', 'waterlevel', 'both'))
  if (param == 'both') {
    if (length(y2.scale) > 1) {
      y2.scale_wt <- y2.scale[[1]]
      y2.scale_qt <- y2.scale[[2]]
    } else {
      y2.scale_wt <- y2.scale_qt <- y2.scale
    }
    if (length(y2.tick1) > 1) {
      y2.tick1_wt <- y2.tick1[[1]]
      y2.tick1_qt <- y2.tick1[[2]]
    } else {
      y2.tick1_wt <- y2.tick1_qt <- y2.scale
    }
    if (verbose) cat('working with waterlevel \n')
    f_args$param <- 'waterlevel'
    param <- 'waterlevel'
    f_args$y2.scale <- y2.scale_wt
    f_args$y2.tick1 <- y2.tick1_wt
    g_wt <- do.call(plot_polder_scenario, f_args)
    f_args$param <- 'discharge'
    param <- 'discharge'
    f_args$y2.scale <- y2.scale_qt
    f_args$y2.tick1 <- y2.tick1_qt
    if (verbose) cat('working with discharge \n')
    q_wt <- do.call(plot_polder_scenario, f_args)
    g <- cowplot::plot_grid(g_wt, q_wt, ncol = 1,
                       rel_heights = 1,
                       align = 'v', axis = 'l')
    return(g)
  } else {
    # in case users gave y2.scale or y2.tick1 more than one values
    y2.scale <- y2.scale[[1]]
    y2.tick1 <- y2.tick1[[1]]
  }
  stopifnot(!c(is.null(name), is.null(case.list), is.null(master.tbl),
               is.null(sobek.project)
  ))
  #----get id_data----
  if (isTRUE(verbose)) cat('Reading data at the measure...\n')
  id_data <- get_polder_data(
    name = name,
    case.list = case.list,
    case.desc = case.desc,
    param = param,
    w.canal = TRUE,
    sobek.project = sobek.project,
    master.tbl = master.tbl,
    verbose = verbose
  )
  case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
  # add case description columns, using for linetype later on
  id_data <- merge(id_data, case_tbl, by = 'case', sort = FALSE)
  cmp_vars <- case_tbl[, get(compare.by)]
  if (isTRUE(cmp.sort)) cmp_vars <- sort(cmp_vars)
  #----read data for Bezugspegel 1 & 2----
  if (!is.null(ref.mID)) {
    if (isTRUE(verbose))
      cat('Reading data for ref.mID...\n')
    if (length(ref.mID) > 1) {
      ref.mID_id <- ref.mID[[1]]
      if (hasName(ref.mID, 'ID')) ref.mID_id <- ref.mID$ID
      ref.mID_name <- ref.mID[[2]]
      if (hasName(ref.mID, 'name')) ref.mID_name <- ref.mID$name
      # ref.mID_color <- ref.mID_name
      ref.mID_color <- paste(ifelse(param == 'discharge', 'Q', 'W'),
                             ref.mID_name)
      ref.mID_type <- ifelse(param == 'discharge', 'qID', 'wID')
      if (hasName(ref.mID, 'type'))
        ref.mID_type <- ref.mID$type
      ref_mID_args <- list(
        case.list = case.list,
        sobek.project = sobek.project,
        id_type = ref.mID_id,
        param = param,
        verbose = FALSE
      )
      names(ref_mID_args)[3] <- ref.mID_type
      ref_mID <- do.call(his_from_case, ref_mID_args)
    } else{
      ref_mID <- his_from_case(
        case.list = case.list,
        sobek.project = sobek.project,
        mID = ref.mID[[1]],
        param = param,
        verbose = FALSE
      )
      ref.mID_name <-
        ifelse(!is.null(names(ref.mID)), names(ref.mID),
               toupper(ref.mID[[1]]))
      ref.mID_color <- paste(ifelse(param == 'discharge', 'Q', 'W'),
                             ref.mID_name)
    }
    colnames(ref_mID) <- c('ts', 'Bezugspegel', 'case')
    id_data <- merge(id_data, ref_mID, by = c('ts', 'case'))
    # get peak difference at the ref_measurement
    scheitel_max_c1 <- id_data[get(compare.by) == cmp_vars[[1]],
                               max(Bezugspegel)]
    scheitel_max_c2 <- id_data[get(compare.by) == cmp_vars[[2]],
                               max(Bezugspegel)]
    scheitel_ref_mID_delta <- scheitel_max_c1 - scheitel_max_c2
    scheitel_ref_mID_delta <- round(scheitel_ref_mID_delta, 2)
    # rounding value for discharge
    if (abs(scheitel_ref_mID_delta) > 2) {
      scheitel_ref_mID_delta <- round(scheitel_ref_mID_delta)
    }
    # get second Bezugspegel
    if (!is.null(ref.mID2)) {
      if (isTRUE(verbose))
        cat('Reading data for ref.mID2...\n')
      if (length(ref.mID2) > 1) {
        ref.mID2_id <- ref.mID2[[1]]
        if (hasName(ref.mID2, 'ID')) ref.mID2_id <- ref.mID2$ID
        ref.mID2_name <- ref.mID2[[2]]
        if (hasName(ref.mID2, 'name')) ref.mID2_name <- ref.mID2$name
        # ref.mID2_color <- ref.mID2_name
        ref.mID2_color <- paste(ifelse(param == 'discharge', 'Q', 'W'),
                                ref.mID2_name)
        ref.mID2_type <- ifelse(param == 'discharge', 'qID', 'wID')
        if (hasName(ref.mID2, 'type'))
          ref.mID2_type <- ref.mID2$type
        ref_mID_args <- list(
          case.list = case.list,
          sobek.project = sobek.project,
          id_type = ref.mID2_id,
          param = param,
          verbose = FALSE
        )
        names(ref_mID_args)[3] <- ref.mID2_type
        ref_mID2 <- do.call(his_from_case, ref_mID_args)

      } else{
        ref_mID2 <- his_from_case(
          case.list = case.list,
          sobek.project = sobek.project,
          mID = ref.mID2[[1]],
          param = param,
          verbose = FALSE
        )
        ref.mID2_name <-
          ifelse(!is.null(names(ref.mID2)), names(ref.mID2),
                 toupper(ref.mID2[[1]]))
        ref.mID2_color <- paste(ifelse(param == 'discharge', 'Q', 'W'),
                                ref.mID2_name)
      }
      colnames(ref_mID2) <- c('ts', 'Bezugspegel2', 'case')
      id_data <- merge(id_data, ref_mID2, by = c('ts', 'case'))
      # get peak difference at the ref_measurement
      scheitel_max_c1 <- id_data[get(compare.by) == cmp_vars[[1]],
                                 max(Bezugspegel2)]
      scheitel_max_c2 <- id_data[get(compare.by) == cmp_vars[[2]],
                                 max(Bezugspegel2)]
      scheitel_ref_mID_delta2 <- scheitel_max_c1 - scheitel_max_c2
      scheitel_ref_mID_delta2 <- round(scheitel_ref_mID_delta2, 2)
      # rounding value for discharge
      if (abs(scheitel_ref_mID_delta2) > 2) {
        scheitel_ref_mID_delta2 <- round(scheitel_ref_mID_delta2)
      }
    }
  }
  # limit data to the peak value
  if (!is.null(peak.nday)) {
    stopifnot(is.numeric(peak.nday))
    if (isTRUE(peak.pegel) & !is.null(ref.mID)) {
      ts_max <- id_data[Bezugspegel == max(Bezugspegel), ts]
    } else{
      ts_max <- id_data[Nach == max(Nach), ts]
    }
    xlim_min <- ts_max - peak.nday * 24 * 3600
    xlim_max <- ts_max + peak.nday * 24 * 3600
    id_data <- id_data[ts >= xlim_min & ts <= xlim_max]
  }
  #----finding scheitel delta at the measure----
  if (isTRUE(delta.measure)) {
    # calculate diff between two max value (different moment)
    scheitel_max_c1 <- id_data[get(compare.by) == cmp_vars[[1]], max(Nach)]
    scheitel_max_c2 <- id_data[get(compare.by) == cmp_vars[[2]], max(Nach)]
    scheitel_measure_delta <- scheitel_max_c1 - scheitel_max_c2
    scheitel_measure_delta <- round(scheitel_measure_delta, 2)
    # rounding value for discharge
    if (abs(scheitel_measure_delta) > 2) {
      scheitel_measure_delta <- round(scheitel_measure_delta)
    }
  }
  #-----preparing plot-----
  if (isTRUE(verbose)) cat('Preparing graphic...\n')
  einlass_cols <- grep('Einlass', colnames(id_data), value = TRUE)
  auslass_cols <- grep('Auslass', colnames(id_data), value = TRUE)
  y2_axis <- isTRUE(q.in) | isTRUE(q.out) | (param == 'discharge' & isTRUE(w.canal))
  # in case there is a y2_axis
  #----processing y-axes limits----
  y1_cols <- c('Nach')
  if (!is.null(ref.mID)) y1_cols <- c('Nach', 'Bezugspegel')
  if (!is.null(ref.mID2)) y1_cols <- c('Nach', 'Bezugspegel', 'Bezugspegel2')
  if (isTRUE(w.canal) & param == 'waterlevel') y1_cols <- c(y1_cols, 'W_innen')
  y1_max <- id_data[, max(.SD, na.rm = TRUE), .SDcols = y1_cols]
  y1_min <- id_data[, min(.SD, na.rm = TRUE), .SDcols = y1_cols]
  y1_length <- y1_max - y1_min
  y1_pretty <- pretty(y1_min:y1_max, 5, 5)
  # in case there is y2_axis
  if (isTRUE(y2_axis)) {
    y2_cols <- c(einlass_cols,
                 auslass_cols)[c(rep(q.in, length(einlass_cols)),
                                 rep(q.out, length(auslass_cols)))]
    if (isTRUE(w.canal) & param == 'discharge') {
      q.in <- FALSE
      q.out <- FALSE
      y2_cols <- 'W_innen'
    }
    y2_max <- id_data[, max(.SD, na.rm = TRUE), .SDcols = y2_cols]
    y2_min <- id_data[, min(.SD, na.rm = TRUE), .SDcols = y2_cols]
    y2_length <- y2_max - y2_min
    if (is.null(y2.scale)) {
      y1_length <- y1_max - y1_min
      y2_length <- y2_max - y2_min
      y2.scale <- y1_length * 0.75 * 1000 / y2_length
      for (i in 0:3) {
        if (abs(y2.scale) > 10 ** i)
          y2.scale <- round(y2.scale, -i)
      }
      y2.scale <- y2.scale / 1000
      y2_shift <- y1_min - y2_min * y2.scale
    } else{
      y2_shift <- round(y1_min - y2_min * y2.scale, 2)
    }
    if (y1_length < 10) {
      y1_max_1 <- y1_max * 100
      y1_min_1 <- y1_min * 100
      y1_pretty <- pretty(y1_min_1:y1_max_1, 5, 5)
      y1_pretty <- y1_pretty / 100
    } else{
      y1_pretty <- pretty(y1_min:y1_max, 5, 5)
    }
    if (!is.null(y2.tick1)) {
      y2_shift = y1_pretty[1] - y2.tick1 * y2.scale
    }
    y2_pretty <- (y1_pretty - y2_shift) / y2.scale
    if (0 %between% c(min(y2_pretty), max(y2_pretty))) {
      y2_pretty <- unique(sort(c(y2_pretty, 0)))
    }
    if (is.infinite(y2.scale)) {
      y1_max <- id_data[, max(.SD, na.rm = TRUE), .SDcols = y1_cols]
      y1_min <- id_data[, min(.SD, na.rm = TRUE), .SDcols = y1_cols]
      y1_pretty <- pretty(y1_min:y1_max, 5, 5)
      y2.scale = 1
      y2_shift = min(y1_pretty)
    }
  }
  # make sure y1_max is in the range of y1_pretty
  y1_tick_diff <- abs(y1_pretty[2] - y1_pretty[1])
  if (max(y1_pretty) < y1_max + y1_tick_diff) {
    y1_max <- y1_max + y1_tick_diff/2
    if (y1_length < 10) {
      y1_max_1 <- y1_max * 10
      y1_min_1 <- y1_min * 10
      y1_pretty <- pretty(y1_min_1:y1_max_1, 5, 5)
      y1_pretty <- y1_pretty / 10
    } else{
      y1_pretty <- pretty(y1_min:y1_max, 5, 5)
    }
  }
  #----initializing graphic----
  if (!is.null(ref.mID)) {
    g <- ggplot(data = id_data,
                mapping = aes(x = ts, linetype = !!ensym(compare.by))
    ) +
      geom_line(aes(y = Bezugspegel, color = ref.mID_color),
      size = 1)
    if (!is.null(ref.mID2)) {
      g <- g +
        geom_line(aes(y = Bezugspegel2, color = ref.mID2_color),
        size = 1)
    }
  } else{
    g <- ggplot(data = id_data,
                mapping = aes(x = ts, linetype = !!ensym(compare.by))
    )
  }
  if (isTRUE(verbose)) cat('Adding hydrographs...\n')
  if (tolower(param) == 'discharge') {
    #----working with discharge----
    g <- g +
      geom_line(aes(y = Nach,  color = 'Q nach Maßnahme'),
                size = 1)
    if (isTRUE(w.canal)) {
      q.in <- FALSE
      # if parameter is discharge, move waterlevel to secondary axis
      g <- g + geom_line(aes(y = W_innen * y2.scale + y2_shift,
                             color = 'W in Maßnahme'),
                         size = 1)
      y2_name <- 'Wasserstand [m+NHN]'
    }
    if (isTRUE(q.in)) {
      y2_name <- 'Abfluss Einlass/Auslass [m³/s]'
      # adding Einlass lines
      if (length(einlass_cols) > 1) {
        id_data_einlass <- melt(id_data, measure.vars = einlass_cols,
                                variable.name = 'Einlass',
                                value.name = 'Q_Einlass')
        id_data_einlass[, Einlass := paste("Q", Einlass)]
        g <- g + geom_line(data = id_data_einlass,
                           aes(y = Q_Einlass * y2.scale + y2_shift,
                           color = Einlass),
                       size = 1)
      } else{
        g <- g + geom_line(aes(
          y = !!ensym(einlass_cols) * y2.scale + y2_shift,
          color = eval(paste('Q', einlass_cols))
        ),
        size = 1)
      }
    }
    if (isTRUE(q.out)) {
      if (length(auslass_cols) > 1) {
        id_data_auslass <- melt(id_data, measure.vars = auslass_cols,
                                variable.name = 'Auslass',
                                value.name = 'Q_Auslass')
        id_data_auslass[, Auslass := paste("Q", Auslass)]
        g <- g + geom_line(data = id_data_auslass,
                           aes(y = Q_Auslass * y2.scale + y2_shift,
                               color = Auslass),
                           size = 1)
      } else{
        g <- g + geom_line(aes(
          y = !!ensym(auslass_cols) * y2.scale + y2_shift,
          color = eval(paste('Q',  auslass_cols))
        ),
        size = 1)
      }
    }
    #----working with WL----
  } else {
    # adding W_innen directly to the graphic should not be a problem
    g <- g +
      geom_line(aes(y = Nach,  color = 'W nach Maßnahme'),
                size = 1)
    if (isTRUE(w.canal)) {
      y2_name <- 'Wasserstand [m+NHN]'
      g <- g + geom_line(aes(y = W_innen,
                             color = 'W in Maßnahme'),
                         size = 1)
    }
    if (isTRUE(q.in)) {
      einlass_cols <- grep('Einlass', colnames(id_data), value = TRUE)
      y2_name <- 'Abfluss Einlass/Auslass [m³/s]'
      # adding Einlass lines
      if (length(einlass_cols) > 1) {
        id_data_einlass <- melt(id_data, measure.vars = einlass_cols,
                                variable.name = 'Einlass',
                                value.name = 'Q_Einlass')
        id_data_einlass[, Einlass := paste('Q', Einlass)]
        g <- g + geom_line(data = id_data_einlass,
                           aes(y = Q_Einlass * y2.scale + y2_shift,
                               color = Einlass),
                           size = 1)
      } else{
        g <- g + geom_line(aes(
          y = !!ensym(einlass_cols) * y2.scale + y2_shift,
          color = eval(paste('Q', einlass_cols))
        ),
        size = 1)
      }
    }
    if (isTRUE(q.out)) {
      y2_name <- 'Abfluss Einlass/Auslass [m³/s]'
      if (length(auslass_cols) > 1) {
        id_data_auslass <- melt(id_data, measure.vars = auslass_cols,
                                variable.name = 'Auslass',
                                value.name = 'Q_Auslass')
        id_data_auslass[, Auslass := paste('Q', Auslass)]
        g <- g + geom_line(data = id_data_auslass,
                           aes(y = Q_Auslass * y2.scale + y2_shift,
                               color = Auslass),
                           size = 1)
      } else{
        g <- g + geom_line(aes(
          y = !!ensym(auslass_cols) * y2.scale + y2_shift,
          color = eval(paste('Q', auslass_cols))
        ),
        size = 1)
      }
    }
  }
  g$labels$colour <- color.name
  g$labels$linetype <- lt.name
  #----calculating Max Volume----
  if (isTRUE(verbose)) cat('Calculating volume...\n')
  if (!is.null(polder.f)) {
    if (is.null(polder.z)) {
      id_data[, H_innen_max := max(W_innen, na.rm = TRUE) -
                min(W_innen, na.rm = TRUE), by = case
              ]
      id_data[, Volume_max := round(H_innen_max * polder.f / 100, 2)]
    } else{
      id_data[, Volume_max := round(polder.z * polder.f / 100, 2)]
    }
  } else {
    id_vol_data <- get_polder_volume(name = name,
                                         case.list = case.list,
                                         case.desc = case.desc,
                                         sobek.project = sobek.project,
                                         master.tbl = master.tbl
    )
    id_data <- merge(id_data, id_vol_data, by = 'case', sort = FALSE)
  }
  #----annotating max value----
  if (isTRUE(verbose)) cat('adding text box...\n')
  for (i in einlass_cols) {
    einlass_max_col <- paste(i, 'Max', sep = '_')
    id_data[, eval(einlass_max_col) := max(get(i), na.rm = TRUE)]
  }
  id_data[, W_in_max := max(W_innen, na.rm = TRUE), by = case]
  # finding the locations on x-axis for the annotated text
  # get length of x_axis, then get text.pos of it
  id_data[, N := .N, by = case]
  id_data[, ts_min := shift(ts, n = floor(text.pos.x*N),
                            fill = NA, type = 'lead'),
          by = case]
  if (!is.null(h.lines)){
    id_hlines <- id_data[, min(ts), by = case]
  }
  id_data_nrow <- id_data[, min(ts_min, na.rm = TRUE), by = case]
  colnames(id_data_nrow) <- c('case', 'ts')
  id_max <- merge(id_data_nrow, id_data, by = c('ts', 'case'), sort = FALSE)
  id_max[, W_in_max := round(W_in_max, 2)]
  id_max[, label := '']
  case_has_max <- id_max[W_in_max == max(W_in_max, na.rm = TRUE), case]
  id_max[case == case_has_max,
         label := paste(
           'V_max in Maßnahme: ', Volume_max, ' Mio. m³\n',
           'W_max in Maßnahme:   ', W_in_max, ' m + NHN\n',
           label,
           sep = "")
         ]
  for (i in einlass_cols) {
    einlass_max_col <- paste(i, 'Max', sep = '_')
    id_max[, eval(einlass_max_col) := round(as.numeric(get(einlass_max_col)), 1),
           .SDcols = einlass_max_col]
    id_max[case == case_has_max,
           label := paste(
             label,
             'Q_max durch ', i, ': ', get(einlass_max_col), ' m³/s\n',
             sep = "")
           ]
  }
  delta_unit <- ifelse(param == 'discharge', 'm³/s', 'm')
  if (isTRUE(delta.pegel) & !is.null(ref.mID)) {
    id_max[, label := paste(
      'Delta am ', ref.mID_name, ": ", scheitel_ref_mID_delta, " ", delta_unit, " \n",
      label,
      sep = ""
    )]
    # adding delta ref.mID2
    if (!is.null(ref.mID2)) {
      id_max[, label := paste(
        'Delta am ', ref.mID2_name, ": ", scheitel_ref_mID_delta2, " ", delta_unit, " \n",
        label,
        sep = ""
      )]
    }
  }
  if (isTRUE(delta.measure)) {
    id_max[, label := paste(
      'Delta an der Maßnahme: ', scheitel_measure_delta, " ", delta_unit, " \n",
      label,
      sep = ""
    )]
  }
  # y position of the text block
  y1_pos_txt <- y1_pretty[(length(y1_pretty) - 1)]
  if (!is.null(text.pos.y)) {
    y1_pos_txt <- text.pos.y
    if (abs(text.pos.y) < 1.2) y1_pos_txt <- text.pos.y * (y1_max - y1_min) + y1_min
  }
  g <- g +
    geom_text(
      data    = id_max[case == case_has_max],
      mapping = aes(x = ts_min,
                    y = y1_pos_txt,
                    label = label),
      hjust = 0,
      vjust = 1
    )

  if (!is.null(h.lines)) {
    for (i in seq_along(h.lines)) {
      hline_label <- ifelse(is.null(names(h.lines[i])),
                            paste('V_', h.lines[[i]], sep = ""),
                            names(h.lines[i])
      )
      # adding new colume to id_hlines
      id_hlines[, eval(hline_label) := h.lines[[i]]]
      g <- g + geom_hline(yintercept = h.lines[[i]], linetype = 3)
    }
    id_hlines <- melt(id_hlines, id.vars = c('case', 'V1'))
    # merging with case table for facetting
    id_hlines <- merge(id_hlines, case_tbl, by = 'case', sort = FALSE)
    g <- g + geom_text(
      data    = id_hlines,
      # V1 is the colume name of the min (ts_hlines)
      mapping = aes(x = V1, y = value,
                    label = sub("^V_", "", variable)
      ),
      hjust   = 0,
      vjust = 0,
      check_overlap = TRUE
    )
  }
  if (isTRUE(y2_axis)) {
    if (y2.scale != 0) {
      y2_pretty <- round(y2_pretty, 2)
      g <- g +
        scale_y_continuous(
          breaks = y1_pretty,
          labels = fm_nr,
          sec.axis =
            sec_axis(trans = ~./y2.scale - y2_shift/y2.scale,
                     breaks = y2_pretty,
                     labels = fm_nr,
                     name = y2_name)
        )
    } else{
      # in this case, y2 is a zero h-line
      g <- g +
        scale_y_continuous(
          breaks = y1_pretty,
          labels = fm_nr,
          sec.axis =
            sec_axis(trans = ~./y2.scale - y2_shift/y2.scale,
                     name = y2_name)
        )
    }
  }
  #----graphic layout----
  y1_label <- ifelse(param == 'discharge', 'Abfluss m³/s', 'Wasserstand [m+NHN]')
  if (is.null(plot.title)) {
    plot.title <- paste(str_extract(y1_label, 'Abfluss|Wasserstand'),
                        ' Ganglinien für Maßnahme: ', name,
                        '. Hochwasser: ', case_tbl$hwe[[1]], ' ',
                        case_tbl$vgf[[1]], sep = '')
  }
  g <- g + theme_bw() +
    theme(
      legend.position = 'bottom',
      text = element_text(size = text.size),
      axis.text.x = element_text(angle = text.x.angle)
    ) +
    scale_x_datetime(
      name = 'Zeit',
      date_breaks = date.break,
      date_labels = date.label
    ) + ylab(y1_label) +
    ggtitle(plot.title)
  #----adding delta line beneath the main graphic----
  if (isTRUE(delta.line)) {
    delta_data <- dcast(id_data, ts ~ get(compare.by),
                        value.var = 'Nach')
    delta_data[, `Delta an der Maßnahme` := get(cmp_vars[1]) - get(cmp_vars[2])]
    delta_data[, eval(cmp_vars[1]) := NULL]
    delta_data[, eval(cmp_vars[2]) := NULL]
    if(!is.null(ref.mID)) {
      delta_p1 <- dcast(id_data, ts ~ get(compare.by),
                        value.var = 'Bezugspegel')
      delta_p1[, eval(paste('Delta am ', ref.mID_name)) :=
                 get(cmp_vars[1]) - get(cmp_vars[2])]
      delta_p1[, eval(cmp_vars[1]) := NULL]
      delta_p1[, eval(cmp_vars[2]) := NULL]
      delta_data <- merge(delta_data, delta_p1, by = 'ts',
                          sort = FALSE)
      if(!is.null(ref.mID2)) {
        delta_p2 <- dcast(id_data, ts ~ get(compare.by),
                            value.var = 'Bezugspegel2')
        delta_p2[, eval(paste('Delta am ', ref.mID2_name)) :=
                     get(cmp_vars[1]) - get(cmp_vars[2])]
        delta_p2[, eval(cmp_vars[1]) := NULL]
        delta_p2[, eval(cmp_vars[2]) := NULL]
        delta_data <- merge(delta_data, delta_p2, by = 'ts',
                            sort = FALSE)
      }
    }
    delta_data <- melt(delta_data,
                       id.vars = 'ts', value.name = 'value',
                       variable.name = 'Delta')
    delta_title <- ifelse(param == 'discharge',
                          paste('Abfluss Differenz [',
                                cmp_vars[1], " - ", cmp_vars[2], "]",
                                sep = ""),
                          paste('Wasserstand Differenz [',
                                cmp_vars[1], " - ", cmp_vars[2], "]",
                                sep = ""))
    delta_ylab <- ifelse(param == 'discharge',
                         'Abfluss Differenz [m³/s]',
                         'Wasserstand Differenz [cm]'
                         )
    if (param == 'waterlevel') delta_data[, value := round(value * 100, 1)]
    g2 <- ggplot(data = delta_data,
                 aes(x = ts, y = value, linetype = Delta)) +
      scale_x_datetime(name = 'Zeit', breaks = date.break,
                       date_labels = date.label) +
      geom_line(size = 1) +
      theme_bw() +
      theme(
        legend.position = 'bottom',
        text = element_text(size = text.size),
        axis.text.x = element_blank()
        ) +
      ggtitle(delta_title) + ylab(delta_ylab)
    g2$labels$linetype = 'Linienart'
    g <- cowplot::plot_grid(g, g2, ncol = 1, rel_heights = rel.heights,
                            align = 'v', axis = 'l')
  }
  if (isTRUE(verbose) & isTRUE(y2_axis)) {
    cat('tried with y2.scale =', y2.scale,
                '. y2_shift =', y2_shift,
                ". Use y2.tick1 and y2.scale to adjust y2-axis\n")
  }

  return(g)
}


#' Get data for polder
#'
#' @inheritParams plot_polder_scenario
#' @export
get_polder_scenario_data <- function(name,
                                     param,
                                     case.list,
                                     case.desc = case.list,
                                     sobek.project,
                                     ref.mID = NULL,
                                     ref.mID2 = NULL,
                                     w.canal = TRUE,
                                     q.in = TRUE,
                                     q.out = q.in,
                                     master.tbl,
                                     verbose = FALSE
) {
  pegel1 <- parse_ref_id(ref.mID)
  pegel2 <- parse_ref_id(ref.mID2)
  case_tbl <- get_polder_scenario_case_tbl(case.list, case.desc, sobek.project)
  id_tbl <- get_polder_scenario_id_tbl(name, case.list, master.tbl,
                                       sobek.project, case_tbl, pegel1, pegel2)
  id_vol <- id_tbl[grepl(paste0(name, '_Vol'), besonderheit)]
  id_tbl <- id_tbl[!grepl(paste0(name, '_Vol'), besonderheit)]
  # for w.canal, always need water level (so only wID|mID)
  # assumed that sID was not given to w.canal - if yes, an error with parameter will occur
  # reading volume
  vol_max_list <- vector()
  for (i in seq_along(case.list)) {
    this_case <- case.list[[i]]
    this_his <- id_vol[case == this_case, his_file][1]
    if (verbose) cat('Get volume for case: ', this_case, '\n')
    this_vol_tbl <- his_from_list(his.file = this_his,
                                  id.list = id_vol[his_file == this_his, ID_F],
                                  param = 'Volume')
    this_vol_tbl[, Volume := rowSums(.SD), .SDcols = -c('ts')]
    vol_max_list[[i]] <- round(max(this_vol_tbl$Volume, na.rm = TRUE) / 10^6, 2)
  }
  vol_max_tbl <- data.table(Volume = vol_max_list, case = case.list)
  # reading data at "nach, vor,
  if (param == 'waterlevel') {
    pat_vor_nach <- '_Vor|_Nach|Bezugspegel'
    if (w.canal) {
      pat_vor_nach <- '_Vor|_Nach|_Innen|Bezugspegel'
    }
    id_vor_nach <- id_tbl[grepl(pat_vor_nach, besonderheit)]
    data_tbl <- get_data_for_id_tbl(case.list, param, id_vor_nach)
    if (q.in | q.out) {
      id_ein_aus <- id_tbl[grepl('_Einlass|_Auslass', besonderheit)]
      data_ein_aus <- get_data_for_id_tbl(case.list, 'discharge', id_ein_aus)
      data_tbl <- merge(data_tbl, data_ein_aus, by = c('ts', 'case'), sort = FALSE)
    }
    data_cols <- colnames(data_tbl)
    colnames(data_tbl) <- rename_polder_cols(data_cols)
  } else {
    if (q.in | q.out) {
      id_rest <- id_tbl[!grepl(paste0(name, '_Innen'),
                               besonderheit)][ID_TYPE != 'wID']
      data_tbl <- get_data_for_id_tbl(case.list, 'discharge', id_rest)
    } else {
      id_rest <- id_tbl[grepl(paste0(name, '_Auslass|_Einlass|Bezugspegel'),
                              besonderheit)][ID_TYPE != 'wID']
      data_tbl <- get_data_for_id_tbl(case.list, 'discharge', id_rest)
    }
    if (w.canal) {
      id_wcanal <- id_tbl[grepl(paste0(name, '_Innen'), besonderheit)]
      data_wcanal <- get_data_for_id_tbl(case.list, 'waterlevel', id_wcanal)
      data_tbl <- merge(data_wcanal, data_tbl, by = c('ts', 'case'), sort = FALSE)
    }
    data_cols <- colnames(data_tbl)
    colnames(data_tbl) <- rename_polder_cols(data_cols)
  }
  data_tbl <- merge(
    data_tbl,
    case_tbl[, c('case', 'zustand', 'zielpegel', 'hwe', 'vgf', 'notiz')],
    by = 'case', sort = FALSE)
  vol_max_tbl <- merge(
    vol_max_tbl,
    case_tbl[, c('case', 'zustand', 'zielpegel', 'hwe', 'vgf', 'notiz')],
    by = 'case', sort = FALSE)
  return(list(data_tbl = data_tbl,  vol_max_tbl = vol_max_tbl))
}


get_polder_scenario_case_tbl <- function(
  case.list, case.desc, sobek.project
) {
  # parsing information from cases
  case_tbl <- parse_case(case.desc, case.list)
  # reading caselist.cmt
  case_cmt <- fread(file.path(sobek.project, 'caselist.cmt'), sep = ' ',
                    quote = "'", col.names = c('case_number', 'case'))
  case_cmt[, case := str_remove_all(case, '"')]
  case_cmt <- case_cmt[case %in% case.list]
  case_chk <- assertthat::are_equal(sort(case_cmt$case),
                                    sort(case.list))
  if (!case_chk) {
    stop('Not all cases were found in the caselist.cmt, check case names or sobek.project')
  }
  case_cmt[, case_folder := file.path(sobek.project, case_number)]
  case_tbl <- merge(case_tbl, case_cmt, by = 'case')
  return(case_tbl)
}

get_polder_scenario_id_tbl <- function(
  name, case.list, master.tbl, sobek.project, case_tbl, pegel1, pegel2
) {
  id_tbl <- get_id_tbl(name = name, case.list = case.list, master.tbl = master.tbl)
  id_tbl <- id_tbl[, c('km', 'river', 'ID') := rep(NULL, 3)]
  if (!is.null(pegel1$ID)) {
    refid_tbl <- case_tbl[, c('case')]
    refid_tbl[, ID_F :=
                pegel1$ID][, ID_TYPE := pegel1$type][, besonderheit := 'Bezugspegel']
    id_tbl <- rbind(id_tbl, refid_tbl)
  }
  if (!is.null(pegel2$ID)) {
    refid_tbl <- case_tbl[, c('case')]
    refid_tbl[, ID_F :=
                pegel2$ID][, ID_TYPE := pegel2$type][, besonderheit := 'Bezugspegel_2']
    id_tbl <- rbind(id_tbl, refid_tbl)
  }
  polder_pat <- paste0(name, '_Vol|',
                       name, '_Innen|',
                       name, '_Vor|',
                       name, '_Auslass|',
                       name, '_Einlass|',
                       name, '_Nach|Bezugspegel')
  id_tbl <- id_tbl[grepl(polder_pat, besonderheit)]
  id_tbl$his_file <- sapply(tolower(id_tbl$ID_TYPE), function(x) switch(
    tolower(x),
    sid = 'struc.his',
    wid = 'calcpnt.his',
    mid = 'measstat.his',
    qid = 'reachseg.his',
    stop('master.tbl has wrong format!')
  ),
  USE.NAMES = FALSE
  )
  id_tbl <- merge(id_tbl, case_tbl[, c('case', 'case_folder')], by = 'case')
  id_tbl[, his_file := file.path(case_folder, his_file)]
  return(id_tbl)
}

get_ids_polder <- function(name, id_tbl) {
  id_vol <- id_tbl[grepl(paste0(name, '_Vol'), besonderheit)]
  id_tbl <- id_tbl[!grepl(paste0(name, '_Vol'), besonderheit)]
  # check if his file for case are all calcpnt.his
  his_type <- unique(basename(id_tbl_vol$his_file))
  if (!isTRUE(all(his_type == 'calcpnt.his')) | length(his_type) == 0) {
    stop('master.tbl has wrong format for Volume IDs of the polder: ', name)
  }
  id_nach <- id_tbl[grepl(paste0(name, '_Nach'), besonderheit)]
  check_id <- function(x, multi = FALSE) {
    id_nrow <- nrow(x)
    if (id_nrow == 0) return(FALSE)
    if (!multi) {
      id_ncase <- length(unique(x$case))
      id_nhis <- length(unique(x$his_file))
      if (id_ncase != id_nhis) return(FALSE)
    }
    if (isTRUE(any(is.na(x)))) return(FALSE)
    return(TRUE)
  }
  if (!check_id(id_nach)) {
    stop('master.tbl has wrong format for "Nach" IDs of the polder: ', name)
  }
  id_innen <- id_tbl[grepl(paste0(name, '_Innen'), besonderheit)]
  if (!check_id(id_innen)) {
    stop('master.tbl has wrong format for "Nach" IDs of the polder: ', name)
  }
  id_vor <- id_tbl[grepl(paste0(name, '_Vor'), besonderheit)]
  if (!check_id(id_vor)) {
    stop('master.tbl has wrong format for "Vor" IDs of the polder: ', name)
  }
  id_ein <- id_tbl[grepl(paste0(name, '_Einlass'), besonderheit)]
  if (!check_id(id_ein, multi = TRUE)) {
    stop('master.tbl has wrong format for "Vor" IDs of the polder: ', name)
  }
  id_aus <- id_tbl[grepl(paste0(name, '_Auslass'), besonderheit)]
  if (!check_id(id_aus, multi = TRUE)) {
    stop('master.tbl has wrong format for "Vor" IDs of the polder: ', name)
  }
  id_pegel <- id_tbl[grepl('Pegel1', besonderheit)]
  ret <- list(
    id_vol = id_vol,
    id_nach = id_nach,
    id_vor = id_vor,
    id_innen = id_innen,
    id_ein = id_ein,
    id_aus = id_aus,
    id_pegel = id_pegel
  )
  return(ret)
}


get_data_for_id_tbl <- function(
  case.list, param, id_tbl
){
  data_list <- list()
  for (i in seq_along(case.list)) {
    this_case <- case.list[i]
    his_files <- unique(id_tbl[case == this_case, his_file])
    # this_his should have length of 1 or 2
    # length >=2 means there is atleast one Bezugspegel with different id_type
    for (j in seq_along(his_files)) {
      data_tmp <- his_from_list(
        his.file = his_files[j],
        id.list = id_tbl[case == this_case & his_file == his_files[j], ID_F],
        param = param
      )
      colnames(data_tmp) <- c(
        'ts',
        id_tbl[case == this_case & his_file == his_files[j], besonderheit]
      )
      if (j == 1) {
        data_this_case <- data_tmp
      } else {
        data_this_case <- merge(data_this_case, data_tmp, by = 'ts', sort = FALSE)
      }
    }
    data_this_case[, case := this_case]
    data_list[[i]] <- data_this_case
  }
  data_tbl <- rbindlist(data_list)
  data_cols <- colnames(data_tbl)
  return(data_tbl)
}

rename_polder_cols <- function(data_cols) {
  data_cols <- str_replace_all(data_cols, '[^,]*_(Vor[^,]*).*', '\\1')
  data_cols <- str_replace_all(data_cols, '[^,]*_(Nach[^,]*).*', '\\1')
  data_cols <- str_replace_all(data_cols, '[^,]*_(Einlass[^,]*).*', '\\1')
  data_cols <- str_replace_all(data_cols, '[^,]*_(Auslass[^,]*).*', '\\1')
  data_cols <- str_replace_all(data_cols, '.*_Innen.*', 'W_innen')
  return(data_cols)
}


# New version -------------------------------------------------------------


#' Plot hydrographs for a measure with comparing by scenario (zustand)
#' @param name Name of the measure (with/without the measure)
#' @param case.list List of cases
#' @param case.desc Correct (according to case naming standard in NHWSP) version of case.list, it will be used for legend
#' @param sobek.project Path to sobek project
#' @param param 'waterlevel', 'discharge', or 'both'.
#' If both, two graphics will be first procedured and then arrange vertically together
#' @param q.in Logical. Should discharge through the inlet be plotted?
#' @param q.out Logical. Should discharge through the outlet be plotted?
#' @param w.canal Logical. Should Wt line inside the measure be plotted?
#' @param ref.mID ID of Bezugspegel
#' @param y2.scale Scaling between main and secondary y-axes.
#' This is an important paramter. If the line for secondary axis is too big, try to change y2.scale
#' y2.scale with be automatically calculated so that the height of the graph on y2-axis is about 3/4 those on y1-axis
#' @param y2.tick1 The value of the first tick on the y2-axis. Using this and y2.scale to make y2-axis looks nice.
#' @param h.lines list of (name = value) to be displayed as horizontal line
#' @param peak.nday Should the plot limit to nday before and after the peak. Default is not (NULL). Otherwise please give a number of days
#' @param peak.pegel If the plot should be limit to the peak area, should it be the peak of the referenced location (ref.mID). Default is not.
#' @param delta.pegel Logical. Should peak delta at ref.mID be displayed
#' @param delta.measure Logical. Should peak delta at the measure be displayed
#' @param delta.line Logical. Should Delta lines be plotted extra, beneath the main plot? Default is FALSE.
#' @param rel.height Relative size of the main and the Delta plot. Default c(2, 0.7)
#' @param compare.by Should the line be compare by 'case' or by 'location'
#' @param cmp.sort Should comparing parameter be sorted. Default is FALSE
#' @param group.by Groupping for delta calculation
#' @param plot.title Title of the plot
#' @param lt.name Name of the linetype legend
#' @param color.name Name of the color legend
#' @param text.pos.x Position of the text block on x-axis, relative value
#' @param text.pos.y Position of the text block on y1-axis, absolute or relative value
#' @param date.break Breaks for x-axis ('n days', '12 hours'...)
#' @param date.label Label format for x-axis, default dd.mm.yy
#' @param text.size Text size for the whole graphic. Default 12
#' @param text.x.angle Angle of x-axis text. Default 0
#' @param polder.f Area of the measure, for calculating Volume
#' @param polder.z Bottom level of the measure for calculating Volume. Give 'auto' for getting the minimum waterlevel in canal value. In this case, make sure the canal is completely dry at T0
#' @param versbose Boring? Should some message be displayed?
#' @param master.tbl Table of ID Coding of the sobek network
#' @return A ggplot2 graphic
#' @export
plot_polder_scenario <- function(
  name,
  case.list,
  case.desc = case.list,
  sobek.project,
  master.tbl,
  param = 'discharge',
  q.in = FALSE,
  q.out = FALSE,
  w.canal = FALSE,
  ref.mID = NULL,
  ref.mID2 = NULL,
  y2.scale = NULL,
  y2.tick1 = NULL,
  h.lines = NULL,
  peak.nday = NULL,
  peak.pegel = FALSE,
  delta.pegel = TRUE,
  delta.measure = TRUE,
  delta.line = FALSE,
  rel.heights = c(2, 0.7),
  compare.by = 'zustand',
  group.by = compare.by,
  cmp.sort = FALSE,
  plot.title = NULL,
  lt.name = 'Linienart',
  color.name = 'Farbe',
  text.pos.x = 0,
  text.pos.y = 1,
  date.break = '3 days',
  date.label = '%d.%m.%y',
  text.size = 12,
  text.x.angle = 0L,
  polder.f = NULL,
  polder.z = NULL,
  verbose = TRUE) {

  #----checking input----
  # there should be only two cases
  stopifnot(length(unlist(case.list)) == 2)
  # I have to do this manually because macht.call does not work correctly
  formal_args <- formals()
  f_args <- list()
  for (a_arg in names(formal_args)) {
    f_args[[a_arg]] <- get(a_arg)
  }
  param <- match.arg(param, c('discharge', 'waterlevel', 'both'))
  if (param == 'both') {
    if (length(y2.scale) > 1) {
      y2.scale_wt <- y2.scale[[1]]
      y2.scale_qt <- y2.scale[[2]]
    } else {
      y2.scale_wt <- y2.scale_qt <- y2.scale
    }
    if (length(y2.tick1) > 1) {
      y2.tick1_wt <- y2.tick1[[1]]
      y2.tick1_qt <- y2.tick1[[2]]
    } else {
      y2.tick1_wt <- y2.tick1_qt <- y2.scale
    }
    if (verbose) cat('working with waterlevel \n')
    f_args$param <- 'waterlevel'
    param <- 'waterlevel'
    f_args$y2.scale <- y2.scale_wt
    f_args$y2.tick1 <- y2.tick1_wt
    g_wt <- do.call(plot_polder_scenario, f_args)
    f_args$param <- 'discharge'
    param <- 'discharge'
    f_args$y2.scale <- y2.scale_qt
    f_args$y2.tick1 <- y2.tick1_qt
    if (verbose) cat('working with discharge \n')
    q_wt <- do.call(plot_polder_scenario, f_args)
    g <- cowplot::plot_grid(g_wt, q_wt, ncol = 1,
                            rel_heights = 1,
                            align = 'v', axis = 'l')
    return(g)
  } else {
    # in case users gave y2.scale or y2.tick1 more than one values
    y2.scale <- y2.scale[[1]]
    y2.tick1 <- y2.tick1[[1]]
  }
  stopifnot(!c(is.null(name), is.null(case.list), is.null(master.tbl),
               is.null(sobek.project)
  ))
  #----get id_data----
  if (isTRUE(verbose)) cat('Reading data at the measure...\n')
  polder_data <- get_polder_scenario_data(
    name = name,
    case.list = case.list,
    case.desc = case.desc,
    param = param,
    ref.mID = ref.mID,
    ref.mID2 = ref.mID2,
    w.canal = TRUE, # to get data for the summary text
    sobek.project = sobek.project,
    master.tbl = master.tbl,
    verbose = verbose
  )
  data_tbl <- polder_data$data_tbl
  vol_tbl <- polder_data$vol_max_tbl
  rm(polder_data)
  cmp_vars <- unique(data_tbl[, get(compare.by)])
  if (isTRUE(cmp.sort)) cmp_vars <- sort(cmp_vars)
  #----read data for Bezugspegel 1 & 2----
  pegel1 <- parse_ref_id(ref.mID)
  pegel2 <- parse_ref_id(ref.mID2)
  pegel1$color <- paste(ifelse(param == 'discharge', 'Q', 'W'), toupper(pegel1$name))
  pegel2$color <- paste(ifelse(param == 'discharge', 'Q', 'W'), toupper(pegel2$name))
  if (!is.null(ref.mID)) {
    scheitel_max_c1 <- data_tbl[get(compare.by) == cmp_vars[[1]],
                                max(Bezugspegel)]
    scheitel_max_c2 <- data_tbl[get(compare.by) == cmp_vars[[2]],
                                max(Bezugspegel)]
    scheitel_ref_mID_delta <- scheitel_max_c1 - scheitel_max_c2
    scheitel_ref_mID_delta <- round(scheitel_ref_mID_delta, 2)
    # rounding value for discharge
    if (abs(scheitel_ref_mID_delta) > 2) {
      scheitel_ref_mID_delta <- round(scheitel_ref_mID_delta)
    }
    # get second Bezugspegel
    if (!is.null(ref.mID2)) {
      # get peak difference at the ref_measurement
      scheitel_max_c1 <- data_tbl[get(compare.by) == cmp_vars[[1]],
                                  max(Bezugspegel_2)]
      scheitel_max_c2 <- data_tbl[get(compare.by) == cmp_vars[[2]],
                                  max(Bezugspegel_2)]
      scheitel_ref_mID_delta2 <- scheitel_max_c1 - scheitel_max_c2
      scheitel_ref_mID_delta2 <- round(scheitel_ref_mID_delta2, 2)
      # rounding value for discharge
      if (abs(scheitel_ref_mID_delta2) > 2) {
        scheitel_ref_mID_delta2 <- round(scheitel_ref_mID_delta2)
      }
    }
  }
  # limit data to the peak value
  if (!is.null(peak.nday)) {
    stopifnot(is.numeric(peak.nday))
    if (isTRUE(peak.pegel) & !is.null(ref.mID)) {
      ts_max <- data_tbl[Bezugspegel == max(Bezugspegel), ts]
    } else{
      ts_max <- data_tbl[Nach == max(Nach), ts]
    }
    xlim_min <- ts_max - peak.nday * 24 * 3600
    xlim_max <- ts_max + peak.nday * 24 * 3600
    data_tbl <- data_tbl[ts >= xlim_min & ts <= xlim_max]
  }
  #----finding scheitel delta at the measure----
  if (isTRUE(delta.measure)) {
    # calculate diff between two max value (different moment)
    scheitel_max_c1 <- data_tbl[get(compare.by) == cmp_vars[[1]], max(Nach)]
    scheitel_max_c2 <- data_tbl[get(compare.by) == cmp_vars[[2]], max(Nach)]
    scheitel_measure_delta <- scheitel_max_c1 - scheitel_max_c2
    scheitel_measure_delta <- round(scheitel_measure_delta, 2)
    # rounding value for discharge
    if (abs(scheitel_measure_delta) > 2) {
      scheitel_measure_delta <- round(scheitel_measure_delta)
    }
  }
  #-----preparing plot-----
  if (isTRUE(verbose)) cat('Preparing graphic...\n')
  data_tbl_cols <- colnames(data_tbl)
  einlass_cols <- grep('Einlass', data_tbl_cols, value = TRUE)
  auslass_cols <- grep('Auslass', data_tbl_cols, value = TRUE)
  y2_axis <- q.in | q.out | (param == 'discharge' & w.canal)
  # in case there is a y2_axis
  #----processing y-axes limits----
  y1_cols <- c('Nach')
  if (!is.null(ref.mID)) y1_cols <- c('Nach', 'Bezugspegel')
  if (!is.null(ref.mID2)) y1_cols <- c('Nach', 'Bezugspegel', 'Bezugspegel_2')
  if (isTRUE(w.canal) & param == 'waterlevel') y1_cols <- c(y1_cols, 'W_innen')
  y1_max <- data_tbl[, max(.SD, na.rm = TRUE), .SDcols = y1_cols]
  y1_min <- data_tbl[, min(.SD, na.rm = TRUE), .SDcols = y1_cols]
  y1_length <- y1_max - y1_min
  y1_pretty <- pretty(y1_min:y1_max, 5, 5)
  # in case there is y2_axis
  if (isTRUE(y2_axis)) {
    y2_cols <- c(einlass_cols,
                 auslass_cols)[c(rep(q.in, length(einlass_cols)),
                                 rep(q.out, length(auslass_cols)))]
    if (isTRUE(w.canal) & param == 'discharge') {
      q.in <- FALSE
      q.out <- FALSE
      y2_cols <- 'W_innen'
    }
    y2_max <- data_tbl[, max(.SD, na.rm = TRUE), .SDcols = y2_cols]
    y2_min <- data_tbl[, min(.SD, na.rm = TRUE), .SDcols = y2_cols]
    y2_length <- y2_max - y2_min
    if (is.null(y2.scale)) {
      y1_length <- y1_max - y1_min
      y2_length <- y2_max - y2_min
      y2.scale <- y1_length * 0.75 * 1000 / y2_length
      for (i in 0:3) {
        if (abs(y2.scale) > 10 ** i)
          y2.scale <- round(y2.scale, -i)
      }
      y2.scale <- y2.scale / 1000
      y2_shift <- y1_min - y2_min * y2.scale
    } else{
      y2_shift <- round(y1_min - y2_min * y2.scale, 2)
    }
    if (y1_length < 10) {
      y1_max_1 <- y1_max * 100
      y1_min_1 <- y1_min * 100
      y1_pretty <- pretty(y1_min_1:y1_max_1, 5, 5)
      y1_pretty <- y1_pretty / 100
    } else{
      y1_pretty <- pretty(y1_min:y1_max, 5, 5)
    }
    if (!is.null(y2.tick1)) {
      y2_shift = y1_pretty[1] - y2.tick1 * y2.scale
    }
    y2_pretty <- (y1_pretty - y2_shift) / y2.scale
    if (0 %between% c(min(y2_pretty), max(y2_pretty))) {
      y2_pretty <- unique(sort(c(y2_pretty, 0)))
    }
    if (is.infinite(y2.scale)) {
      y1_max <- data_tbl[, max(.SD, na.rm = TRUE), .SDcols = y1_cols]
      y1_min <- data_tbl[, min(.SD, na.rm = TRUE), .SDcols = y1_cols]
      y1_pretty <- pretty(y1_min:y1_max, 5, 5)
      y2.scale = 1
      y2_shift = min(y1_pretty)
    }
  }
  # make sure y1_max is in the range of y1_pretty
  y1_tick_diff <- abs(y1_pretty[2] - y1_pretty[1])
  if (max(y1_pretty) < y1_max + y1_tick_diff) {
    y1_max <- y1_max + y1_tick_diff/2
    if (y1_length < 10) {
      y1_max_1 <- y1_max * 10
      y1_min_1 <- y1_min * 10
      y1_pretty <- pretty(y1_min_1:y1_max_1, 5, 5)
      y1_pretty <- y1_pretty / 10
    } else{
      y1_pretty <- pretty(y1_min:y1_max, 5, 5)
    }
  }
  #----initializing graphic----
  if (!is.null(ref.mID)) {
    g <- ggplot(data = data_tbl,
                mapping = aes(x = ts, linetype = !!ensym(compare.by))
    ) +
      geom_line(aes(y = Bezugspegel, color = pegel1$color),
                size = 1)
    if (!is.null(ref.mID2)) {
      g <- g +
        geom_line(aes(y = Bezugspegel_2, color = pegel2$color),
                  size = 1)
    }
  } else{
    g <- ggplot(data = data_tbl,
                mapping = aes(x = ts, linetype = !!ensym(compare.by))
    )
  }
  if (isTRUE(verbose)) cat('Adding hydrographs...\n')
  if (tolower(param) == 'discharge') {
    #----working with discharge----
    g <- g +
      geom_line(aes(y = Nach,  color = 'Q nach Maßnahme'),
                size = 1)
    if (isTRUE(w.canal)) {
      q.in <- FALSE
      # if parameter is discharge, move waterlevel to secondary axis
      g <- g + geom_line(aes(y = W_innen * y2.scale + y2_shift,
                             color = 'W in Maßnahme'),
                         size = 1)
      y2_name <- 'Wasserstand [m+NHN]'
    }
    if (isTRUE(q.in)) {
      y2_name <- 'Abfluss Einlass/Auslass [m³/s]'
      # adding Einlass lines
      if (length(einlass_cols) > 1) {
        id_data_einlass <- melt(data_tbl, measure.vars = einlass_cols,
                                variable.name = 'Einlass',
                                value.name = 'Q_Einlass')
        id_data_einlass[, Einlass := paste("Q", Einlass)]
        g <- g + geom_line(data = id_data_einlass,
                           aes(y = Q_Einlass * y2.scale + y2_shift,
                               color = Einlass),
                           size = 1)
      } else{
        g <- g + geom_line(aes(
          y = !!ensym(einlass_cols) * y2.scale + y2_shift,
          color = eval(paste('Q', einlass_cols))
        ),
        size = 1)
      }
    }
    if (isTRUE(q.out)) {
      if (length(auslass_cols) > 1) {
        id_data_auslass <- melt(data_tbl, measure.vars = auslass_cols,
                                variable.name = 'Auslass',
                                value.name = 'Q_Auslass')
        id_data_auslass[, Auslass := paste("Q", Auslass)]
        g <- g + geom_line(data = id_data_auslass,
                           aes(y = Q_Auslass * y2.scale + y2_shift,
                               color = Auslass),
                           size = 1)
      } else{
        g <- g + geom_line(aes(
          y = !!ensym(auslass_cols) * y2.scale + y2_shift,
          color = eval(paste('Q',  auslass_cols))
        ),
        size = 1)
      }
    }
    #----working with WL----
  } else {
    # adding W_innen directly to the graphic should not be a problem
    g <- g +
      geom_line(aes(y = Nach,  color = 'W nach Maßnahme'),
                size = 1)
    if (w.canal) {
      y2_name <- 'Wasserstand [m+NHN]'
      g <- g + geom_line(aes(y = W_innen,
                             color = 'W in Maßnahme'),
                         size = 1)
    }
    if (q.in) {
      einlass_cols <- grep('Einlass', colnames(data_tbl), value = TRUE)
      y2_name <- 'Abfluss Einlass/Auslass [m³/s]'
      # adding Einlass lines
      if (length(einlass_cols) > 1) {
        id_data_einlass <- melt(data_tbl, measure.vars = einlass_cols,
                                variable.name = 'Einlass',
                                value.name = 'Q_Einlass')
        id_data_einlass[, Einlass := paste('Q', Einlass)]
        g <- g + geom_line(data = id_data_einlass,
                           aes(y = Q_Einlass * y2.scale + y2_shift,
                               color = Einlass),
                           size = 1)
      } else{
        g <- g + geom_line(aes(
          y = !!ensym(einlass_cols) * y2.scale + y2_shift,
          color = eval(paste('Q', einlass_cols))
        ),
        size = 1)
      }
    }
    if (q.out) {
      y2_name <- 'Abfluss Einlass/Auslass [m³/s]'
      if (length(auslass_cols) > 1) {
        id_data_auslass <- melt(data_tbl, measure.vars = auslass_cols,
                                variable.name = 'Auslass',
                                value.name = 'Q_Auslass')
        id_data_auslass[, Auslass := paste('Q', Auslass)]
        g <- g + geom_line(data = id_data_auslass,
                           aes(y = Q_Auslass * y2.scale + y2_shift,
                               color = Auslass),
                           size = 1)
      } else{
        g <- g + geom_line(aes(
          y = !!ensym(auslass_cols) * y2.scale + y2_shift,
          color = eval(paste('Q', auslass_cols))
        ),
        size = 1)
      }
    }
  }
  g$labels$colour <- color.name
  g$labels$linetype <- lt.name
  #----calculating Max Volume----
  if (!is.null(polder.f)) {
    if (is.null(polder.z)) {
      data_tbl[, H_innen_max := max(W_innen, na.rm = TRUE) -
                 min(W_innen, na.rm = TRUE), by = case
               ]
      data_tbl[, Volume_max := round(H_innen_max * polder.f / 100, 2)]
    } else{
      data_tbl[, Volume_max := round(polder.z * polder.f / 100, 2)]
    }
  } else {
    data_tbl <- merge(data_tbl, vol_tbl[, c('case', 'Volume')], by = 'case', sort = FALSE)
  }
  #----annotating max value----
  if (verbose) cat('Adding text box...\n')
  for (i in einlass_cols) {
    einlass_max_col <- paste(i, 'Max', sep = '_')
    data_tbl[, eval(einlass_max_col) := max(get(i), na.rm = TRUE)]
  }
  data_tbl[, W_in_max := max(W_innen, na.rm = TRUE), by = case]
  # finding the locations on x-axis for the annotated text
  # get length of x_axis, then get text.pos of it
  data_tbl[, N := .N, by = case]
  data_tbl[, ts_min := shift(ts, n = floor(text.pos.x*N),
                             fill = NA, type = 'lead'),
           by = case]
  if (!is.null(h.lines)) {
    id_hlines <- data_tbl[, min(ts), by = case]
  }
  id_data_nrow <- data_tbl[, min(ts_min, na.rm = TRUE), by = case]
  colnames(id_data_nrow) <- c('case', 'ts')
  data_max <- merge(id_data_nrow, data_tbl, by = c('ts', 'case'), sort = FALSE)
  data_max[, W_in_max := round(W_in_max, 2)]
  data_max[, label := '']
  case_has_max <- data_max[W_in_max == max(W_in_max, na.rm = TRUE), case]
  data_max[case == case_has_max,
           label := paste(
             'V_max in Maßnahme: ', Volume, ' Mio. m³\n',
             'W_max in Maßnahme:   ', W_in_max, ' m + NHN\n',
             label,
             sep = "")
           ]
  for (i in einlass_cols) {
    einlass_max_col <- paste(i, 'Max', sep = '_')
    data_max[, eval(einlass_max_col) := round(as.numeric(get(einlass_max_col)), 1),
             .SDcols = einlass_max_col]
    data_max[case == case_has_max,
             label := paste(
               label,
               'Q_max durch ', i, ': ', get(einlass_max_col), ' m³/s\n',
               sep = "")
             ]
  }
  delta_unit <- ifelse(param == 'discharge', 'm³/s', 'm')
  if (isTRUE(delta.pegel) & !is.null(ref.mID)) {
    data_max[, label := paste(
      'Delta am ', pegel1$name, ": ", scheitel_ref_mID_delta, " ", delta_unit, " \n",
      label,
      sep = ""
    )]
    # adding delta ref.mID2
    if (!is.null(ref.mID2)) {
      data_max[, label := paste(
        'Delta am ', pegel2$name, ": ", scheitel_ref_mID_delta2, " ", delta_unit, " \n",
        label,
        sep = ""
      )]
    }
  }
  if (isTRUE(delta.measure)) {
    data_max[, label := paste(
      'Delta an der Maßnahme: ', scheitel_measure_delta, " ", delta_unit, " \n",
      label,
      sep = ""
    )]
  }
  # y position of the text block
  y1_pos_txt <- y1_pretty[(length(y1_pretty) - 1)]
  if (!is.null(text.pos.y)) {
    y1_pos_txt <- text.pos.y
    if (abs(text.pos.y) < 1.2) y1_pos_txt <- text.pos.y * (y1_max - y1_min) + y1_min
  }
  g <- g +
    geom_text(
      data    = data_max[case == case_has_max],
      mapping = aes(x = ts_min,
                    y = y1_pos_txt,
                    label = label),
      hjust = 0,
      vjust = 1
    )

  if (!is.null(h.lines)) {
    for (i in seq_along(h.lines)) {
      hline_label <- ifelse(is.null(names(h.lines[i])),
                            paste('V_', h.lines[[i]], sep = ""),
                            names(h.lines[i])
      )
      # adding new colume to id_hlines
      id_hlines[, eval(hline_label) := h.lines[[i]]]
      g <- g + geom_hline(yintercept = h.lines[[i]], linetype = 3)
    }
    id_hlines <- melt(id_hlines, id.vars = c('case', 'V1'))
    # merging with case table for facetting
    case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
    id_hlines <- merge(id_hlines, case_tbl, by = 'case', sort = FALSE)
    g <- g + geom_text(
      data    = id_hlines,
      # V1 is the colume name of the min (ts_hlines)
      mapping = aes(x = V1, y = value,
                    label = sub("^V_", "", variable)
      ),
      hjust   = 0,
      vjust = 0,
      check_overlap = TRUE
    )
  }
  if (isTRUE(y2_axis)) {
    if (y2.scale != 0) {
      y2_pretty <- round(y2_pretty, 2)
      g <- g +
        scale_y_continuous(
          breaks = y1_pretty,
          labels = fm_nr,
          sec.axis =
            sec_axis(trans = ~./y2.scale - y2_shift/y2.scale,
                     breaks = y2_pretty,
                     labels = fm_nr,
                     name = y2_name)
        )
    } else{
      # in this case, y2 is a zero h-line
      g <- g +
        scale_y_continuous(
          breaks = y1_pretty,
          labels = fm_nr,
          sec.axis =
            sec_axis(trans = ~./y2.scale - y2_shift/y2.scale,
                     # labels = fm_nr,
                     name = y2_name)
        )
    }
  }
  #----graphic layout----
  y1_label <- ifelse(param == 'discharge', 'Abfluss m³/s', 'Wasserstand [m+NHN]')
  if (is.null(plot.title)) {
    plot.title <- paste0(str_extract(y1_label, 'Abfluss|Wasserstand'),
                        ' Ganglinien für Maßnahme: ', name,
                        '. Hochwasser: ', data_tbl$hwe[[1]], ' ', data_tbl$vgf[[1]])
  }
  g <- g + theme_bw() +
    theme(
      legend.position = 'bottom',
      text = element_text(size = text.size),
      axis.text.x = element_text(angle = text.x.angle)
    ) +
    scale_x_datetime(
      name = 'Zeit',
      date_breaks = date.break,
      date_labels = date.label
    ) + ylab(y1_label) +
    ggtitle(plot.title)
  #----adding delta line beneath the main graphic----
  if (isTRUE(delta.line)) {
    delta_data <- dcast(data_tbl, ts ~ get(compare.by),
                        value.var = 'Nach')
    delta_data[, `Delta an der Maßnahme` := get(cmp_vars[1]) - get(cmp_vars[2])]
    delta_data[, eval(cmp_vars[1]) := NULL]
    delta_data[, eval(cmp_vars[2]) := NULL]
    if (!is.null(ref.mID)) {
      delta_p1 <- dcast(data_tbl, ts ~ get(compare.by),
                        value.var = 'Bezugspegel')
      delta_p1[, eval(paste('Delta am ', ref.mID_name)) :=
                 get(cmp_vars[1]) - get(cmp_vars[2])]
      delta_p1[, eval(cmp_vars[1]) := NULL]
      delta_p1[, eval(cmp_vars[2]) := NULL]
      delta_data <- merge(delta_data, delta_p1, by = 'ts',
                          sort = FALSE)
      if(!is.null(ref.mID2)) {
        delta_p2 <- dcast(data_tbl, ts ~ get(compare.by),
                          value.var = 'Bezugspegel_2')
        delta_p2[, eval(paste('Delta am ', ref.mID2_name)) :=
                   get(cmp_vars[1]) - get(cmp_vars[2])]
        delta_p2[, eval(cmp_vars[1]) := NULL]
        delta_p2[, eval(cmp_vars[2]) := NULL]
        delta_data <- merge(delta_data, delta_p2, by = 'ts',
                            sort = FALSE)
      }
    }
    delta_data <- melt(delta_data,
                       id.vars = 'ts', value.name = 'value',
                       variable.name = 'Delta')
    delta_title <- ifelse(param == 'discharge',
                          paste('Abfluss Differenz (',
                                cmp_vars[1], " - ", cmp_vars[2], ")",
                                sep = ""),
                          paste('Wasserstand Differenz (',
                                cmp_vars[1], " - ", cmp_vars[2], ")",
                                sep = ""))
    delta_ylab <- ifelse(param == 'discharge',
                         'Abfluss Differenz [m³/s]',
                         'Wasserstand Differenz [cm]'
    )
    if (param == 'waterlevel') delta_data[, value := round(value * 100, 1)]
    g2 <- ggplot(data = delta_data,
                 aes(x = ts, y = value, linetype = Delta)) +
      scale_x_datetime(name = 'Zeit', breaks = date.break,
                       date_labels = date.label) +
      geom_line(size = 1) +
      theme_bw() +
      theme(
        legend.position = 'bottom',
        text = element_text(size = text.size),
        axis.text.x = element_blank()
      ) +
      ggtitle(delta_title) + ylab(delta_ylab)
    g2$labels$linetype = 'Linienart'
    g <- cowplot::plot_grid(g, g2, ncol = 1, rel_heights = rel.heights,
                            align = 'v', axis = 'l')
  }
  if (verbose & y2_axis) {
    cat('tried with y2.scale = ', y2.scale,
        '. y2_shift = ', y2_shift,
        ". Use y2.tick1 and y2.scale to adjust y2-axis\n"
    )
  }

  return(g)
}
