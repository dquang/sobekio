#' Plot hydrographs for a measure with comparing by scenario (zustand)
#' @param name Name of the measure (with/without the measure)
#' @param case.list List of cases
#' @param case.desc Correct (according to case naming standard in NHWSP) version of case.list, it will be used for legend
#' @param sobek.project Path to sobek project
#' @param param 'Waterlevel' or 'Discharge'
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
  name = NULL,
  case.list = NULL,
  case.desc = case.list,
  sobek.project = NULL,
  master.tbl = NULL,
  param = 'discharge',
  q.in = FALSE,
  q.out = FALSE,
  # sum.q.in = FALSE,
  # sum.q.out = FALSE,
  w.canal = FALSE,
  ref.mID = NULL,
  ref2 = NULL,
  y2.scale = NULL,
  y2.tick1 = NULL,
  h.lines = NULL,
  peak.nday = NULL,
  peak.pegel = FALSE,
  delta.pegel = FALSE,
  delta.measure = TRUE,
  delta.line = FALSE,
  rel.heights = c(2, 0.7),
  compare.by = 'zustand',
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
  verbose = TRUE){
  #----checking input----
  # there should be only two cases
  stopifnot(length(unlist(case.list)) == 2)
  param = tolower(param)
  stopifnot(param %in% c('discharge', 'waterlevel'))
  stopifnot(!c(is.null(name), is.null(case.list), is.null(master.tbl),
               is.null(sobek.project)
  ))
  #----get id_data----
  if (isTRUE(verbose)) print('Reading data at the measure...')
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
  # read data for Bezugspegel
  if (!is.null(ref.mID)) {
    if (isTRUE(verbose))
      print('Reading data for ref.mID...')
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
  }
  if (!is.null(ref2)) {
    if (isTRUE(verbose))
      print('Reading data for ref.mID...')
    if (length(ref2) > 1) {
      ref2_id <- ref2[[1]]
      if (hasName(ref2, 'ID')) ref2_id <- ref2$ID
      ref2_name <- ref2[[2]]
      if (hasName(ref2, 'name')) ref2_name <- ref2$name
      # ref2_color <- ref2_name
      ref2_color <- paste(ifelse(param == 'discharge', 'Q', 'W'),
                             ref2_name)
      ref2_type <- ifelse(param == 'discharge', 'qID', 'wID')
      if (hasName(ref2, 'type'))
        ref2_type <- ref2$type
      ref2_args <- list(
        case.list = case.list,
        sobek.project = sobek.project,
        id_type = ref2_id,
        param = param,
        verbose = FALSE
      )
      names(ref2__args)[3] <- ref2_type
      ref2_data <- do.call(his_from_case, ref2_args)
    } else{
      ref2_data <- his_from_case(
        case.list = case.list,
        sobek.project = sobek.project,
        mID = ref2[[1]],
        param = param,
        verbose = FALSE
      )
      ref2_name <-
        ifelse(!is.null(names(ref2)), names(ref2),
               toupper(ref2[[1]]))
      ref2_color <- paste(ifelse(param == 'discharge', 'Q', 'W'),
                             ref2_name)
    }
    colnames(ref_mID) <- c('ts', 'Bezugsort', 'case')
    id_data <- merge(id_data, ref2_data, by = c('ts', 'case'))
    # get peak difference at the ref_measurement
    scheitel_max_c1 <- id_data[get(compare.by) == cmp_vars[[1]],
                               max(Bezugsort)]
    scheitel_max_c2 <- id_data[get(compare.by) == cmp_vars[[2]],
                               max(Bezugsort)]
    scheitel_ref2_delta <- scheitel_max_c1 - scheitel_max_c2
    scheitel_ref2_delta <- round(scheitel_ref2_delta, 2)
    # rounding value for discharge
    if (abs(scheitel_ref2_delta) > 2) {
      scheitel_ref2_delta <- round(scheitel_ref2_delta)
    }
  }
  # limit data to the peak value
  if (!is.null(peak.nday)){
    stopifnot(is.numeric(peak.nday))
    if (isTRUE(peak.pegel) & !is.null(ref.mID)){
      ts_max <- id_data[Bezugspegel == max(Bezugspegel), ts]
    } else{
      ts_max <- id_data[Nach == max(Nach), ts]
    }
    xlim_min <- ts_max - peak.nday * 24 * 3600
    xlim_max <- ts_max + peak.nday * 24 * 3600
    id_data <- id_data[ts >= xlim_min & ts <= xlim_max]
  }
  # finding scheitel delta at the measure
  if (isTRUE(delta.measure)){
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
  if (isTRUE(verbose)) print('Preparing graphic...')
  einlass_cols <- grep('Einlass', colnames(id_data), value = TRUE)
  auslass_cols <- grep('Auslass', colnames(id_data), value = TRUE)
  y2_axis <- isTRUE(q.in)|isTRUE(q.out)|(param == 'discharge' & isTRUE(w.canal))
  # in case there is a y2_axis
  #----processing y-axes limits----
  if (isTRUE(y2_axis)) {
    y1_cols <- c('Nach')
    if (!is.null(ref.mID)) y1_cols <- c('Nach', 'Bezugspegel')
    if (isTRUE(w.canal) & param == 'waterlevel') y1_cols <- c(y1_cols, 'W_innen')
    y2_cols <- c(einlass_cols,
                 auslass_cols)[c(rep(q.in, length(einlass_cols)),
                                 rep(q.out, length(auslass_cols)))]
    if (isTRUE(w.canal) & param == 'discharge') {
      q.in <- FALSE
      q.out <- FALSE
      y2_cols <- 'W_innen'
    }
    y1_max <- id_data[, max(.SD, na.rm = TRUE), .SDcols = y1_cols]
    y1_min <- id_data[, min(.SD, na.rm = TRUE), .SDcols = y1_cols]
    y2_max <- id_data[, max(.SD, na.rm = TRUE), .SDcols = y2_cols]
    y2_min <- id_data[, min(.SD, na.rm = TRUE), .SDcols = y2_cols]
    y1_length <- y1_max - y1_min
    y2_length <- y2_max - y2_min
    if (is.null(y2.scale)) {
      y1_length <- y1_max - y1_min
      y2_length <- y2_max - y2_min
      y2.scale <- y1_length * 0.75 * 1000 / y2_length
      for (i in 0:3) {
        # if (abs(y2.scale) > 1) y2.scale <- round(y2.scale)
        if (abs(y2.scale) > 10 ** i)
          y2.scale <- round(y2.scale, -i)
      }
      y2.scale <- y2.scale / 1000
      y2_shift <- y1_min - y2_min * y2.scale
      # if (y1_max < y2_max * y2.scale + y2_shift)
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
    if (0 %between% c(min(y2_pretty), max(y2_pretty))){
      y2_pretty <- unique(sort(c(y2_pretty, 0)))
    }
  } else{
    # in case there is no y2_axis
    y1_cols <- c('Nach', 'Vor')
    if (!is.null(ref.mID))
      y1_cols <- c('Nach', 'Vor', 'Bezugspegel')
    if (isTRUE(w.canal) &
        param == 'waterlevel')
      y1_cols <- c(y1_cols, 'W_innen')
    y1_max <- id_data[, max(.SD, na.rm = TRUE), .SDcols = y1_cols]
    y1_min <- id_data[, min(.SD, na.rm = TRUE), .SDcols = y1_cols]
    y1_pretty <- pretty(y1_min:y1_max, 5, 5)
  }
  if (is.infinite(y2.scale)){
    y1_max <- id_data[, max(.SD, na.rm = TRUE), .SDcols = y1_cols]
    y1_min <- id_data[, min(.SD, na.rm = TRUE), .SDcols = y1_cols]
    y1_pretty <- pretty(y1_min:y1_max, 5, 5)
    y2.scale = 1
    y2_shift = min(y1_pretty)
  }
  # make sure y1_max is in the range of y1_pretty
  y1_tick_diff <- abs(y1_pretty[2] - y1_pretty[1])
  if (max(y1_pretty) < y1_max + y1_tick_diff){
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
  if (!is.null(ref.mID)){
    g <- ggplot(data = id_data,
                mapping = aes(x = ts, linetype = !!ensym(compare.by))
    ) +
      geom_line(aes(
        y = Bezugspegel,
        color = ref.mID_color
      ),
      size = 1)
  } else{
    g <- ggplot(data = id_data,
                mapping = aes(x = ts, linetype = !!ensym(compare.by))
    )
  }
  if (isTRUE(verbose)) print('Adding hydrographs...')
  if (tolower(param) == 'discharge'){
    #----working with discharge----
    g <- g +
      geom_line(aes(y = Nach,  color = 'Q nach Maßnahme'),
                size = 1)
    if (isTRUE(w.canal)){
      q.in <- FALSE
      # y2_min <- min(id_data$W_innen, na.rm = TRUE)
      # if (y2_max - y2_min > 10) {
      #   y2_min <- round(y2_min, -1)
      # } else {
      #   y2_min <- floor(y2_min)
      # }
      # if (y2_min*y2.scale != y1_min) {
      #   y2_shift <- floor(y2_shift - y2_min*y2.scale)
      # }
      # if parameter is discharge, move waterlevel to secondary axis
      g <- g + geom_line(aes(y = W_innen * y2.scale + y2_shift,
                             color = 'W in Maßnahme'),
                         size = 1)
      y2_name <- 'Wasserstand (m+NHN)'
    }
    if (isTRUE(q.in)){
      y2_name <- 'Abfluss Einlass/Auslass (m³/s)'
      # y2_min <- id_data[, .SD,
      #                   .SDcols = einlass_cols
      #                   ] %>%
      #   min(na.rm = TRUE)
      # if (y2_max - y2_min > 10) {
      #   y2_min <- round(y2_min, -1)
      # } else {
      #   y2_min <- floor(y2_min)
      # }
      # if (y2_min*y2.scale != y1_min) {
      #   y2_shift <- floor(y2_shift - y2_min*y2.scale)
      # }
      # adding Einlass lines
      if (length(einlass_cols) > 1){
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
    if (isTRUE(q.out)){
      # y2_min <- id_data[, .SD,
      #                   .SDcols = c(einlass_cols, auslass_cols)
      #                   ] %>%
      #   min(na.rm = TRUE)
      # if (y2_max - y2_min > 10) {
      #   y2_min <- round(y2_min, -1)
      # } else {
      #   y2_min <- floor(y2_min)
      # }
      # if (y2_min*y2.scale != y1_min) {
      #   y2_shift <- floor(y2_shift - y2_min*y2.scale)
      # }
      if (length(auslass_cols) > 1){
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
    if (isTRUE(w.canal)){
      y2_name <- 'Wasserstand (m+NHN)'
      # if(is.null(ref.mID)){
      #   y1_cols <- c('Nach', 'Vor', 'W_innen')
      # } else{
      #   y1_cols <- c('Nach', 'Vor', 'W_innen', 'Bezugspegel')
      # }
      # y1_max <- id_data[, max(.SD, na.rm = TRUE),
      #                   .SDcols = y1_cols]
      # y1_min <- id_data[, min(.SD, na.rm = TRUE),
      #                   .SDcols = c('Nach', 'Vor', 'W_innen')]
      # y2_max <- ceiling(y1_max/y2.scale)
      # y1_pretty <-  pretty(y1_min:y1_max, 5, 5)
      g <- g + geom_line(aes(y = W_innen,
                             color = 'W in Maßnahme'),
                         size = 1)
    }
    if (isTRUE(q.in)){
      einlass_cols <- grep('Einlass', colnames(id_data), value = TRUE)
      y2_name <- 'Abfluss Einlass/Auslass (m³/s)'
      # y2_min <- id_data[, .SD,
      #                   .SDcols = einlass_cols
      #                   ] %>%
      #   min(na.rm = TRUE)
      # if (y2_max - y2_min > 10) {
      #   y2_min <- round(y2_min, -1)
      # } else {
      #   y2_min <- floor(y2_min)
      # }
      # if (y2_min*y2.scale != y1_min) {
      #   y2_shift <- floor(y2_shift - y2_min*y2.scale)
      # }
      # adding Einlass lines
      if (length(einlass_cols) > 1){
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
    if (isTRUE(q.out)){
      y2_name <- 'Abfluss Einlass/Auslass (m³/s)'
      # y2_min <- id_data[, .SD,
      #                   .SDcols = c(einlass_cols, auslass_cols)
      #                   ] %>%
      #   min(na.rm = TRUE)
      # if (y2_max - y2_min > 10) {
      #   y2_min <- round(y2_min, -1)
      # } else {
      #   y2_min <- floor(y2_min)
      # }
      # if (y2_min*y2.scale != y1_min) {
      #   y2_shift <- floor(y2_shift - y2_min*y2.scale)
      # }
      if (length(auslass_cols) > 1){
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
  if (isTRUE(verbose)) print('Calculating volume...')
  if(!is.null(polder.f)){
    if(is.null(polder.z)){
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
  if (isTRUE(verbose)) print('Adding text box...')
  # print(einlass_cols)
  # if (isTRUE(q.in)){
  for (i in einlass_cols) {
    einlass_max_col <- paste(i, 'Max', sep = '_')
    id_data[, eval(einlass_max_col) := max(get(i), na.rm = TRUE)]
  }
  # }
  id_data[, W_in_max := max(W_innen, na.rm = TRUE), by = case]
  # finding the locations on x-axis for the annotated text
  # get length of x_axis, then get text.pos of it
  id_data[, N := .N, by = case]
  id_data[, ts_min := shift(ts, n = floor(text.pos.x*N),
                            fill = NA, type = 'lead'),
          by = case]
  if(!is.null(h.lines)){
    id_hlines <- id_data[, min(ts), by = case]
  }
  id_data_nrow <- id_data[, min(ts_min, na.rm = TRUE), by = case]
  colnames(id_data_nrow) <- c('case', 'ts')
  id_max <- merge(id_data_nrow, id_data, by = c('ts', 'case'), sort = FALSE)
  # id_max[, Q_in_max := round(Q_in_max)]
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
  for (i in einlass_cols){
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
  if (isTRUE(delta.pegel) & !is.null(ref.mID)){
    id_max[, label := paste(
      'Delta am ', ref.mID_name, ": ", scheitel_ref_mID_delta, " ", delta_unit, " \n",
      label,
      sep = ""
    )]
  }
  if (isTRUE(delta.measure)){
    id_max[, label := paste(
      'Delta an der Maßnahme: ', scheitel_measure_delta, " ", delta_unit, " \n",
      label,
      sep = ""
    )]
  }
  # y position of the text block
  y1_pos_txt <- y1_pretty[(length(y1_pretty)-1)]
  if (!is.null(text.pos.y)){
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
  
  if (!is.null(h.lines)){
    # id_hlines with 2 cols: V1, case
    for (i in seq_along(h.lines)){
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
  if (isTRUE(y2_axis)){
    if (y2.scale != 0){
      g <- g +
        scale_y_continuous(
          breaks = y1_pretty,
          sec.axis =
            sec_axis(trans = ~./y2.scale - y2_shift/y2.scale,
                     breaks = y2_pretty,
                     labels = round(y2_pretty, 2),
                     name = y2_name)
        )
    } else{
      # in this case, y2 is a zero h-line
      g <- g +
        scale_y_continuous(
          breaks = y1_pretty,
          sec.axis =
            sec_axis(trans = ~./y2.scale - y2_shift/y2.scale,
                     # breaks = y2_pretty,
                     # labels = round(y2_pretty, 2),
                     name = y2_name)
        )
    }
  }
  #----graphic layout----
  y1_label <- ifelse(param == 'discharge', 'Abfluss m³/s', 'Wasserstand (m+NHN)')
  if (is.null(plot.title)){
    plot.title <- paste(str_extract(y1_label, 'Abfluss|Wasserstand'),
                        ' Ganglinien für Maßnahme: ', name,
                        '. Hochwasser: ', case_tbl$hwe[[1]],
                        sep = '')
  }
  g <- g + theme_bw() +
    theme(
      legend.position = 'bottom',
      text = element_text(size = text.size),
      axis.text.x = element_text(angle = text.x.angle)
    )+
    scale_x_datetime(
      name = 'Zeit',
      date_breaks = date.break,
      date_labels = date.label
    ) + ylab(y1_label) +
    ggtitle(plot.title)
  #----adding delta line beneath the main graphic----
  if (isTRUE(delta.line)){
    if(!is.null(ref.mID)){
      delta_data <- dcast(id_data, ts ~ get(compare.by),
                          value.var = 'Bezugspegel')
      delta_data[, eval(paste('Delta am ', ref.mID_name)) :=
                   get(cmp_vars[1]) - get(cmp_vars[2])]
      delta_data[, eval(cmp_vars[1]) := NULL]
      delta_data[, eval(cmp_vars[2]) := NULL]
      # if (param == 'waterlevel'){
      delta_mea <- dcast(id_data, ts ~ get(compare.by),
                         value.var = 'Nach')
      # delta_data[, delta_pos := 'Delta an der Maßnahme']
      delta_mea[, `Delta an der Maßnahme` := get(cmp_vars[1]) - get(cmp_vars[2])]
      delta_mea[, eval(cmp_vars[1]) := NULL]
      delta_mea[, eval(cmp_vars[2]) := NULL]
      delta_data <- merge(delta_data, delta_mea, by = 'ts',
                          sort = FALSE)
      # }
    } else{
      delta_data <- dcast(id_data, ts ~ get(compare.by),
                          value.var = 'Nach')
      # delta_data[, delta_pos := 'Delta an der Maßnahme']
      delta_data[, `Delta an der Maßnahme` := get(cmp_vars[1]) - get(cmp_vars[2])]
      delta_data[, eval(cmp_vars[1]) := NULL]
      delta_data[, eval(cmp_vars[2]) := NULL]
      # delta_data <- merge(delta_data, delta_mea, by = 'ts',
      # sort = FALSE)
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
                         'Abfluss Differenz (m³/s)',
                         'Wasserstand Differenz (cm)'
    )
    if (param == 'waterlevel') delta_data[, value := round(value * 100, 1)]
    g2 <- ggplot(data = delta_data,
                 aes(x = ts, y = value, linetype = Delta))+
      scale_x_datetime(name = 'Zeit', breaks = date.break,
                       date_labels = date.label)+
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
    print(paste('tried with y2.scale = ', y2.scale,
                '. y2_shift = ', y2_shift,
                ". Use y2.tick1 and y2.scale to adjust y2-axis",
                sep = ""))
  }
  
  return(g)
}

g_tmp <-  plot_longprofile(
  river = 'Rhein',
  from.km = 750,
  to.km = 751,
  case.list = c(
    'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
    'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt'
    ),
  case.desc = c(
    'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
    'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt'
    ),
  sobek.project = 'c:/rhein.lit',
  cmp.sort = FALSE,
  compare.by = 'vgf',
  delta = TRUE,
  lt.by = 'vgf',
  param = 'waterlevel',
  # talweg = TRUE,
  master.tbl = rhein_tbl
  )
g_tmp
View(g_tmp$data)
