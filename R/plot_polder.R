#' Plot hydrographs for a measure with comparing by scenario (zustand)
#' @param name Name of the measure (with/without the measure)
#' @param case.list List of cases
#' @param case.desc Correct (according to case naming standard in NHWSP) version of case.list, it will be used for legend
#' @param sobek.project Path to sobek project
#' @param master.tbl Table of ID Coding of the sobek network
#' @param param 'Waterlevel' or 'Discharge' Hydrograph
#' @param q.in Logical. Should discharge through the inlet be plotted?
#' @param q.out Logical. Should discharge through the outlet be plotted?
#' @param w.canal Logical. Should Wt line inside the measure be plotted?
#' @param ref.mID ID of Bezugspegel
#' @param y2.scale Scaling between main and secondary y-axes. This is an important paramter. If the line for secondary axis is too big, try to change y2.scale
#' @param h.lines List of (name = value) to be displayed as horizontal line
#' @param compare.by Should the line be compare by 'case' or by 'location'
#' @param facet.by Should the graphic be facetted. Default by 'zustand'
#' @param plot.title Title of the graphic
#' @param lt.name Name of the linetype legend
#' @param color.name Name of the color legend
#' @param text.pos.x Position of the text block on x-axis, relative value
#' @param text.pos.y Position of the text block on y1-axis, absolute or relative value
#' @param date.break Breaks for x-axis ('n days', '12 hours'...)
#' @param date.label Label format for x-axis. Default dd.mm.yy
#' @param text.size Text size for the whole graphic. Default 12
#' @param text.x.angle Angle of x-axis text. Default 0
#' @param polder.f Area of the measure, for calculating Volume
#' @param polder.z Bottom level of the measure for calculating Volume. Give 'auto' for getting the minimum waterlevel in canal value. In this case, make sure the canal is completely dry at T0
#' @param versbose Boring? Should some message be displayed?
#' @return A ggplot2 graphic
#' @export
plot_polder <- function(
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
  y2.scale = 25,
  h.lines = NULL,
  compare.by = 'zustand',
  facet.by =  'hwe',
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

  # check input
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
    upstream = TRUE,
    sobek.project = sobek.project,
    master.tbl = master.tbl,
    verbose = verbose
  )
  case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
  # adding case description columns, using for linetype later on
  id_data <- merge(id_data, case_tbl, by = 'case', sort = FALSE)
  # finding scheitel delta at the measure
  #-----preparing plot-----
  if (isTRUE(verbose)) print('Preparing graphic...')
  y1_label <- ifelse(param == 'discharge', 'Abfluss m³/s', 'Wasserstand (m+NHN)')
  delta_unit <- ifelse(param == 'discharge', 'm³/s', 'm')
  cols_not_plot <- c('case_desc', 'Nach', 'Vor', 'ts', 'case', 'zustand',
                     'zielpegel', 'hwe', 'notiz', 'vgf')
  y1_max <- id_data[, max(.SD, na.rm = TRUE), .SDcols = c('Nach', 'Vor')]
  y1_min <- id_data[, min(.SD, na.rm = TRUE), .SDcols = c('Nach', 'Vor')]
  y1_pretty <-  pretty(y1_min:y1_max, 5, 5)
  y2_max <- y1_max/y2.scale
  y2_shift <- y1_pretty[1]
  y2_min <- y2_shift/y2.scale
  if (y2_max - y2_min > 10) {
    y2_min <- round(y2_min, -1)
  } else {
    y2_min <- floor(y2_min)
  }
  # adding line from Bezugspegel and initializing the graphic
  if (!is.null(ref.mID)){
    if (isTRUE(verbose)) print('Reading data for ref.mID...')
    if (length(ref.mID) > 1){
      ref.mID_id <- ref.mID[[1]]
      if (hasName(ref.mID, 'ID')) ref.mID_id <- ref.mID$ID
      ref.mID_name <- ref.mID[[2]]
      if (hasName(ref.mID, 'name')) ref.mID_name <- ref.mID$name
      ref.mID_color <- ref.mID_name
      ref.mID_type <- ifelse(param == 'discharge', 'qID', 'wID')
      if (hasName(ref.mID, 'type')) ref.mID_type <- ref.mID$type
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
      ref_mID <- his_from_case(case.list = case.list,
                               sobek.project = sobek.project,
                               mID = ref.mID[[1]], param = param,
                               verbose = FALSE)
      ref.mID_color <- ifelse(!is.null(names(ref.mID)), names(ref.mID), 
                              toupper(ref.mID[[1]]))
    }
    colnames(ref_mID) <- c('ts', 'Bezugspegel', 'case')
    id_data <- merge(id_data, ref_mID, by = c('ts', 'case'))
    y1_min <- min(y1_min, ref_mID$Bezugspegel, na.rm = TRUE)
    y1_max <- max(y1_max, ref_mID$Bezugspegel, na.rm = TRUE)
    y1_pretty <-  pretty(y1_min:y1_max, 5, 5)
    g <- ggplot(data = id_data,
                mapping = aes(x = ts,
                              linetype = !!ensym(compare.by))
                ) +
      geom_line(aes(y = Bezugspegel,
                    color = eval(ref.mID_color)
      ),
      size = 1)
  } else{
    g <- ggplot(data = id_data,
                mapping = aes(x = ts, linetype = !!ensym(compare.by))
                )
  }
  if (isTRUE(verbose)) print('Adding hydrographs...')
  # if parameter is discharge, move waterlevel to secondary axis
  if (tolower(param) == 'discharge'){
    g <- g +
      geom_line(aes(
        y = Nach,
        color = 'Nach der Maßnahme'
      ),
      size = 1) +
      geom_line(aes(
        y = Vor,
        color = 'Vor der Maßnahme'
      ),
      size = 1)
    if (isTRUE(w.canal)){
      q.in <- FALSE
      y2_min <- min(id_data$W_innen, na.rm = TRUE)
      if (y2_max - y2_min > 10) {
        y2_min <- round(y2_min, -1)
      } else {
        y2_min <- floor(y2_min)
      }
      if (y2_min*y2.scale != y1_min) {
        y2_shift <- floor(y2_shift - y2_min*y2.scale)
      }
      g <- g + geom_line(aes(y = W_innen * y2.scale + y2_shift,
                             color = 'In der Maßnahme'),
                         size = 1)
      y2_name <- 'Wasserstand (m+NHN)'
    }
    einlass_cols <- grep('Einlass', colnames(id_data), value = TRUE)
    auslass_cols <- grep('Auslass', colnames(id_data), value = TRUE)
    if (isTRUE(q.in)){
      y2_name <- 'Abfluss Einlass/Auslass (m³/s)'
      y2_min <- id_data[, .SD,
                        .SDcols = einlass_cols
                        ] %>%
        min(na.rm = TRUE)
      if (y2_max - y2_min > 10) {
        y2_min <- round(y2_min, -1)
      } else {
        y2_min <- floor(y2_min)
      }
      if (y2_min*y2.scale != y1_min) {
        y2_shift <- floor(y2_shift - y2_min*y2.scale)
      }
      # adding Einlass lines
      if (length(einlass_cols) > 1){
        id_data_einlass <- melt(id_data, measure.vars = einlass_cols,
                                variable.name = 'Einlass',
                                value.name = 'Q_Einlass')
        g <- g + geom_line(data = id_data_einlass,
                           aes(y = Q_Einlass * y2.scale + y2_shift,
                               color = Einlass),
                           size = 1)
      } else{
        g <- g + geom_line(aes(
          y = !!ensym(einlass_cols) * y2.scale + y2_shift,
          color = eval(einlass_cols)
        ),
        size = 1)
      }
      if (isTRUE(q.out)){
        y2_min <- id_data[, .SD,
                          .SDcols = c(einlass_cols, auslass_cols)
                          ] %>%
          min(na.rm = TRUE)
        if (y2_max - y2_min > 10) {
          y2_min <- round(y2_min, -1)
        } else {
          y2_min <- floor(y2_min)
        }
        if (y2_min*y2.scale != y1_min) {
          y2_shift <- floor(y2_shift - y2_min*y2.scale)
        }
        if (length(auslass_cols) > 1){
          id_data_auslass <- melt(id_data, measure.vars = auslass_cols,
                                  variable.name = 'Auslass',
                                  value.name = 'Q_Auslass')
          g <- g + geom_line(data = id_data_auslass,
                             aes(y = Q_Auslass * y2.scale + y2_shift,
                                 color = Auslass),
                             size = 1)
        } else{
          g <- g + geom_line(aes(
            y = !!ensym(auslass_cols) * y2.scale + y2_shift,
            color = eval(auslass_cols)
          ),
          size = 1)
        }
      }
    }
    #----working with WL----
  } else {
    # adding W_innen directly to the graphic should not be a problem
    g <- g +
      geom_line(aes(y = Nach,  color = 'Nach der Maßnahme'),
                size = 1) +
      geom_line(aes(y = Vor,  color = 'Vor der Maßnahme'),
                size = 1)
    if (isTRUE(w.canal)){
      y2_name <- 'Wasserstand (m+NHN)'
      y1_max <- id_data[, max(.SD, na.rm = TRUE),
                        .SDcols = c('Nach', 'Vor', 'W_innen')]
      y1_min <- id_data[, min(.SD, na.rm = TRUE),
                        .SDcols = c('Nach', 'Vor', 'W_innen')]
      y2_max <- ceiling(y1_max/y2.scale)
      y1_pretty <-  pretty(y1_min:y1_max, 5, 5)
      g <- g + geom_line(aes(y = W_innen,
                             color = 'In der Maßnahme'),
                         size = 1)
    }
    if (isTRUE(q.in)){
      einlass_cols <- grep('Einlass', colnames(id_data), value = TRUE)
      y2_name <- 'Abfluss Einlass/Auslass (m³/s)'
      y2_min <- id_data[, .SD,
                        .SDcols = einlass_cols
                        ] %>%
        min(na.rm = TRUE)
      if (y2_max - y2_min > 10) {
        y2_min <- round(y2_min, -1)
      } else {
        y2_min <- floor(y2_min)
      }
      if (y2_min*y2.scale != y1_min) {
        y2_shift <- floor(y2_shift - y2_min*y2.scale)
      }
      # adding Einlass lines
      if (length(einlass_cols) > 1){
        id_data_einlass <- melt(id_data, measure.vars = einlass_cols,
                                variable.name = 'Einlass',
                                value.name = 'Q_Einlass')
        g <- g + geom_line(data = id_data_einlass,
                           aes(y = Q_Einlass * y2.scale + y2_shift,
                               color = Einlass),
                           size = 1)
      } else{
        g <- g + geom_line(aes(
          y = !!ensym(einlass_cols) * y2.scale + y2_shift,
          color = eval(einlass_cols)
        ),
        size = 1)
      }
      if (isTRUE(q.out)){
        auslass_cols <- grep('Auslass', colnames(id_data), value = TRUE)
        y2_min <- id_data[, .SD,
                          .SDcols = c(einlass_cols, auslass_cols)
                          ] %>%
          min(na.rm = TRUE)
        if (y2_max - y2_min > 10) {
          y2_min <- round(y2_min, -1)
        } else {
          y2_min <- floor(y2_min)
        }
        if (y2_min*y2.scale != y1_min) {
          y2_shift <- floor(y2_shift - y2_min*y2.scale)
        }
        if (length(auslass_cols) > 1){
          id_data_auslass <- melt(id_data, measure.vars = auslass_cols,
                                  variable.name = 'Auslass',
                                  value.name = 'Q_Auslass')
          g <- g + geom_line(data = id_data_auslass,
                             aes(y = Q_Auslass * y2.scale + y2_shift,
                                 color = Auslass),
                             size = 1)
        } else{
          g <- g + geom_line(aes(
            y = !!ensym(auslass_cols) * y2.scale + y2_shift,
            color = eval(auslass_cols)
          ),
          size = 1)
        }
      }
    }
  }
  g$labels$colour <- color.name
  g$labels$linetype <- lt.name
  # calculating Max Volume
  if (isTRUE(verbose)) print('Calculating volume...')
  if(!is.null(polder.f)){
    if(is.null(polder.z)){
      id_data[, H_innen_max := max(W_innen, na.rm = TRUE) -
                min(W_innen, na.rm = TRUE), by = case]
      id_data[, Volume_max := round(H_innen_max * polder.f / 100, 2)]
    } else{
      id_data[, Volume_max := round(polder.z * polder.f / 100, 2)]
    }
  } else {
    id_vol_data <- get_polder_volume(
      name = name,
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
  for (i in einlass_cols){
    einlass_max_col <- paste(i, 'Max', sep = '_')
    id_data[, eval(einlass_max_col) := max(get(i), na.rm = TRUE), by = case]
  }
  id_data[, W_in_max := max(W_innen, na.rm = TRUE), by = case]
  # finding the locations on x-axis for the annotated text
  # get length of x_axis, then get text.pos of it
  id_data[, N := .N, by = case]
  id_data[, ts_min := shift(ts,
                            n = floor(text.pos.x * N),
                            fill = NA,
                            type = 'lead'),
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
  # case_has_max <- id_max[W_in_max == max(W_in_max, na.rm = TRUE), case]
  id_max[,
         label := paste(
           'Volume Max: ', Volume_max, ' Mio. m³\n',
           'W_in Max:   ', W_in_max, ' m + NHN\n',
           label,
           sep = "")
         ]
  for (i in einlass_cols){
    einlass_max_col <- paste(i, 'Max', sep = '_')
    id_max[, eval(einlass_max_col) := round(as.numeric(get(einlass_max_col)), 1),
           .SDcols = einlass_max_col, by = case]
    id_max[,
           label := paste(
             label,
             'Q_max durch ', i, ': ', get(einlass_max_col), ' m³/s\n',
             sep = "")
           ]
  }
  # removing 'Inf' in id_max
  id_max[, label := str_replace_all(label, "-*Inf.*\n", "k.A.\n")]
  id_max[near(Volume_max, 0, tol = 0.011), label := '']
  # y position of the text block
  y1_pos_txt <- y1_pretty[(length(y1_pretty)-1)]
  if (!is.null(text.pos.y)){
    y1_pos_txt <- text.pos.y
    if (abs(text.pos.y) < 1.2) y1_pos_txt <- text.pos.y * (y1_max - y1_min) + y1_min
  }
  g <- g +
    geom_text(
      data    = id_max,
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
  if ((param == 'discharge' & isTRUE(w.canal))|isTRUE(q.in)){
    y2_pretty <- (y1_pretty - y2_shift)/y2.scale
    g <-  g +
      scale_y_continuous(
        breaks = y1_pretty,
        sec.axis =
          sec_axis(trans = ~.*1/y2.scale - y2_shift/y2.scale,
                   breaks = y2_pretty,
                   labels = round(y2_pretty, 2),
                   name = y2_name)
      )
  }
  #----graphic layout----
  if (is.null(plot.title)){
    plot.title <- paste(str_extract(y1_label, 'Abfluss|Wasserstand'),
                        ' Ganglinien für Maßnahme: ', name, 
                        sep = '')
  }
  g <- g + theme_bw() +
    theme(
      legend.position = 'bottom',
      text = element_text(size = text.size),
      axis.text.x = element_text(angle = text.x.angle)
      )+
    scale_x_datetime(
      date_breaks = date.break,
      date_labels = date.label
      )+
    ylab(y1_label) + xlab('Zeit')+
    ggtitle(plot.title)
  if(!is.null(facet.by)){
    g <- g + facet_grid(rows = ensym(facet.by), scales = 'free_x')
  }

  return(g)
}
