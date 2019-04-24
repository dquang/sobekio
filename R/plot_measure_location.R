#' Plot hydrographs for a measure with comparing by location (vor/nach)
#' @param name Name of the measure (with/without the measure)
#' @param case.list Name of 2 cases
#' @param case.desc = Short version of case.list, it will be used for legend
#' @param param 'Waterlevel' or 'Discharge' Hydrograph
#' @param y2.scale Scaling between main and secondary y-axes. This is an important paramter. If the line for secondary axis is too big, try to change y2.scale
#' @param sobek.project Path to sobek project
#' @param ref.mID ID of Bezugspegel
#' @param Q.zu Logical. Should discharge through the inlet be plotted?
#' @param Q.ab Logical. Should discharge through the outlet be plotted?
#' @param W.innen Logical. Should Wt line inside the measure be plotted?
#' @param delta.pegel Logical. Should peak delta at ref.mID be displayed
#' @param delta.measure Logical. Should peak delta at the measure be displayed
#' @param polder.F Area of the measure, for calculating Volume
#' @param polder.Z Bottom level of the measure for calculating Volume. Give 'auto' for getting the minimum waterlevel in canal value. In this case, make sure the canal is completely dry at T0
#' @param h.lines list of (name = value) to be displayed as horizontal line
#' @param text.pos.x Positioning of the text block as relative value to the length of x-axis
#' @param v.just Justification of the text block
#' @param zoom Should be whole graphic zoom to n days around the peak?
#' @param master.tbl Table of ID Coding of the sobek network
#' @return A ggplot2 graphic
#' @export
plot_measure_location <- function(
  name = NULL,
  case.list = NULL,
  case.desc = NULL,
  param = 'waterlevel',
  y2.scale = 25,
  sobek.project = NULL,
  ref.mID = NULL,
  Q.zu = FALSE,
  Q.ab = FALSE,
  W.innen = FALSE,
  delta = FALSE,
  V.max = TRUE,
  polder.F = NULL,
  polder.Z = NULL,
  h.lines = NULL,
  text.pos = 0.01,
  v.just = 1,
  facet.by = 'case',
  zoom = NULL,
  master.tbl = NULL){
  # setting for display different lines on different graphics
  if (isTRUE(Q.zu)) delta <- FALSE
  param = tolower(param)
  stopifnot(param %in% c('discharge', 'waterlevel'))
  stopifnot(!c(is.null(name), is.null(case.list), is.null(master.tbl),
               is.null(sobek.project)
  ))
  # get id_data
  id_data <- .get_data_for_cases(
    name = name,
    case.list = case.list,
    case.desc = case.desc,
    param = param,
    sobek.project = sobek.project,
    master.tbl = master.tbl,
    verbose = verbose
  )
  # case_tbl <- .parsing_case_name(case.desc = case.desc, orig.name = case.list)
  # adding case description columns, using for linetype later on
  # id_data <- merge(id_data, case_tbl, by = 'case')
  # finding scheitel delta at the measure
  y1_label <- ifelse(param == 'discharge', 'Abfluss m³/s', 'Wasserstand (m+NHN)')
  delta_unit <- ifelse(param == 'discharge', 'm³/s', 'm')
  # line_type <- ifelse(param == 'discharge', 'Abfluss', 'Wasserstand')
  y1_max <- id_data[, max(.SD, na.rm = TRUE), .SDcols = c('Nach', 'Vor')]
  y1_min <- id_data[, min(.SD, na.rm = TRUE), .SDcols = c('Nach', 'Vor')]
  y2_min <- id_data[, min(.SD, na.rm = TRUE),
                    .SDcols = -c('Nach', 'Vor', 'ts', 'case')]
  y1_pretty <-  pretty(y1_min:y1_max, 5, 5)
  y2_max <- y1_max/y2.scale
  y2_shift <- y1_pretty[1]
  y2_min <- floor(y2_shift/y2.scale)
  # adding line from Bezugspegel
  if (!is.null(ref.mID)){
    ref_mID <- his_from_case(case.list = case.list,
                             sobek.project = sobek.project,
                             mID = ref.mID, param = param,
                             verbose = FALSE)
    colnames(ref_mID) <- c('ts', 'Bezugspegel', 'case')
    id_data <- merge(id_data, ref_mID, by = c('ts', 'case'))
    y1_min <- min(y1_min, ref_mID$Bezugspegel, na.rm = TRUE)
    y1_max <- max(y1_max, ref_mID$Bezugspegel, na.rm = TRUE)
    y1_pretty <-  pretty(y1_min:y1_max, 5, 5)
    hwe <- paste(year(id_data[.N, ts]),
                 str_extract(case.list[1], 'Mittel|Selten'))
    g <- ggplot(data = id_data,
                mapping = aes(x = ts, linetype = line_type)) +
      theme_bw() +
      theme(legend.position = 'bottom')+
      scale_x_datetime()+
      ylab(y1_label) + xlab('Zeit')+
      ggtitle(paste('Ganglinien für Maßnahme: ', name, ". Hochwasser: ", hwe,
                    sep = ''
      )) +
      geom_line(aes(y = Bezugspegel,
                    color = 'Bezugspegel',
                    linetype = line_type
      ),
      size = 1)
  } else{
    g <- ggplot(data = id_data,
                mapping = aes(x = ts)) +
      theme_bw() +
      theme(legend.position = 'bottom')+
      scale_x_datetime()+
      ylab(y1_label) + xlab('Zeit')+
      ggtitle(paste('Ganglinien für Maßnahme: ', name))
  }
  # print(id_tbl)
  # if parameter is discharge, move waterlevel to secondary axis
  if (tolower(param) == 'discharge'){
    g <- g +
      # geom_line(aes(y = Vor, color = 'Vor der Maßnahme',
      #               linetype = 'Abfluss'),
      #           size = 1) +
      geom_line(aes(y = Nach,  color = 'Nach der Maßnahme',
                    linetype = line_type),
                size = 1)
    if (isTRUE(W.innen)){
      delta <- FALSE
      Q.zu <- FALSE
      y2_min <- floor(min(id_data$W_innen, na.rm = TRUE))
      if (y2_min*y2.scale != y1_min) y2_shift <- y2_shift - floor(y2_min*y2.scale)
      # if (y2_min*y2.scale > y1_min) y2_shift <- y2_shift - y2_min*y2.scale
      g <- g + geom_line(aes(y = W_innen * y2.scale + y2_shift,
                             color = 'In der Maßnahme', linetype = line_type),
                         size = 1)
      y2_name <- 'Wasserstand (m+NHN)'
    }
    # if (isTRUE(W.innen)) Q.zu <- FALSE
    if (isTRUE(Q.zu)){
      y2_name <- 'Abfluss durch Bauwerke (m³/s)'
      y2_min <- floor(min(id_data$Einlass, na.rm = TRUE))
      if (y2_min*y2.scale != y1_min) y2_shift <- y2_shift - floor(y2_min*y2.scale)
      g <- g + geom_line(aes(y = Einlass * y2.scale + y2_shift,
                             color = 'Q_Einlass', linetype = line_type),
                         size = 1)
      if (isTRUE(Q.ab)){
        g <- g + geom_line(aes(y = Auslass * y2.scale + y2_shift,
                               color = 'Q_Auslass', linetype = line_type),
                           size = 1)
      }
    }
    #----working with WL----
  } else {
    # adding W_innen directly to the graphic should not be a problem
    g <- g +
      # geom_line(aes(y = Vor, color = 'Vor Maßnahme',
      #               linetype = line_type),
      #           size = 1) +
      geom_line(aes(y = Nach,  color = 'Nach Maßnahme',
                    linetype = line_type),
                size = 1)
    if (isTRUE(W.innen)){
      y2_name <- 'Wasserstand (m+NHN)'
      y1_max <- id_data[, max(.SD, na.rm = TRUE),
                        .SDcols = c('Nach', 'Vor', 'W_innen')]
      y1_min <- id_data[, min(.SD, na.rm = TRUE),
                        .SDcols = c('Nach', 'Vor', 'W_innen')]
      y2_max <- ceiling(y1_max/y2.scale)
      y1_pretty <-  pretty(y1_min:y1_max, 5, 5)
      # delta <- FALSE
      # Q.zu <- FALSE
      g <- g + geom_line(aes(y = W_innen,
                             color = 'In der Maßnahme',
                             linetype = 'Wasserstand'),
                         size = 1)
    }
    if (isTRUE(Q.zu)){
      y2_name <- 'Abfluss durch Bauwerke (m³/s)'
      y2_min <- floor(min(id_data$Einlass, na.rm = TRUE))
      if (y2_min*y2.scale != y1_min) {
        y2_shift <- y2_shift - floor(y2_min*y2.scale)
      }
      g <- g + geom_line(aes(y = Einlass * y2.scale + y2_shift,
                             color = 'Q_Einlass', linetype = line_type),
                         size = 1)
      if (isTRUE(Q.ab)){
        g <- g + geom_line(aes(y = Auslass * y2.scale + y2_shift,
                               color = 'Q_Auslass', linetype = line_type),
                           size = 1)
        y2_min <- floor(min(id_data$Auslass, na.rm = TRUE))
        if (y2_min*y2.scale != y1_min){
          y2_shift <- y2_shift - floor(y2_min*y2.scale)
        }
      }
    }
  }
  g$labels$colour <- 'Farbe'
  g$labels$linetype <- 'Linienart'
  #----annotating max value----
  id_data[, Q_in_max := max(Einlass, na.rm = TRUE), by = case]
  id_data[, W_in_max := max(W_innen, na.rm = TRUE), by = case]
  # calculating Max Volume
  if(!is.null(polder.F)){
    if(is.null(polder.Z)){
      id_data[, H_innen_max := max(W_innen, na.rm = TRUE) -
                min(W_innen, na.rm = TRUE), by = case
              ]
      id_data[, Volume_max := round(H_innen_max * polder.F / 100, 2)]
    } else{
      id_data[, Volume_max := round(polder.Z * polder.F / 100, 2)]
    }
  } else{
    if (nrow(id_vol) >= 1){
      id_vol_data <- .get_volume_for_cases(
        name = name, case.list = ccase.list,
        case.desc = case.desc,
        sobek.project = sobek.project,
        master.tbl = id_vol,
        verbose = FALSE
      )
      id_data <- merge(id_data, id_vol_data, by = 'case', sort = FALSE)
    } else{
      id_data[, Volume_max := Inf]
    }
  }
  # finding the locations on x-axis for the annotated text
  # get length of x_axis, then get 1/5 of it
  x_axis_n <- id_data[case == cname_1, .N]
  i_pos_txt <- floor(x_axis_n*text.pos)
  x_pos_txt <- id_data[case == cname_1][i_pos_txt, ]
  # x_pos_txt <- id_data[, ts_min := shift(ts, n = floor(0.2*N),
  #                           fill = NA, type = 'lead'),
  #         by = case]
  if(!is.null(h.lines)){
    i_pos_hline <- 1 #floor(x_axis_n*0.05)
    id_hlines <- id_data[case == cname_1][i_pos_hline,
                                          c('ts', 'case', 'line_type',
                                            'Bezugspegel')]
  }
  x_pos_txt[Volume_max != Inf, label := paste(
    'Volume Max: ', Volume_max, ' Mio. m³\n',
    'Q_in Max:   ', round(Q_in_max), ' m³/s\n',
    'W_in Max:   ', round(W_in_max, 2), ' m + NHN\n',
    sep = ""
  )]
  x_pos_txt[Volume_max == Inf, label := paste(
    'Volume Max: k.A.\n',
    'Q_in Max:   ', round(Q_in_max), ' m³/s\n',
    'W_in Max:   ', round(W_in_max, 2), ' m + NHN\n',
    sep = ""
  )]
  x_pos_txt[Q_in_max == 0, label := paste(
    'Volume Max: 0 m³\n',
    'Q_in Max:   ', round(Q_in_max), ' m³/s\n',
    'W_in Max:   ', round(W_in_max, 2), ' m + NHN\n',
    sep = ""
  )]

  if (isTRUE(delta.pegel) & !is.null(ref.mID)){
    x_pos_txt[, label := paste(
      'Delta am Bezugspegel: ', scheitel_ref_mID_delta, " ", delta_unit, " \n",
      label,
      sep = ""
    )]
  }
  if (isTRUE(delta.measure)){
    x_pos_txt[, label := paste(
      'Delta an der Maßnahme: ', scheitel_measure_delta, " ", delta_unit, " \n",
      label,
      sep = ""
    )]
  }
  # y position of the text block
  y1_pos_txt <- y1_pretty[(length(y1_pretty)-1)]
  g <- g +
    # facet_wrap(.~case, scales = 'free_x')+
    geom_text(
      data    = x_pos_txt,
      mapping = aes(x = ts,
                    y = y1_pos_txt,
                    label = label),
      hjust = 0, vjust = v.just
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
    id_hlines <- melt(id_hlines, id.vars = c('ts', 'case', 'line_type',
                                             'Bezugspegel'))
    g <- g + geom_text(
      data    = id_hlines,
      # V1 is the colume name of the min (ts_hlines)
      mapping = aes(x = ts, y = value,
                    label = sub("^V_", "", variable)
      ),
      hjust   = 1,
      vjust = 0
    )
  }
  if ((param == 'discharge' & isTRUE(W.innen))|Q.zu){
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
  return(g)
}
