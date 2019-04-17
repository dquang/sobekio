#' Compare (with/without) hydrographs for a measure
#' @param name Name of the measure (with/without the measure)
#' @param case.name Name of 2 cases
#' @param case.desc = Short version of case.name, it will be used for legend
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
#' @param text.pos Positioning of the text block as relative value to the length of x-axis
#' @param v.just Justification of the text block
#' @param zoom Should be whole graphic zoom to n days around the peak?
#' @param master.tbl Table of ID Coding of the sobek network
#' @return A ggplot2 graphic
#' @export
plot_measure_compare <- function(
  name = NULL,
  case.name = NULL,
  case.desc = c('Planzustand', 'Bezugszustand'),
  param = 'waterlevel',
  y2.scale = 25,
  sobek.project = NULL,
  ref.mID = NULL,
  Q.zu = FALSE,
  Q.ab = FALSE,
  W.innen = FALSE,
  delta.pegel = FALSE,
  delta.measure = FALSE,
  V.max = TRUE,
  polder.F = NULL,
  polder.Z = NULL,
  h.lines = NULL,
  text.pos = 0.01,
  v.just = 1,
  zoom = NULL,
  master.tbl = NULL){
  # setting for display different lines on different graphics
  # if (isTRUE(Q.zu)) delta <- FALSE
  stopifnot(length(unlist(case.name)) == 2)
  # checking input
  param = tolower(param)
  stopifnot(param %in% c('discharge', 'waterlevel'))
  stopifnot(!c(is.null(name), is.null(case.name), is.null(master.tbl),
               is.null(sobek.project)
  ))
  # get the naming of the cases, for legend
  cname_1 <- case.name[[1]]
  cname_2 <- case.name[[2]]
  # get ID table of the "Maßnahme"
  id_tbl <- master.tbl[grepl(name, besonderheit, fixed = TRUE)]
  stopifnot(nrow(id_tbl) > 1)
  id_mitte <- id_tbl[grepl('.+_Innen', besonderheit)][1]
  id_st <- id_tbl[grepl('.+_Ab|.*_Zu', besonderheit) & ID_TYPE == 'sID']
  id_vol <- id_tbl[grepl('.+_Vol', besonderheit) & ID_TYPE == 'wID']
  if (param == 'discharge'){
    # this take only the first row, if there is none, it should get an NA
    id_in <- id_tbl[grepl('_Einlass', besonderheit) &
                      grepl('qID|mID', ID_TYPE)
                    ][1]
    id_out <- id_tbl[grepl('_Auslass', besonderheit)&
                       grepl('qID|mID', ID_TYPE)
                     ][1]
  } else{
    id_in <- id_tbl[grepl('_Einlass', besonderheit) &
                      grepl('wID|mID', ID_TYPE)
                    ][1]
    id_out <- id_tbl[grepl('_Auslass', besonderheit)&
                       grepl('wID|mID', ID_TYPE)
                     ][1]
  }
  id_in_args <- list(case.list = case.name, sobek.project = sobek.project,
                     id_type = id_in$ID, param = param, verbose = FALSE)
  id_out_args <- list(case.list = case.name, sobek.project = sobek.project,
                      id_type = id_out$ID, param = param, verbose = FALSE)
  # change parameter name to correct ID_TYPE
  names(id_in_args)[3] <- id_in$ID_TYPE
  names(id_out_args)[3] <- id_out$ID_TYPE
  # get data for ID in and out
  ft_id_in <- do.call(his_from_case, id_in_args)
  colnames(ft_id_in) <- c('ts', 'Vor', 'case')
  ft_id_out <- do.call(his_from_case, id_out_args)
  colnames(ft_id_out) <- c('ts', 'Nach', 'case')
  # combine data in, and out
  ft_in_out <- merge(ft_id_in, ft_id_out, by = c('ts', 'case'))
  # get qt through the Structures
  st_ab_zu <- his_from_case(case.list = case.name,
                            sobek.project = sobek.project,
                            sID = id_st$ID, param = 'discharge',
                            verbose = FALSE
  )
  st_ab_zu_cname <- c('ts', id_st$besonderheit, 'case')
  st_ab_zu_cname <- sub('.*_Zu', 'Einlass', id_st$besonderheit)
  st_ab_zu_cname <- sub('.*_Ab', 'Auslass', st_ab_zu_cname)
  colnames(st_ab_zu) <- c('ts', st_ab_zu_cname, 'case')
  # get Waterlevel
  id_mitte_args <- list(case.list = case.name, sobek.project = sobek.project,
                        id_mitte_type = id_mitte$ID, param = "waterlevel",
                        verbose = FALSE)
  names(id_mitte_args)[3] <- id_mitte$ID_TYPE
  wt_id_mitte <- do.call(his_from_case, id_mitte_args)
  colnames(wt_id_mitte) <- c('ts', 'W_innen', 'case')
  # merging data
  id_data <- merge(ft_in_out, st_ab_zu, by = c('ts', 'case'))
  id_data <- merge(id_data, wt_id_mitte, by = c('ts', 'case'))
  # adding case description column, using for linetype later on
  for (i in seq_along(case.name)) {
    id_data[case == case.name[i], line_type := case.desc[i]]
  }
  # finding scheitel delta at the measure
  if (isTRUE(delta.measure)){
    # calculate diff between two max value (different moment)
    scheitel_max_c1 <- id_data[case == case.name[[1]], max(Nach)]
    scheitel_max_c2 <- id_data[case == case.name[[2]], max(Nach)]
    scheitel_measure_delta <- scheitel_max_c1 - scheitel_max_c2
    scheitel_measure_delta <- round(scheitel_measure_delta, 2)
    # rounding value for discharge
    if (scheitel_measure_delta > 2) {
      scheitel_measure_delta <- round(scheitel_measure_delta)
    }
  }
  y1_label <- ifelse(param == 'discharge', 'Abfluss m³/s', 'Wasserstand (m+NHN)')
  delta_unit <- ifelse(param == 'discharge', 'm³/s', 'm')
  # line_type <- ifelse(param == 'discharge', 'Abfluss', 'Wasserstand')
  y1_max <- id_data[, max(.SD, na.rm = TRUE), .SDcols = c('Nach', 'Vor')]
  y1_min <- id_data[, min(.SD, na.rm = TRUE), .SDcols = c('Nach', 'Vor')]
  y2_min <- id_data[, min(.SD, na.rm = TRUE),
                    .SDcols = -c('Nach', 'Vor', 'ts', 'case', 'line_type')]
  y1_pretty <-  pretty(y1_min:y1_max, 5, 5)
  y2_max <- y1_max/y2.scale
  y2_shift <- y1_pretty[1]
  y2_min <- floor(y2_shift/y2.scale)
  # adding line from Bezugspegel
  if (!is.null(ref.mID)){
    ref_mID <- his_from_case(case.list = case.name,
                             sobek.project = sobek.project,
                             mID = ref.mID, param = param,
                             verbose = FALSE)
    colnames(ref_mID) <- c('ts', 'Bezugspegel', 'case')
    id_data <- merge(id_data, ref_mID, by = c('ts', 'case'))
    # get peak difference at the ref_measurement
    for (i in seq_along(case.name)){
      ref_mID[case == case.name[i],
              line_type := case.desc[i]]
    }
    scheitel_max_c1 <- ref_mID[case == case.name[[1]], max(Bezugspegel)]
    scheitel_max_c2 <- ref_mID[case == case.name[[2]], max(Bezugspegel)]
    scheitel_ref_mID_delta <- scheitel_max_c1 - scheitel_max_c2
    scheitel_ref_mID_delta <- round(scheitel_ref_mID_delta, 2)
    # rounding value for discharge
    if (scheitel_ref_mID_delta > 2) {
      scheitel_ref_mID_delta <- round(scheitel_ref_mID_delta)
    }
    y1_min <- min(y1_min, ref_mID$Bezugspegel, na.rm = TRUE)
    y1_max <- max(y1_max, ref_mID$Bezugspegel, na.rm = TRUE)
    y1_pretty <-  pretty(y1_min:y1_max, 5, 5)
    hwe <- paste(year(id_data[.N, ts]),
                 str_extract(case.name[1], 'Mittel|Selten'))
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
    if (nrow(id_vol) >= 1 & id_data$Q_in_max[1] > 0){
      p_volume <- his_from_case(case.list = case.name,
                                sobek.project = sobek.project,
                                param = 'Volume',
                                wID = id_vol$ID, verbose = FALSE)
      p_volume <- p_volume[, rowSums(.SD, na.rm = TRUE), by = case,
                           .SDcols = -c('ts')]
      p_volume <- p_volume[, round(max(V1)/10^6, 2), by = case]
      colnames(p_volume) <- c('case', 'Volume_max')
      id_data <- merge(id_data, p_volume,
                       by = c('case'))
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
