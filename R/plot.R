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

#' Quick plot for long profile or pos (column name)
#' @import ggplot2
#' @export
#' @param pos "longprofile" or list of list of column to plot
#' @param indt Input data.table or data.frame
#' @param x.lab x label
#' @param y.lab y label
#' @param title title
#' @param x.angle rotate angle of x.label
#' @param x.hjust hjust of x.label
#' @param size size of line/point
#' @param faced default FALSE. If TRUE, faced by case
#' @param sort to short longprofile by max value, not yet implemented
#' @return a ggplot2 graphic object
plot_wirkung <- function(pos = "longprofile",
                         indt = NULL,
                         x.lab = 'Lage',
                         y.lab = 'Wasserstand (m+NHN)',
                         title = '',
                         x.angle = 90L,
                         x.hjust = 1L,
                         size = 1L,
                         h.line = NULL,
                         faced = NULL,
                         sort = TRUE){

  data_tb <- data.table(indt)
  lage_all <- colnames(data_tb)
  lage_all <- lage_all[-grep("ts|case", lage_all)]
  if (pos[[1]] == "longprofile"){
    data_tb <- data_tb[, lapply(.SD, max, na.rm = TRUE), by = case]
    data_tb <- melt(data_tb,
                    id.vars = c('case'),
                    variable.name = 'lage',
                    measure.vars = lage_all
    )
    g <- ggplot(data = data_tb,
                mapping = aes(x = lage, y = value, color = case)
    ) + theme_bw() +
      theme(legend.position = 'bottom',
            axis.text.x = element_text(angle = x.angle, hjust = x.hjust)
      ) +
      xlab(x.lab) +
      ylab(y.lab) +
      ggtitle(title) +
      geom_point(size = size)
  } else{
    for (p in pos){
      if ( !p %in% lage_all) stop('column: ', p, ' not found in data table')
    }
    data_tb <- data_tb[, c('ts', pos, 'case'), with = FALSE]
    # print(data_tb)
    data_tb <- melt(data_tb,
                    id.vars = c('ts', 'case'),
                    variable.name = 'Lage'
    )
    # print(data_tb)
    g <- ggplot(data = data_tb,
                mapping = aes(x = ts,
                              # y = get_column_by_name(p, data_tb),
                              y = value,
                              color = case,
                              linetype = Lage)
    ) +
      scale_x_datetime() + theme_bw() +
      theme(legend.position = 'bottom',
            axis.text.x = element_text(angle = x.angle, hjust = x.hjust)
      ) +
      xlab(x.lab) +
      ylab(y.lab) +
      ggtitle(title) +
      geom_line(size = size)

    if(!is.null(h.line)){
      t_min <- min(data_tb$ts)
      y_min <- round(min(data_tb$value))
      y_max <- round(max(data_tb$value))
      y_break <- sort(c(pretty(y_min:y_max, 5),
                        unlist(h.line, use.names = F)))
      for (i in seq_along(h.line)){
        g <- g + geom_hline(yintercept = h.line[[i]], linetype="dashed")+
          annotate("text",
                   x = t_min,
                   y = h.line[[i]],
                   label = names(h.line)[[i]]
          )
      }
      g <- g + scale_y_continuous(breaks = y_break, limits = c(y_min, NA))
    }
  }

  if (!is.null(faced)){
    g <- g + facet_wrap(.~case, scales = faced)
  }
  return(g)
}


#' Plot Q/W hydrograph for the
#' @import grid
#' @export
#' @param pos Names of columns to draw
#' @param indt Input data
#' @param peak.duration Halftime of the flooding period, default 5 days
#' @param x.lab Label for x axis, default 'Lage'
#' @param y.lab Label for y axis, default 'Discharge (m³/s)'
#' @param title Graphic title
#' @param x.angle Rotation angle of x label
#' @param x.hjust hjust of x label
#' @param size size of the line
#' @param y.min ymin
#' @param delta Annotate Delta value, if only for one case and two columns
#' @param h.line Numeric vector for adding HLINE
#' @param y.interval Tick interval for y axis
#' @param faced faced by case, default FALSE
#' @return a ggplot2 graphic
plot_polder <- function(pos = NULL,
                        indt = NULL,
                        peak.duration = 5L,
                        x.lab = 'Lage',
                        y.lab = 'Abfluss (m³/s)',
                        title = '',
                        x.angle = 90L,
                        x.hjust = 1L,
                        size = 1L,
                        y.min = NULL,
                        delta = FALSE,
                        h.line = NULL,
                        y.interval = 500L,
                        faced = NULL){
  data_tb <- data.table(indt)
  lage_all <- colnames(data_tb)
  lage_all <- lage_all[-grep("ts|case", lage_all)]
  data_tb <- data_tb[, c('ts', pos, 'case'), with = FALSE]
  q_max <- melt(data_tb[, lapply(.SD, max, na.rm = TRUE), .SDcols = pos],
                measure.vars = pos, variable.factor = F)
  col_has_max <- q_max[value == max(value), variable]
  time_at_peak <- data_tb[get(col_has_max)==max(get(col_has_max)), ts][1]
  t_min <- time_at_peak - peak.duration*24*3600
  t_max <- time_at_peak + peak.duration*24*3600
  q_min <- as.integer(data_tb[ts == t_min,
                              get(col_has_max)])
  y_min <- ifelse(is.null(y.min), q_min, y.min)
  # print(data_tb)
  data_tb <- melt(data_tb,
                  id.vars = c('ts', 'case'),
                  variable.name = 'Lage'
  )
  # if (max(qmax$value)>300 & y.lab == 'Wasserstand (m+NHN)'){
  #   warning('check y label again')
  # }
  # print(data_tb)
  g <- ggplot(data = data_tb,
              mapping = aes(x = ts,
                            # y = get_column_by_name(p, data_tb),
                            y = value,
                            color = Lage,
                            linetype = case
              )) +
    scale_x_datetime(date_breaks = '1 days',
                     limits = c(t_min, t_max)
                     # minor_breaks = "6 hours"
    ) + theme_bw() +
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = x.angle, hjust = x.hjust)
    ) +
    xlab(x.lab) +
    ylab(y.lab) +
    ggtitle(title) +
    geom_line(size = size) + ylim(y_min, NA)
  if (delta & length(pos) == 2 & length(unique(indt$case)) == 1){
    my_grob = grobTree(textGrob(paste("Delta:",
                                      round(max(q_max$value)
                                            - min(q_max$value))
                                      ),
                                x = 0.6,  y = 0.95, hjust = 0,
                                gp = gpar(col = "blue",
                                          fontsize = 15, fontface="bold")))
    g <- g+
      annotation_custom(my_grob)
    # annotate("text",
    #          x = time_at_peak,
    #          y = max(q_max$value) + 0.02*(max(q_max$value) - y_min),
    #          label = paste("Delta:",
    #                        round(max(q_max$value) - min(q_max$value), 2)
    #          )
    #          )

  }
  if(!is.null(h.line)){
    y_max <- round(max(q_max$value))
    y_break <- sort(c(pretty(y_min:y_max, 5),
                      unlist(h.line, use.names = F)))
    for (i in seq_along(h.line)){
      g <- g + geom_hline(yintercept = h.line[[i]], linetype="dashed")+
        annotate("text",
                 x = t_min,
                 y = h.line[[i]] + round(0.1*(y_max - y_min)),
                 label = names(h.line)[i]
                 )
    }
    g <- g + scale_y_continuous(breaks = y_break, limits = c(y_min, NA))
  }
  if (!is.null(faced)){
    g <- g + facet_wrap(.~case, scales = faced)
  }
  return(g)
}


#' Plot peak difference at one place
#' @import grid
#' @export
#' @param pos Names of columns to draw
#' @param indt Input data
#' @param x.lab Label for x axis, default 'Lage'
#' @param y.lab Label for y axis, default 'Discharge (m³/s)'
#' @param title Graphic title
#' @param x.angle Rotation angle of x label
#' @param x.hjust hjust of x label
#' @param size size of the line
#' @param y.min ymin
#' @param delta Annotate Delta value, if only for one case and two columns
#' @param addon.text Another text to be put under the Delta value (optional)
#' @param h.line Numeric vector for adding HLINE
#' @param y.interval Tick interval for y axis
#' @param peak.duration Halftime of the flooding period, default 5 days
#' @param faced faced by case, default FALSE
#' @return a ggplot2 graphic
plot_scheitel_delta <- function(
  pos = '',
  indt = NULL,
  x.lab = 'Zeit',
  y.lab = 'Abfluss (m³/s)',
  title = paste('Ganglinie an der Lage:', pos) ,
  x.angle = 90L,
  x.hjust = 1L,
  size = 1L,
  y.min = NULL,
  delta = TRUE,
  addon.text = NULL,
  h.line = NULL,
  y.interval = 500L,
  peak.duration = 5L,
  faced = NULL){
  data_tb <- data.table(indt)
  lage_all <- colnames(data_tb)
  lage_all <- lage_all[-grep("ts|case", lage_all)]
  data_tb <- data_tb[, c('ts', pos, 'case'), with = FALSE]
  q_max <- data_tb[, lapply(.SD, max, na.rm = TRUE), .SDcols = pos, by = case]
  time_at_peak <- data_tb[get(pos)==max(get(pos)), ts][1]
  t_min <- time_at_peak - peak.duration*24*3600
  t_max <- time_at_peak + peak.duration*24*3600
  q_min <- as.integer(data_tb[ts == t_min,
                              get(pos)])
  y_min <- ifelse(is.null(y.min), q_min, y.min)

  data_tb <- melt(data_tb,
                  id.vars = c('ts', 'case'),
                  variable.name = 'Lage'
  )
  g <- ggplot(data = data_tb,
              mapping = aes(x = ts,
                            # y = get_column_by_name(p, data_tb),
                            y = value,
                            color = case,
                            linetype = case
              )) +
    scale_x_datetime(date_breaks = '1 days',
                     limits = c(t_min, t_max)
                     # minor_breaks = "6 hours"
    ) + theme_bw() +
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = x.angle, hjust = x.hjust)
    ) +
    xlab(x.lab) +
    ylab(y.lab) +
    ggtitle(title) +
    geom_line(size = size) + ylim(y_min, NA)
  if (delta & length(pos) == 1){
    delta_value <- max(q_max[,get(pos)]) - min(q_max[,get(pos)])
    delta_value  <- ifelse(delta_value > 1, round(delta_value, 0),
                           round(delta_value, 2))
    my_grob = grobTree(
      textGrob(paste("Delta:", delta_value),
               x = 0.6,  y = 0.95, hjust = 0,
               gp = gpar(col = "blue",
                         fontsize = 12, fontface="bold")
               )
    )
    g <- g +
      annotation_custom(my_grob)
    # insert Max. Zulauf if given
    if (!is.null(addon.text)){
      addon_text = grobTree(
        textGrob(addon.text,
                 x = 0.6,  y = 0.90, hjust = 0,
                 gp = gpar(col = "blue",
                           fontsize = 12, fontface="bold")
        )
      )
      g <- g + annotation_custom(addon_text)
    }
  }
  if(!is.null(h.line)){
    y_max <- max(q_max[, get(pos)])
    y_break <- sort(c(pretty(y_min:y_max, 5),
                      unlist(h.line, use.names = F)))
    for (i in seq_along(h.line)){
      g <- g + geom_hline(yintercept = h.line[[i]], linetype="dashed")+
        annotate('text', x = t_min,
                 y = h.line[[i]]+ round(0.04*(y_max - y_min)),
                 label = names(h.line)[i])
    }
    g <- g + scale_y_continuous(breaks = y_break, limits = c(y_min, NA))
  }
  if (!is.null(faced)){
    g <- g + facet_wrap(.~case, scales = faced)
  }
  return(g)
}


#' Plot hydrographs at a measure
#' @param name Name of the measure
#' @param case.name Name of cases
#' @param param 'Waterlevel' or 'Discharge' Hydrograph
#' @param y2.scale Scaling between main and secondary y-axes
#' @param sobek.project Path to sobek project
#' @param ref.mID ID of Bezugspegel
#' @param Q.zu Logical. Should discharge through the inlet be plotted?
#' @param Q.ab Logical. Should discharge through the outlet be plotted?
#' @param W.innen Logical. Should Wt line inside the measure be plotted?
#' @param delta Logical. Should diffrence between Before and After values be plotted?
#' @param polder.F Area of the measure, for calculating Volume
#' @param polder.Z Bottom level of the measure for calculating Volume. Give 'auto' for getting the minimum waterlevel in canal value. In this case, make sure the canal is completely dry at T0
#' @param master.tbl Table of ID Coding of the sobek network
#' @return A ggplot2 graphic
#' @export
plot_measure <- function(
  name = NULL,
  case.name = NULL,
  param = 'waterlevel',
  y2.scale = 50,
  sobek.project = NULL,
  ref.mID = NULL,
  Q.zu = TRUE,
  Q.ab = TRUE,
  W.innen = FALSE,
  delta = FALSE,
  V.max = TRUE,
  polder.F = NULL,
  polder.Z = NULL,
  master.tbl = NULL){
  # setting for display different lines on different graphics
  if (isTRUE(Q.zu)) delta <- FALSE
  # checking input
  param = tolower(param)
  stopifnot(param %in% c('discharge', 'waterlevel'))
  stopifnot(!c(is.null(name), is.null(case.name), is.null(master.tbl),
                         is.null(sobek.project)
                         ))
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
  id_data[, Delta := Nach - Vor]
  y1_label <- ifelse(param == 'discharge', 'Abfluss m³/s', 'Wasserstand (m+NHN)')
  line_type <- ifelse(param == 'discharge', 'Abfluss', 'Wasserstand')
  y1_max <- id_data[, max(.SD, na.rm = TRUE), .SDcols = c('Nach', 'Vor')]
  y1_min <- id_data[, min(.SD, na.rm = TRUE), .SDcols = c('Nach', 'Vor')]
  y2_min <- id_data[, min(.SD, na.rm = TRUE),
                    .SDcols = -c('Nach', 'Vor', 'ts', 'case')]
  y1_pretty <-  pretty(y1_min:y1_max, 5, 5)
  y2_max <- y1_max/y2.scale
  y2_shift <- y1_pretty[1]
  y2_min <- y2_shift/y2.scale
  # adding line from Bezugspegel
  if (!is.null(ref.mID)){
    ref_mID <- his_from_case(case.list = case.name,
                             sobek.project = sobek.project,
                             mID = ref.mID, param = param,
                             verbose = FALSE)
    colnames(ref_mID) <- c('ts', 'Bezugspegel', 'case')
    id_data <- merge(id_data, ref_mID, by = c('ts', 'case'))
    y1_min <- min(y1_min, ref_mID$Bezugspegel, na.rm = TRUE)
    y1_max <- max(y1_max, ref_mID$Bezugspegel, na.rm = TRUE)
    y1_pretty <-  pretty(y1_min:y1_max, 5, 5)
    g <- ggplot(data = id_data,
                mapping = aes(x = ts)) +
      theme_bw() +
      theme(legend.position = 'bottom')+
      scale_x_datetime()+
      ylab(y1_label) + xlab('Zeit')+
      ggtitle(paste('Ganglinien für Maßnahme: ', name)) +
      geom_line(aes(y = Bezugspegel,
                    color = 'Bezugspegel',
                    linetype = eval(line_type)
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
  print(id_tbl)
  # if parameter is discharge, move waterlevel to secondary axis
  if (tolower(param) == 'discharge'){
    g <- g +
      geom_line(aes(y = Vor, color = 'Vor der Maßnahme',
                    linetype = 'Abfluss'),
                size = 1) +
      geom_line(aes(y = Nach,  color = 'Nach der Maßnahme',
                    linetype = 'Abfluss'),
                size = 1)
    if (isTRUE(W.innen)){
      delta <- FALSE
      Q.zu <- FALSE
      y2_min <- min(id_data$W_innen, na.rm = TRUE)
      if (y2_min*y2.scale != y1_min) y2_shift <- y2_shift - y2_min*y2.scale
      # if (y2_min*y2.scale > y1_min) y2_shift <- y2_shift - y2_min*y2.scale
      g <- g + geom_line(aes(y = W_innen * y2.scale + y2_shift,
                             color = 'In der Maßnahme', linetype = 'Wasserstand'),
                         size = 1)
      y2_name <- 'Wasserstand (m+NHN)'
    }
    # if (isTRUE(W.innen)) Q.zu <- FALSE
    if (isTRUE(Q.zu)){
      y2_name <- 'Abfluss durch Bauwerke (m³/s)'
      y2_min <- min(id_data$Einlass, na.rm = TRUE)
      if (y2_min*y2.scale != y1_min) y2_shift <- y2_shift - y2_min*y2.scale
      g <- g + geom_line(aes(y = Einlass * y2.scale + y2_shift,
                             color = 'Q_Einlass', linetype = 'Abfluss'),
                         size = 1)
      if (isTRUE(Q.ab)){
        g <- g + geom_line(aes(y = Auslass * y2.scale + y2_shift,
                               color = 'Q_Auslass', linetype = 'Abfluss'),
                           size = 1)
      }
    }
    if (isTRUE(delta)){
      y2_name <- 'Differenze'
      y2_min <- min(id_data$Delta, na.rm = TRUE)
      if (y2_min*y2.scale != y1_min) y2_shift <- y2_shift - y2_min*y2.scale
      g <- g + geom_line(aes(y = Delta * y2.scale + y2_shift,
                             color = 'Delta', linetype = 'Abfluss'),
                         size = 1)
    }
    #----working with WL----
  } else {
    # adding W_innen directly to the graphic should not be a problem
    g <- g +
      geom_line(aes(y = Vor, color = 'Vor Maßnahme',
                    linetype = 'Wasserstand'),
                size = 1) +
      geom_line(aes(y = Nach,  color = 'Nach Maßnahme',
                    linetype = 'Wasserstand'),
                size = 1)
    if (isTRUE(W.innen)){
      y2_name <- 'Wasserstand (m+NHN)'
      y1_max <- id_data[, max(.SD, na.rm = TRUE),
                        .SDcols = c('Nach', 'Vor', 'W_innen')]
      y1_min <- id_data[, min(.SD, na.rm = TRUE),
                        .SDcols = c('Nach', 'Vor', 'W_innen')]
      y2_max <- y1_max/y2.scale
      y1_pretty <-  pretty(y1_min:y1_max, 5, 5)
      delta <- FALSE
      Q.zu <- FALSE
      g <- g + geom_line(aes(y = W_innen,
                             color = 'In der Maßnahme',
                             linetype = 'Wasserstand'),
                         size = 1)
    }
    if (isTRUE(Q.zu)){
      y2_name <- 'Abfluss durch Bauwerke (m³/s)'
      y2_min <- min(id_data$Einlass, na.rm = TRUE)
      if (y2_min*y2.scale != y1_min) y2_shift <- y2_shift - y2_min*y2.scale
      g <- g + geom_line(aes(y = Einlass * y2.scale + y2_shift,
                             color = 'Q_Einlass', linetype = 'Abfluss'),
                         size = 1)
      if (isTRUE(Q.ab)){
        g <- g + geom_line(aes(y = Auslass * y2.scale + y2_shift,
                               color = 'Q_Auslass', linetype = 'Abfluss'),
                           size = 1)
      }
    }
    if (isTRUE(delta)){
      y2_name <- 'Wasserstand Different'
      y2_min <- min(id_data$Delta, na.rm = TRUE)
      if (y2_min*y2.scale != y1_min) y2_shift <- y2_shift - y2_min*y2.scale
      g <- g + geom_line(aes(y = Delta * y2.scale + y2_shift,
                             color = 'Delta', linetype = 'Abfluss'),
                         size = 1)
    }
  }
  g$labels$colour <- 'Farbe'
  g$labels$linetype <- 'Linienart'
  #----annotating max value----
  id_data[, Q_in_max := max(Einlass, na.rm = TRUE), by = case]
  id_data[, W_in_max := max(W_innen, na.rm = TRUE), by = case]
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
  id_data[, N := as.integer(.N*0.2), by = case]
  id_data[, ts_min := shift(ts, n = N, fill = NA, type = 'lead'), by = case]
  id_data_nrow <- id_data[, min(ts_min, na.rm = TRUE), by = case]
  colnames(id_data_nrow) <- c('case', 'ts')
  id_max <- merge(id_data_nrow, id_data, by = c('ts', 'case'))
  id_max[Volume_max != Inf, label := paste(
    'Volume Max: ', Volume_max, ' Mio. m³\n',
    'W_in Max:   ', round(W_in_max, 2), 'm + NHN\n',
    'Q_in Max:   ', round(Q_in_max), ' m³/s\n', sep = ""
  )]
  id_max[Volume_max == Inf, label := paste(
    'Volume Max: k.A.\n',
    'W_in Max:   ', round(W_in_max, 2), 'm + NHN\n',
    'Q_in Max:   ', round(Q_in_max), ' m³/s\n', sep = ""
  )]
  g <- g +
    facet_wrap(.~case, scales = 'free_x')+
    geom_text(
      data    = id_max,
      mapping = aes(x = ts, y = -Inf, label = label),
      hjust   = 0,
      vjust   = 0
    )
  if (param == 'discharge'|Q.zu|delta){
    # y2_pretty_min <- y2_min - y2_shift/y2.scale
    # y2_pretty_max <- y2_max - y2_shift/y2.scale
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
