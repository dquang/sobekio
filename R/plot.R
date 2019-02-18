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
#' @param indt Input data.table or data.frame
#' @param pos "longprofile" or list of list of column to plot
#' @param x.lab x label
#' @param y.lab y label
#' @param title title
#' @param x.angle rotate angle of x.label
#' @param x.hjust hjust of x.label
#' @param size size of line/point
#' @param faced default FALSE. If TRUE, faced by case
#' @param sort to short longprofile by max value, not yet implemented
#' @return a ggplot2 graphic object
plot_wirkung <- function(indt = NULL,
                         pos = "longprofile",
                         x.lab = 'Lage',
                         y.lab = 'Wasserstand (m+NHN)',
                         title = '',
                         x.angle = 90L,
                         x.hjust = 1L,
                         size = 1L,
                         faced = FALSE,
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
  }
  if (faced) g <- g + facet_wrap(.~case)
  return(g)
}


#' Plot Q/W hydrograph for the
#' @import grid
#' @export
#' @param indt Input data
#' @param pos Names of columns to draw
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
plot_polder <- function(indt = NULL,
                        pos = NULL,
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
                        faced = FALSE){
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
                 y = h.line[[i]] + round(0.1(y_max - ymin)),
                 label = names(h.line)[i]
                 )
    }
    g <- g + scale_y_continuous(breaks = y_break, limits = c(y_min, NA))
  }
  if(faced) g <- g + facet_wrap(.~case, scales = "free")
  return(g)
}


#' Plot peak difference at one place
#' @import grid
#' @export
#' @param indt Input data
#' @param pos Names of columns to draw
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
plot_scheitel_delta <- function(
  indt = NULL,
  pos = '',
  peak.duration = 5L,
  x.lab = 'Zeit',
  y.lab = 'Abfluss (m³/s)',
  title = paste('Ganglinie an der Lage:', pos) ,
  x.angle = 90L,
  x.hjust = 1L,
  size = 1L,
  y.min = NULL,
  delta = TRUE,
  max.zulauf = "",
  h.line = NULL,
  y.interval = 500L,
  faced = FALSE){
  data_tb <- data.table(indt)
  lage_all <- colnames(data_tb)
  lage_all <- lage_all[-grep("ts|case", lage_all)]
  data_tb <- data_tb[, c('ts', pos, 'case'), with = FALSE]
  q_max <- data_tb[, lapply(.SD, max, na.rm = TRUE), .SDcols = pos, by = case]
  time_at_peak <- data_tb[get(pos)==max(get(pos)), ts]
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
    my_grob = grobTree(
      textGrob(paste("Delta:",
                     round(max(q_max[,get(pos)]) -
                             min(q_max[,get(pos)])),
                                      "m³/s"),
    x = 0.6,  y = 0.95, hjust = 0,
    gp = gpar(col = "blue",
              fontsize = 12, fontface="bold")
    )
    )
    max_zulauf = grobTree(
      textGrob(paste("Max. Zulauf:",
                     max.zulauf,
                     "m³/s"),
               x = 0.6,  y = 0.90, hjust = 0,
               gp = gpar(col = "blue",
                         fontsize = 12, fontface="bold")
      )
    )

    g <- g +
      annotation_custom(my_grob) + annotation_custom(max_zulauf)
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
  if(faced) g <- g + facet_wrap(.~case, scales = "free")
  return(g)
}
