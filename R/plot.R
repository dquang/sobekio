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


#' Plot peak difference at one place
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
#' @export
plot_scheitel_delta <- function(
  pos = NULL,
  indt = NULL,
  arg = NULL,
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

  if (!is.null(arg)){
    data_tb <- do.call(his_from_case, args = arg)
  } else{
    data_tb <- data.table(indt)
  }
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

#' Plot hydrographs at different locations
#' @param case.list List of cases
#' @param case.desc Case naming according to NHWSP Standard
#' @param sobek.project Path to sobek project
#' @param param Waterlevel/Discharge
#' @param compare.by Grouping the lines by linetype. Default 'zustand'
#' @param facet.by Facetted by, default 'hwe'
#' @param id.type Default mID
#' @param id.list List of the ids
#' @param p.title Title of the plot
#' @param x.lab x-axis title
#' @param y.lab y-axis title
#' @param date.breaks x-axis breaks
#' @param date.labels Formatting of the date on x-axis
#' @param text.x.angle Angle of text on x-axis
#' @param text.size Size of all text
#' @return A ggplot2 graphic
#' @export
plot_multi_lines <- function(
  case.list = NULL,
  case.desc = case.list,
  sobek.project = NULL,
  param = 'discharge',
  compare.by = 'zustand',
  facet.by = 'hwe',
  id.type = 'mID',
  id.list =  c('p_worms',
               "p_main_muendung",
               "P_Mainz",
               "p_nahe_muendung",
               'p_kaub',
               'p_lahn_muendung',
               "p_mosel_muendung"
  ),
  p.title = 'Ganglinien an der Orten',
  x.lab = 'Zeit',
  y.lab = ifelse(param == 'discharge', 'Abfluss m³/s', 'Wasserstand (m+NHN)'),
  date.breaks = '3 days',
  date.labels = "%d.%m.%Y",
  text.x.angle = 90L,
  text.size = 12L
){
  stopifnot(!c(is.null(case.list), is.null(sobek.project), is.null(id.list)))
  his_args <- list(
    case.list = case.list,
    sobek.project = sobek.project,
    param = param,
    mID = id.list,
    verbose = FALSE
  )
  if (id.type == 'latID') id.type <- 'lID'
  names(his_args)[4] <- id.type
  case_type <- parse_case(case.desc = case.desc, orig.name = case.list)
  qt <- do.call(his_from_case, args = his_args)
  qt <- melt(qt, id.vars = c('ts', 'case'))
  qt[, variable := str_replace_all(variable, "_", " ")]
  qt[, variable := str_to_title(variable)]
  qt[, variable := str_replace_all(variable, " ", "_")]
  qt <- merge(qt, case_type, by = 'case', sort = FALSE)
  g <- ggplot(qt,
              aes(x = ts, y = value,
                  color = variable,
                  # quasiquotation, that's a great option from tidyverse
                  linetype = !!ensym(compare.by)
              )
  ) +
    scale_x_datetime(
      date_breaks = date.breaks,
      date_labels = date.labels
    ) +
    geom_line(size = 1) +
    theme_bw()+
    theme(
      legend.position = 'bottom',
      text = element_text(
        size = text.size
      ),
      axis.text.x = element_text(
        angle = text.x.angle
      )
    ) +
    ggtitle(p.title)+
    xlab(x.lab) + ylab(y.lab)
  if (!is.null(facet.by)){
    g <- g + facet_grid(. ~ get(facet.by), scales = 'free_x')
  }
  g$labels$colour <- 'Farbe'
  g$labels$linetype <- 'Linienart'
  g
}
