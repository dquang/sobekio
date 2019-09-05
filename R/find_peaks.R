#' Find the center of (mass) volume and peaks
#' @param indt input data.table
#' @param peak.percent peak area as percent, ex. 0.05 means that the 5% on the top is consider as peak area.
#' @param peak.min minimum value given as the starting value of area of peak.
#' @param column Name of the column in the indt to work with
#' @param nups minimum number of increasing steps before a peak is reached
#' @param ndowns minimum number of decreasing steps after the peak
#' @return a list of (plot = graphic, peaks = matrix of peaks, and m_center = row index for the center)
#' @export
find_peaks <- function(
  indt = NULL,
  peak.percent = 0.05,
  peak.min = NULL,
  column = NULL,
  nups = 3,
  ndowns = nups
){
  # stopifnot(column %in% colnames(indt)
  indt <- indt[, .SD, .SDcols = c('ts', column)]
  colnames(indt) <- c('ts', 'value')
  indt[, orig_row := .I]
  if (is.null(peak.min)) {
    peak_min <- max(indt$value, na.rm = TRUE) * (1 - peak.percent)
  } else{
    peak_min <- peak.min
  }
  indt_peak <- indt[value >= peak_min]
  # find peaks using findpeaks from pracma package
  f_peaks <- pracma::findpeaks(indt_peak$value, nups = nups, ndowns = ndowns)
  # find the center of mass
  m_center <- sum(cumsum(indt_peak$value) <= 0.5 * sum(indt_peak$value))
  g <- ggplot(indt, aes(x = orig_row, y = value)) + geom_line() +
    geom_vline(xintercept = indt_peak[m_center, orig_row], color = 'blue',
               linetype = 2) +
    theme_bw() +
    geom_segment(aes(
      x = indt_peak[1, orig_row],
      y = peak_min,
      xend = indt_peak[.N, orig_row],
      yend = peak_min
    ), color = 'red')
  for (i in seq_along(f_peaks[, 2])){
    g <- g + geom_vline(xintercept = indt_peak[f_peaks[i,2], orig_row],
                        color = 'blue')
  }
  # print(g)
  return(list(plot = g, peaks = f_peaks,
              m_center = indt_peak[m_center, orig_row]))
}


#' Find the center of (mass) volume and peaks
#' @param indt input data.table as result from his_from_case
#' @param peak.percent peak area as percent, ex. 0.05 means that the 5% on the top is consider as peak area.
#' @return a list of (plot = graphic, peaks = matrix of peaks, and m_center = row index for the center)
#' @export
volume_center <- function(
  indt = NULL,
  peak.percent = 0.02,
  do.par = TRUE
){
  # stopifnot(column %in% colnames(indt)
  all_cols <- colnames(indt)
  all_cols <- all_cols[!all_cols %in% c('ts', 'case', 'orig_row')]
  indt[, orig_row := .I]
  row_has_max <- vector(mode = 'integer', length = length(all_cols))
  # col_max <- vector(mode = 'double', length = length(all_cols))
  col_max <- Rfast::colMaxs(as.matrix(indt[, .SD, .SDcols = all_cols]), value = TRUE)
  # indt_peak <- list()
  if (isTRUE(do.par)) {
    doParallel::registerDoParallel(parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    ret <- foreach::foreach(i = 1:length(all_cols), .combine = c) %dopar% {
      # col_max[i] <- max(indt[[all_cols[i]]], na.rm = TRUE)
      if (!is.infinite(col_max[i])) {
        peak_min <- col_max[i] * (1 - peak.percent)
        indt_peak <- indt[get(all_cols[i]) >= peak_min, .SD, 
                          .SDcols = c(all_cols[i], "orig_row")
                          ]
        # find the center of mass
        m_center <- sum(cumsum(indt_peak[, get(all_cols[i])]) <= 
                          0.5 * sum(indt_peak[, get(all_cols[i])])
        )
        row_has_max[i] <- indt_peak[m_center, orig_row]
      } else {
        row_has_max[i] <- NA_integer_
      }
      row_has_max[i]
    }
    doParallel::stopImplicitCluster()
  } else {
    for (i in seq_along(all_cols)) {
      # col_max[i] <- max(indt[[all_cols[i]]], na.rm = TRUE)
      if (!is.infinite(col_max[i])) {
        peak_min <- col_max[i] * (1 - peak.percent)
        indt_peak <- indt[get(all_cols[i]) >= peak_min, .SD, 
                          .SDcols = c(all_cols[i], "orig_row")
                          ]
        # find the center of mass
        m_center <- sum(cumsum(indt_peak[, 1]) <= 
                          0.5 * sum(indt_peak[, 1])
        )
        row_has_max[i] <- indt_peak[m_center, orig_row]
      } else {
        row_has_max[i] <- NA_integer_
      }
    }
    ret <- row_has_max
  }
  return(ret)
}


#' Find peak time and volume center for all nodes/reaches along a river segment
#'
#' This function reads all hydrographs of nodes/reaches along a river segment
#' and find the positions of the peaks (max_value) and positions of the volume
#' center of the area that is above the horizontal line defined by
#' (1 - peak.percent) * max_value
#'
#' @param river Name of the river
#' @param case.list List of cases
#' @param case.desc Description of the cases
#' @param sobek.project Path to sobek project
#' @param from.km Begin of the segment
#' @param to.km End of the segment
#' @param km.break get value for only every km.break
#' @param master.tbl Master table
#' @param param discharge/waterlevel
#' @param peak.percent Percentage of the max to define the area for 
#' volume-center calculating  
#' @param do.par Parallel computing.
#'
#' @return a data.table
#' @export
peak_time <- function(
  river = NULL,
  case.list = NULL,
  case.desc = case.list,
  sobek.project = NULL,
  from.km = -Inf,
  to.km = Inf,
  km.step = NULL,
  master.tbl = NULL,
  param = 'discharge',
  peak.percent = NULL,
  do.par = TRUE
) {
  stopifnot(param %in% c('discharge', 'waterlevel'))
  id_type <- ifelse(param == 'discharge', 'qID', 'wID')
  # get table of IDs of the segment from the master.tbl
  id_tbl <- get_segment_id_tbl(
    river = river,
    from.km = from.km,
    to.km = to.km,
    case.list = case.list, 
    case.desc = case.desc,
    master.tbl = master.tbl)[ID_TYPE == id_type]
  if (!is.null(km.step)) {
    id_tbl <- filter(id_tbl, km %% km.step == 0 | nchar(besonderheit) > 0)
  }
  case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
  setorder(id_tbl, km)
  # read data from cases
  for (i in case.list) {
    indt_arg <- list(case.list = i, 
                     sobek.project = sobek.project,
                     idtype = id_tbl[case == i, ID_F],
                     param = param)
    names(indt_arg)[3] <- id_type
    indt <- do.call(his_from_case, indt_arg)
    all_cols <- colnames(indt)
    all_cols <- all_cols[!all_cols %in% c('ts', 'case', 'orig_row')]
    col_max_pos <- Rfast::colMaxs(as.matrix(indt[case == i, .SD,
                                                 .SDcols = all_cols]))
    case_ts <- indt[, ts]
    col_max_time <- vector() 
    for (j in seq_along(col_max_pos)) {
      col_max_time[j] <- case_ts[col_max_pos[j]]
    }
    id_tbl[case == i, Peak_Time := col_max_time]
    if (!is.null(peak.percent)) {
      col_max_pos <- volume_center(indt[case == i], 
                                   peak.percent = peak.percent,
                                   do.par = do.par)
      for (j in col_max_pos) {
        col_max_time[j] <- case_ts[col_max_pos[j]]
      }
      id_tbl[case == i, Volume_Center := col_max_time]
    } 
  }
  id_tbl <- merge(id_tbl, case_tbl, by = 'case', sort = FALSE)
  return(id_tbl)
}



#' Plot peak time
#'
#' This function make ggplot and plotly for the peak time table (output from 
#' peak_time function)
#'
#' @param indt data.table output from peak_time
#' @param date.breaks date breaks for the y-axis
#' @param date.labels date labels for the y-axis
#' @param km.step only plot for every km.step (and for all "besonderheit")
#' @param pt.size Point size
#' @param txt.size Text size for both x-axes
#' @param color.by Variable for colors
#' @param shape.by Variable for shapes
#' @param peak.type Type of peak time ('vc' for volume center, 'peak' for time of peak or 'both')
#' @param overlap List of overlapping labels on the x2 (top) axis
#' @return a list of two plot (g, p)
#' @export
plot_peak_time <- function(
  indt,
  date.breaks = '2 hours',
  date.labels = '%d.%m - %H',
  km.step = NULL,
  pt.size = 2,
  txt.size = 10,
  color.by = 'case',
  shape.by = 'Type',
  peak.type = c('peak', 'vc', 'both'),
  overlap = overlap_rhein
) {
  # stopifnot('Peak_Time' %in% colnames(indt))
  peak.type = match.arg(peak.type, c('peak', 'vc', 'both'))
  if (!is.null(km.step)) {
    indt <- filter(indt, km %% km.step <= 0.2 | nchar(besonderheit) > 0) %>%
      as.data.table()
  }
  b_tick <- indt[case == unique(case)[[1]] &
                   nchar(besonderheit) > 0,
                 c("km", "besonderheit")]
  if (!is.null(overlap)) {
    for (i in seq_along(overlap)) {
      overlap_i_pos <-
        b_tick[grepl(overlap[[i]], besonderheit), which = TRUE]
      if (isTRUE(overlap_i_pos > 1)) {
        overlap_nchar <- nchar(b_tick[overlap_i_pos -  1, besonderheit])
        b_tick[overlap_i_pos,
               besonderheit := str_replace(besonderheit, 'Polder_|DRV_', '')]
        b_tick[overlap_i_pos, 
               besonderheit := paste(str_dup(' ', 2 * overlap_nchar),
                                     '--', besonderheit)]
      }
    }
  }
  measure_cols <- switch(peak.type,
    peak = c('Peak_Time'),
    vc = c('Volume_Center'),
    both = c('Peak_Time', 'Volume_Center')
  )
  all_cols <- c('km', 'besonderheit', measure_cols)
  if (color.by != 'Type') all_cols <- c(all_cols, color.by)
  if (shape.by != 'Type') all_cols <- c(all_cols, shape.by)
  all_cols <- unique(all_cols)
  indt <- indt[, .SD, .SDcols = all_cols] %>%
    melt(measure.vars = measure_cols, variable.name = 'Type', 
         value.name = 'Zeitpunkt')
  indt[, Zeitpunkt := as.POSIXct(Zeitpunkt, tz = "GMT", origin = '1970-01-01')]
  g <- ggplot(data = indt, 
              aes(x = km, y = Zeitpunkt, 
                  color = !!ensym(color.by), 
                  shape = !!ensym(shape.by)
                  )
              ) +
    scale_y_datetime(
      name = paste('Eintrittszeitpunkt (', date.labels, ')', sep = ''),
      date_breaks = date.breaks, 
      date_labels = date.labels) +
    geom_point(size = pt.size) + theme_grey() +
    scale_x_continuous(
      name = 'FLUSS KM',
      breaks = pretty(min(indt$km):max(indt$km), 20, 20),
      sec.axis = dup_axis(
        name = 'STATION',
        breaks = b_tick$km,
        labels = b_tick$besonderheit)
      ) +
    theme(
      axis.text.x.top =
        element_text(
          angle = 90,
          hjust = 0,
          vjust = 0.5,
          size = txt.size
        ),
      axis.text.x.bottom =
        element_text(
          angle = 0,
          hjust = 0.5,
          size = txt.size
        ),
      legend.position = 'bottom'
    ) +
    # scale_shape_manual(values = c(1, 3)) +
    geom_vline(xintercept = indt[nchar(besonderheit) > 0, km],
               color = 'grey', size = 0.2)
  # g
  # p <- plotly::plot_ly(data = indt, 
  #                      x = ~km,
  #                      y = ~Zeitpunkt,
  #                      color = ~case,
  #                      symbol = ~Type
  # ) %>%
  #   plotly::add_markers(
  #     xaxis = 'x2',
  #   ) %>%
  #   plotly::layout(
  #     legend = list(orientation = 'h',
  #                   yanchor = 'bottom'),
  #     # xaxis = list(rangemode = "tozero"),
  #     xaxis2 = list(
  #       side = 'bottom',
  #       # rangemode = "tozero",
  #       anchor = "y",
  #       # overlaying = "x",
  #       ticktext = indt[nchar(besonderheit) > 0, besonderheit],
  #       tickvals = indt[nchar(besonderheit) > 0, km]
  #     ),
  #     xaxis2 = list(
  #       side = 'top',
  #       # rangemode = "tozero",
  #       anchor = "y",
  #       # overlaying = "x",
  #       ticktext = indt[nchar(besonderheit) > 0, besonderheit],
  #       tickvals = indt[nchar(besonderheit) > 0, km]
  #     ),
  #     yaxis = list(dtick = 86400000 / 6)
  #   )
  # p
  return(g)
}
