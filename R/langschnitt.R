#' Plot long profile for a DRV
#' @param Name Name of the DRV
#' @param case.list List of cases to plot, default is all
#' @param case.desc Standard naming of case.list
#' @param sobek.project Path to sobek project
#' @param param dicharge/waterlevel
#' @param lt.by Linetype defining by this value
#' @param color.by Coloring by this value
#' @param facet.by Facetting by this value
#' @param compare.by Calculating delta by this value. Default 'zustand'
#' @param cmp.sort Should comparing parameter be sorted. Default is FALSE
#' @param group.by Groupping for delta calculation
#' @param color.name Name of color in the legend
#' @param lt.name Name of linetype in the legend
#' @param delta Should delta also plotted?
#' @param reserve.x Logical. If TRUE the x-axis will be reserved
#' @param x.lab x-axis label
#' @param y.lab y-axis label
#' @param to.upstream distance (km) to upstream of the DRV to be included in the graphic
#' @param to.downstream distance (km) to downstream of the DRV to be included in the graphic
#' @param y2.scale scale of y2-axis. Default value will be automatic calculated.
#' @param y2.tick1 The value of the first tick on the y2-axis. Using this and y2.scale to make y2-axis looks nice.
#' @param plot.title Title of the graphic
#' @param text.size Size of text
#' @param text.x.top.angle Angle of text at top
#' @param text.x.bottom.angle Angle of text at bottom
#' @param ntick.x Minimum number of ticks on x-axis. Default = 10
#' @param a.fill Fill color of the highlight area
#' @param a.alpha Transparent ratio (alpha blending) of the highlight area
#' @param overlap List of overlap labels should be avoid
#' @param master.tbl Master table
#' @param verbose Print some messages if TRUE
#' @return a ggplot2 graphic
#' @export
plot_drv <- function(
  name = NULL,
  case.list = NULL,
  case.desc = case.list,
  sobek.project = NULL,
  param = 'discharge',
  lt.by = 'zustand',
  color.by = 'vgf',
  facet.by = NULL,
  compare.by = 'zustand',
  cmp.sort = FALSE,
  group.by = compare.by,
  color.name = 'Farbe',
  lt.name = 'Linienart',
  delta = FALSE,
  reserve.x = FALSE,
  x.lab = 'Lage (KM)',
  y.lab = ifelse(param == 'discharge',
                 'Abfluss (m³/s)', 'Wasserstand (m+NHN)'),
  to.upstream = 0,
  to.downstream = 0,
  y2.scale = NULL,
  y2.tick1 = NULL,
  plot.title = NULL,
  text.size = 12,
  text.x.top.angle = 90L,
  text.x.top.size = 8L,
  text.x.bottom.angle = 0L,
  ntick.x = 10L,
  a.fill =  exl_std[3],
  a.alpha = 0.1,
  overlap = NULL,
  master.tbl = NULL,
  verbose = TRUE
){
  stopifnot(is.numeric(to.upstream) & is.numeric(to.downstream))
  case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
  if (!is.null(compare.by)){
    if(!compare.by %in% c('zustand', 'vgf', 'notiz', 'zielpegel')){
      stop("compare.by must be one of ('zustand', 'vgf', 'notiz', 'zielpegel')")
    }
    cmp_vars <- unique(case_tbl[, get(compare.by)])
    if (isTRUE(cmp.sort)) cmp_vars <- sort(cmp_vars)
    if (length(cmp_vars) != 2) {
      stop('compare.by must have two values not: ',
           str_flatten(cmp_vars, collapse = ", " ))
    }
  }
  if (!is.null(group.by)){
    grp_vars <- unique(case_tbl[, get(group.by)])
    if (!group.by %in% c('zustand', 'vgf', 'notiz', 'zielpegel')){
      stop("group.by must be one of ('zustand', 'vgf', 'notiz', 'zielpegel')")
    }
  }
  if (isTRUE(delta)){
    if (is.null(compare.by) | is.null(group.by)){
      stop('For caculating delta, compare.by and group.by must be specified!')
    }
    if(compare.by != group.by){
      total_case <- unique(as.vector(outer(cmp_vars, grp_vars, paste, sep="_")))
      if (length(total_case) != length(case.list)){
        stop("Combination of compare.by and group.by does not have the same length as case.list")
      }
    }
  }
  if (is.null(plot.title)){
    plot.title <- paste('Längsschnitt',
                       str_extract(y.lab, 'Abfluss|Wasserstand'),
                       'entlang DRV:', name
                       )
  }
  #----get data----
  if (verbose) print('Reading data...')
  data_tbl <- get_drv_data(name = name,
                           case.list = case.list,
                           case.desc = case.desc,
                           sobek.project = sobek.project,
                           param = param,
                           master.tbl = master.tbl,
                           to.upstream = to.upstream,
                           to.downstream = to.downstream,
                           get.max = TRUE,
                           verbose = verbose)
  data_tbl <- merge(data_tbl, case_tbl, by = 'case', sort = FALSE)
  # data_tbl[, besonderheit := gsub('DRV_', '', besonderheit)]
  drv_begin_tick <- master.tbl[
    grepl(
      pattern = paste("DRV", name, 'Begin', sep = "_"),
      x = besonderheit
      ),
    km
    ]
  drv_end_tick <- master.tbl[
    grepl(
      pattern = paste("DRV", name, 'End', sep = "_"),
      x = besonderheit
    ),
    km
    ]
  b_tick <- data_tbl[case == case.list[[1]] &
                       nchar(besonderheit) > 0,
                     c("km", "besonderheit")
                     ]
  # processing overlap labels
  if (!is.null(overlap)){
    for (i in seq_along(overlap)){
      # overlap_i <- b_tick[grepl(overlap[[i]], besonderheit), besonderheit][[1]]
      overlap_i_pos <- b_tick[grepl(overlap[[i]], besonderheit), which = TRUE]
      if (isTRUE(overlap_i_pos > 1)){
        overlap_nchar <- nchar(b_tick[overlap_i_pos -  1, besonderheit])
        b_tick[overlap_i_pos,
               besonderheit := str_replace(besonderheit, 'Polder_|DRV_', '')]
        b_tick[overlap_i_pos, besonderheit := paste(str_dup(' ', 2* overlap_nchar),
                                                    '--', besonderheit)]
      }
    }
  }
  x_min <- data_tbl[, min(km, na.rm = TRUE)]
  x_max <- data_tbl[, max(km, na.rm = TRUE)]
  y1_min <- data_tbl[, min(scheitel, na.rm = TRUE)]
  y1_max <- data_tbl[, max(scheitel, na.rm = TRUE)]
  x_pretty <- pretty(x_min:x_max, ntick.x, ntick.x)
  #----delta == TRUE----
  if (isTRUE(delta)){
    data_tbl[, group := seq_len(.N), by = get(compare.by)]
    y2_name <- paste('Delta', 
                     # str_to_sentence(compare.by),
                     ifelse(param == 'discharge', '(m³/s)', '(m)')
    )

    if (compare.by != group.by){
      y2_name <- paste('Delta', 
                       # str_to_sentence(compare.by),
                       'nach', str_to_sentence(group.by), 'gruppiert',
                       ifelse(param == 'discharge', '(m³/s)', '(m)')
      )
      lt.by <- compare.by
      color.by <- group.by
      data_tbl_delta <-
        dcast(data_tbl, group  ~ get(compare.by) + get(group.by)  ,
              value.var = 'scheitel')
      for (i in grp_vars){
        col_i <- paste('Delta', i, sep = '_')
        col_1 <- paste(cmp_vars[1], i, sep = '_')
        col_2 <- paste(cmp_vars[2], i, sep = '_')
        data_tbl_delta[, eval(col_i) := get(col_1) - get(col_2)]
        data_tbl_delta[, eval(col_1) := NULL]
        data_tbl_delta[, eval(col_2) := NULL]
      }
      data_tbl_delta <- melt(data_tbl_delta, id.vars = 'group',
                             variable.name = group.by,
                             value.name = 'delta',
                             sort = FALSE)
      data_tbl_delta <- data_tbl_delta[!is.na(delta)]
      colnames(data_tbl_delta)[2] <- 'delta_color'
      data_tbl <- merge(data_tbl, data_tbl_delta, by = 'group')
    } else{
      data_tbl_delta <-
        dcast(data_tbl, group  ~ get(compare.by),
              value.var = 'scheitel')
      col_1 <- cmp_vars[1]
      col_2 <- cmp_vars[2]
      data_tbl_delta[, delta := get(col_1) - get(col_2)]
      data_tbl_delta[, eval(col_1) := NULL]
      data_tbl_delta[, eval(col_2) := NULL]
      data_tbl_delta[, delta_color := paste('Delta',
                                            str_to_sentence(compare.by))
                     ]
      data_tbl <-
        merge(data_tbl, data_tbl_delta, by = 'group', sort = FALSE)
    }
    data_tbl[, delta := round(delta, 3)]
    y2_min <- min(data_tbl$delta, na.rm = TRUE)
    y2_max <- max(data_tbl$delta, na.rm = TRUE)
    y2_length <- y2_max - y2_min
    y1_length <- y1_max - y1_min
    if (y2_max - y2_min > 10) {
      y2_min <- round(y2_min, -1)
    } else {
      if (abs(y2_min) > 1) y2_min <- floor(y2_min)
    }
    if (is.null(y2.scale)){
      y2.scale <- y1_length * 0.5 * 1000 / y2_length
      for (i in 0:3) {
        # if (abs(y2.scale) > 1) y2.scale <- round(y2.scale)
        if (abs(y2.scale) > 10 ** i)
          y2.scale <- round(y2.scale, -i)
      }
      y2.scale <- y2.scale / 1000
      if (isTRUE(verbose)) print(paste('tried with y2.scale =', y2.scale))
    }
    y2_shift <- y1_min - y2_min * y2.scale
    # y2_max <- (y1_max - y2_shift)/ y2.scale
    y1_max <- max(y1_max,
                  y2_max * y2.scale + y2_shift)
    y1_min <- min(y1_min,
                  y2_min * y2.scale + y2_shift)
    if (y1_length < 10) {
      y1_max_1 <- y1_max * 100
      y1_min_1 <- y1_min * 100
      y1_pretty <- pretty(y1_min_1:y1_max_1, 5, 5)
      y1_pretty <- y1_pretty / 100
    } else{
      y1_pretty <- pretty(y1_min:y1_max, 5, 5)
    }
    check_y1_pretty <- (max(y1_pretty) - min(y1_pretty)) / (y1_max - y1_min)
    if (length(y1_pretty) < 5 | check_y1_pretty > 1){
      y1_min_1 <- y1_min * 10
      y1_max_1 <- y1_max * 10
      y1_pretty <- pretty(y1_min_1:y1_max_1, 5, 5)
      y1_pretty <- y1_pretty/10
      # y2_pretty <- (y1_pretty - y2_shift) / y2.scale
    }
    if (!is.null(y2.tick1)) {
      y2_shift = y1_pretty[1] - y2.tick1 * y2.scale
    }
    y2_pretty <- (y1_pretty - y2_shift) / y2.scale
    # print(y1_pretty)
    y2_pmin <- min(y2_pretty)
    y2_pmax <- max(y2_pretty)
    if (0 %between% c(y2_pmin, y2_pmax)){
      # print(y2_pretty)
      pos_y2_zero <- ceiling(abs(y2_pmax / (y2_pmax - y2_pmin)))
      if (pos_y2_zero > length(y1_pretty)) pos_y2_zero  <- length(y1_pretty)
      y2_shift <- y1_pretty[pos_y2_zero]
      y1_max <- max(y1_max,
                    y2_max * y2.scale + y2_shift)
      y1_min <- min(y1_min,
                    y2_min * y2.scale + y2_shift)
      y1_pretty <- pretty(y1_min:y1_max, 5, 5)
      check_y1_pretty <- (max(y1_pretty) - min(y1_pretty)) / (y1_max - y1_min)
      if (length(y1_pretty) < 5 | check_y1_pretty > 1){
        y1_min_1 <- y1_min * 10
        y1_max_1 <- y1_max * 10
        y1_pretty <- pretty(y1_min_1:y1_max_1, 5, 5)
        y1_pretty <- y1_pretty/10
      }
      if (!is.null(y2.tick1)) {
        y2_shift = y1_pretty[1] - y2.tick1 * y2.scale
      }
      y2_pretty <- (y1_pretty - y2_shift) / y2.scale
      y2_pretty <- unique(sort(c(y2_pretty, 0)))
    }
    data_tbl[get(compare.by) == cmp_vars[2], delta := NA]
    data_tbl[get(compare.by) == cmp_vars[2], delta_color := NA]
  }
  # adding DRV multi ticks
  
  #----add graphic----
  if (verbose) print('Preparing graphic...')
  # preparing data for highlighting DRV
  setorder(b_tick, km)
  b_tick[grepl('DRV_([^,;]*)_Begin', besonderheit), drv_start := km]
  b_tick[grepl('DRV_([^,;]*)_End', besonderheit), drv_end := km]
  data_tbl <- merge(data_tbl, b_tick, by = c('km', 'besonderheit'), sort = FALSE)
  g <- ggplot(data = data_tbl,
              aes(x = km,
                  linetype = !!ensym(lt.by),
                  color = !!ensym(color.by),
                  y = scheitel)
  ) +
    theme_bw() +
    theme(legend.position = 'bottom',
          axis.text.x.top =
            element_text(angle = text.x.top.angle,
                         hjust = 0,
                         vjust = 0.5,
                         size = text.x.top.size),
          axis.text.x.bottom =
            element_text(angle = text.x.bottom.angle,
                         hjust = 0.5,
                         size = text.size)
    ) +
    labs(title = plot.title) +
    ylab(y.lab)
  if (isTRUE(reserve.x)){
    g <- g +
      scale_x_reverse(
        name = x.lab,
        breaks = x_pretty,
        sec.axis =  dup_axis(
          breaks = b_tick$km,
          labels = b_tick$besonderheit,
          name = 'Station'
        )
      )
  } else{
    g <- g +
      scale_x_continuous(
        name = x.lab,
        breaks = x_pretty,
        sec.axis =  dup_axis(
          breaks = b_tick$km,
          labels = b_tick$besonderheit,
          name = 'Station'
        )
      )
  }
  g <- g + geom_line(size = 1)
  g$labels$colour <- color.name
  g$labels$linetype <- lt.name
  if (isTRUE(delta)){
    g <- g + geom_line(
      data = data_tbl,
      aes(
        y = delta * y2.scale + y2_shift,
        # shape = !!ensym(compare.by),
        color = delta_color,
        linetype = 'Delta'
      ),
      size = 1
    ) +
      scale_y_continuous(
        breaks = y1_pretty,
        labels = y1_pretty,
        sec.axis =
          sec_axis(
            trans = ~ (. - y2_shift) / y2.scale,
            breaks = y2_pretty,
            labels = round(y2_pretty, 3),
            name = y2_name
          )
      )
  }
  #----adding DRV rectangle and facet----
  # if (highlight.all == TRUE)
  # add_drv_anno <- function(start, end, g) {
  #   g <- g + annotate(
  #     'rect',
  #     xmin = start,
  #     xmax = end,
  #     ymin = -Inf,
  #     ymax = Inf,
  #     fill =  a.fill,
  #     alpha = a.alpha
  #   )
  # }
  for (i in seq_along(b_tick[!is.na(drv_end), drv_end])){
    g <- g + annotate(
      'rect',
      xmin = b_tick[!is.na(drv_start), drv_start][i],
      xmax = b_tick[!is.na(drv_end), drv_end][i],
      ymin = -Inf,
      ymax = Inf,
      fill =  a.fill,
      alpha = a.alpha
    )
  }
  if (!is.null(facet.by)){
    g <- g + facet_grid(rows = ensym(facet.by))
  }

  return(g)
}


#' Plot long profile for a river segment
#' @param river Name of the river
#' @param from.km Start location (km)
#' @param to.km End location (km)
#' @param case.list List of cases to plot, default is all
#' @param case.desc Standard naming of case.list
#' @param sobek.project Path to sobek project
#' @param param dicharge/waterlevel
#' @param lt.by Linetype defining by this value
#' @param color.by Coloring by this value
#' @param compare.by Calculating delta by this value
#' @param cmp.sort Should comparing parameter be sorted. Default is FALSE
#' @param group.by Groupping for delta calculation
#' @param color.name Name of color in the legend
#' @param lt.name Name of linetype in the legend
#' @param delta Should delta also plotted?
#' @param reserve.x Logical. If TRUE the x-axis will be reserved
#' @param x.lab x-axis label
#' @param y.lab y-axis label
#' @param to.upstream distance (km) to upstream of the DRV to be included in the graphic
#' @param to.downstream distance (km) to downstream of the DRV to be included in the graphic
#' @param y2.scale scale of y2-axis. Default value will be automatic calculated
#' @param y2.tick1 The value of the first tick on the y2-axis. Using this and y2.scale to make y2-axis looks nice.
#' @param plot.title Title of the graphic
#' @param text.size Size of text
#' @param text.x.top.angle Angle of text at top
#' @param text.x.bottom.angle Angle of text at bottom
#' @param ntick.x Minimum number of ticks on x-axis. Default = 10
#' @param highlight A vector of two locations (km), ex. c(4, 10) to be highlighted
#' @param a.fill Fill color of the highlight area
#' @param a.alpha Transparent ratio (alpha blending) of the highlight area
#' @param overlap List of overlap labels should be avoid
#' @param master.tbl Master table
#' @param verbose Print some messages if TRUE
#' @return a ggplot2 graphic
#' @export
plot_longprofile <- function(
  river = NULL,
  from.km = -Inf,
  to.km = Inf,
  case.list = NULL,
  case.desc = case.list,
  sobek.project = NULL,
  param = 'discharge',
  lt.by = 'zustand',
  color.by = 'vgf',
  # facet.by = NULL,
  # facet.scale = 'fixed',
  compare.by = 'zustand',
  cmp.sort = FALSE,
  group.by = compare.by,
  color.name = 'Farbe',
  lt.name = 'Linienart',
  delta = FALSE,
  reserve.x = FALSE,
  x.lab = 'Lage (KM)',
  y.lab = ifelse(param == 'discharge',
                 'Abfluss (m³/s)', 'Wasserstand (m+NHN)'),
  y2.scale = NULL,
  y2.tick1 = NULL,
  plot.title = NULL,
  text.size = 12,
  text.x.top.angle = 90L,
  text.x.top.size = 8L,
  text.x.bottom.angle = 0L,
  ntick.x = 10L,
  highlight = NULL,
  highlight.text = NULL,
  a.fill = exl_std[3],
  a.alpha = 0.1,
  overlap = NULL,
  master.tbl = NULL,
  verbose = TRUE
){
  stopifnot(length(unique(case.list)) == length(case.list))
  stopifnot(is.numeric(from.km) & is.numeric(to.km))
  stopifnot(to.km > from.km)
  if (!is.null(highlight)){
    stopifnot(is.numeric(highlight) & length(highlight) >1)
    if(!is.null(highlight.text)){
      stopifnot(length(highlight.text) >1)
    }
  }
  case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
  if (!is.null(compare.by)){
    if(!compare.by %in% c('zustand', 'vgf', 'notiz', 'zielpegel')){
      stop("compare.by must be one of ('zustand', 'vgf', 'notiz', 'zielpegel')")
    }
    cmp_vars <- unique(case_tbl[, get(compare.by)])
    if (isTRUE(cmp.sort)) cmp_vars <- sort(cmp_vars)
    if (length(cmp_vars) != 2) {
      stop('compare.by must have two values not: ',
           str_flatten(cmp_vars, collapse = ", " ))
    }
  }
  if (!is.null(group.by)){
    grp_vars <- unique(case_tbl[, get(group.by)])
    if (!group.by %in% c('hwe', 'zustand', 'vgf', 'notiz', 'zielpegel')){
      stop("group.by must be one of ('hwe', 'zustand', 'vgf', 'notiz', 'zielpegel')")
    }
  }
  if (isTRUE(delta)){
    if (is.null(compare.by) | is.null(group.by)){
      stop('For caculating delta, compare.by and group.by must be specified!')
    }
    if(compare.by != group.by){
      total_case <- unique(as.vector(outer(cmp_vars, grp_vars, paste, sep="_")))
      if (length(total_case) != length(case.list)){
        print("Combination of compare.by and group.by does not have the same length as case.list")
        print('notiz can be modified and used as a groupping parameter')
        stop('groupping by ', group.by,
             'is not unique for calculating delta between ', compare.by)
      }
    }
  }
  river_ids <- master.tbl[, .N, by = river]
  if (is.null(river)){
    setorder(river_ids, -N)
    river <- river_ids$river[[1]]
  } else{
    stopifnot(river %in% river_ids$river)
  }
  #----get data----
  if (verbose) print('Reading data...')
  # id_tbl <- get_segment_id_tbl(
  #   river = river,
  #   from.km = from.km,
  #   to.km = to.km,
  #   case.list = case.list,
  #   case.desc = case.desc,
  #   master.tbl = master.tbl
  # )
  data_tbl <- get_segment_data(
    river = river,
    from.km = from.km,
    to.km = to.km,
    case.list = case.list,
    case.desc = case.desc,
    param = param,
    get.max = TRUE,
    sobek.project = sobek.project,
    master.tbl = master.tbl,
    verbose = verbose
  )
  from.km <- max(min(data_tbl$km, na.rm = TRUE), from.km)
  to.km <- min(max(data_tbl$km, na.rm = TRUE), to.km)
  if (is.null(plot.title)){
    plot.title <- paste('Längsschnitt ',
                        str_extract(y.lab, 'Abfluss|Wasserstand'),
                        ' entlang ', river, ' von KM ',
                        format(from.km, nsmall = 2, decimal.mark = ","),
                        ' bis KM ',
                        format(to.km, nsmall = 2, decimal.mark = ","),
                        sep = ''
    )
  }
  data_tbl <- merge(data_tbl, case_tbl, by = 'case', sort = FALSE)
  # id_tbl[, besonderheit := gsub('DRV_', '', besonderheit)]
  b_tick <- data_tbl[case == case.list[[1]] &
                         nchar(besonderheit) > 0,
                       c("km", "besonderheit")
                     ]
  # processing overlap labels
  if (!is.null(overlap)){
    for (i in seq_along(overlap)){
      # overlap_i <- b_tick[grepl(overlap[[i]], besonderheit), besonderheit][[1]]
      overlap_i_pos <- b_tick[grepl(overlap[[i]], besonderheit), which = TRUE]
      if (isTRUE(overlap_i_pos > 1)){
        overlap_nchar <- nchar(b_tick[overlap_i_pos -  1, besonderheit])
        b_tick[overlap_i_pos,
               besonderheit := str_replace(besonderheit, 'Polder_|DRV_', '')]
        b_tick[overlap_i_pos, besonderheit := paste(str_dup(' ', 2* overlap_nchar),
                                                    '--', besonderheit)]
      }
    }
  }
  x_min <- data_tbl[, min(km, na.rm = TRUE)]
  x_max <- data_tbl[, max(km, na.rm = TRUE)]
  y1_min <- data_tbl[, min(scheitel, na.rm = TRUE)]
  y1_max <- data_tbl[, max(scheitel, na.rm = TRUE)]
  x_pretty <- pretty(x_min:x_max, ntick.x, ntick.x)
  #----delta == TRUE----
  if (isTRUE(delta)){
    data_tbl[, group := seq_len(.N), by = get(compare.by)]
    y2_name <- paste('Delta', 
                     # str_to_sentence(compare.by),
                     ifelse(param == 'discharge', '(m³/s)', '(m)')
                     )

    if (compare.by != group.by){
      y2_name <- paste('Delta', 
                       # str_to_sentence(compare.by),
                       'nach', str_to_sentence(group.by), 'gruppiert',
                       ifelse(param == 'discharge', '(m³/s)', '(m)')
                       )
      lt.by <- compare.by
      color.by <- group.by
      data_tbl_delta <-
        dcast(data_tbl, group  ~ get(compare.by) + get(group.by)  ,
              value.var = 'scheitel')
      for (i in grp_vars){
        col_i <- paste('Delta', i, sep = '_')
        col_1 <- paste(cmp_vars[1], i, sep = '_')
        col_2 <- paste(cmp_vars[2], i, sep = '_')
        data_tbl_delta[, eval(col_i) := get(col_1) - get(col_2)]
        data_tbl_delta[, eval(col_1) := NULL]
        data_tbl_delta[, eval(col_2) := NULL]
      }
      data_tbl_delta <- melt(data_tbl_delta, id.vars = 'group',
                             variable.name = group.by,
                             value.name = 'delta',
                             sort = FALSE)
      data_tbl_delta <- data_tbl_delta[!is.na(delta)]
      colnames(data_tbl_delta)[2] <- 'delta_color'
      data_tbl <- merge(data_tbl, data_tbl_delta, by = 'group')
    } else{
      data_tbl_delta <-
        dcast(data_tbl, group  ~ get(compare.by),
              value.var = 'scheitel')
      col_1 <- cmp_vars[1]
      col_2 <- cmp_vars[2]
      data_tbl_delta[, delta := get(col_1) - get(col_2)]
      data_tbl_delta[, eval(col_1) := NULL]
      data_tbl_delta[, eval(col_2) := NULL]
      data_tbl_delta[, delta_color := paste('Delta',
                                            str_to_sentence(compare.by))
                                            ]
      data_tbl <-
        merge(data_tbl, data_tbl_delta, by = 'group', sort = FALSE)
    }
    data_tbl[, delta := round(delta, 3)]
    y2_min <- min(data_tbl$delta, na.rm = TRUE)
    y2_max <- max(data_tbl$delta, na.rm = TRUE)
    y2_length <- y2_max - y2_min
    y1_length <- y1_max - y1_min
    if (y2_max - y2_min > 10) {
      y2_min <- round(y2_min, -1)
    } else {
      if (abs(y2_min) > 1) y2_min <- floor(y2_min)
    }
    if (is.null(y2.scale)){
      y2.scale <- y1_length * 0.5 * 1000 / y2_length
      for (i in 0:3) {
        # if (abs(y2.scale) > 1) y2.scale <- round(y2.scale)
        if (abs(y2.scale) > 10 ** i)
          y2.scale <- round(y2.scale, -i)
      }
      y2.scale <- y2.scale / 1000
      if (isTRUE(verbose)) print(paste('tried with y2.scale =', y2.scale))
    }
    y2_shift <- y1_min - y2_min * y2.scale
    # y2_max <- (y1_max - y2_shift)/ y2.scale
    y1_max <- max(y1_max,
                  y2_max * y2.scale + y2_shift)
    y1_min <- min(y1_min,
                  y2_min * y2.scale + y2_shift)
    if (y1_length < 10) {
      y1_max_1 <- y1_max * 100
      y1_min_1 <- y1_min * 100
      y1_pretty <- pretty(y1_min_1:y1_max_1, 5, 5)
      y1_pretty <- y1_pretty / 100
    } else{
      y1_pretty <- pretty(y1_min:y1_max, 5, 5)
    }
    check_y1_pretty <- (max(y1_pretty) - min(y1_pretty)) / (y1_max - y1_min)
    if (length(y1_pretty) < 5 | check_y1_pretty > 1){
      y1_min_1 <- y1_min * 10
      y1_max_1 <- y1_max * 10
      y1_pretty <- pretty(y1_min_1:y1_max_1, 5, 5)
      y1_pretty <- y1_pretty/10
      # y2_pretty <- (y1_pretty - y2_shift) / y2.scale
    }
    if (!is.null(y2.tick1)) {
      y2_shift = y1_pretty[1] - y2.tick1 * y2.scale
    }
    y2_pretty <- (y1_pretty - y2_shift) / y2.scale
    # print(y1_pretty)
    y2_pmin <- min(y2_pretty)
    y2_pmax <- max(y2_pretty)
    if (0 %between% c(y2_pmin, y2_pmax)){
      # print(y2_pretty)
      pos_y2_zero <- ceiling(abs(y2_pmax / (y2_pmax - y2_pmin)))
      if (pos_y2_zero > length(y1_pretty)) pos_y2_zero  <- length(y1_pretty)
      y2_shift <- y1_pretty[pos_y2_zero]
      y1_max <- max(y1_max,
                    y2_max * y2.scale + y2_shift)
      y1_min <- min(y1_min,
                    y2_min * y2.scale + y2_shift)
      y1_pretty <- pretty(y1_min:y1_max, 5, 5)
      check_y1_pretty <- (max(y1_pretty) - min(y1_pretty)) / (y1_max - y1_min)
      if (length(y1_pretty) < 5 | check_y1_pretty > 1){
        y1_min_1 <- y1_min * 10
        y1_max_1 <- y1_max * 10
        y1_pretty <- pretty(y1_min_1:y1_max_1, 5, 5)
        y1_pretty <- y1_pretty/10
      }
      if (!is.null(y2.tick1)) {
        y2_shift = y1_pretty[1] - y2.tick1 * y2.scale
      }
      y2_pretty <- (y1_pretty - y2_shift) / y2.scale
      y2_pretty <- unique(sort(c(y2_pretty, 0)))
    }
    data_tbl[get(compare.by) == cmp_vars[2], delta := NA]
    data_tbl[get(compare.by) == cmp_vars[2], delta_color := NA]
  }
  #----add graphic----
  if (verbose) print('Preparing graphic...')
  g <- ggplot(data = data_tbl,
              aes(x = km,
                  linetype = !!ensym(lt.by),
                  color = !!ensym(color.by),
                  y = scheitel)
  ) +
    theme_bw() +
    theme(legend.position = 'bottom',
          axis.text.x.top =
            element_text(angle = text.x.top.angle,
                         hjust = 0,
                         vjust = 0.5,
                         size = text.x.top.size),
          axis.text.x.bottom =
            element_text(angle = text.x.bottom.angle,
                         hjust = 0.5,
                         size = text.size)
    ) +
    labs(title = plot.title) +
    ylab(y.lab)
  if (isTRUE(reserve.x)){
    g <- g +
      scale_x_reverse(
        name = x.lab,
        breaks = x_pretty,
        sec.axis =  dup_axis(
          breaks = b_tick$km,
          labels = b_tick$besonderheit,
          name = 'Station'
        )
      )
  } else{
    g <- g +
      scale_x_continuous(
        name = x.lab,
        breaks = x_pretty,
        sec.axis =  dup_axis(
          breaks = b_tick$km,
          labels = b_tick$besonderheit,
          name = 'Station'
        )
      )
  }
  g <- g + geom_line(size = 1)
  g$labels$colour <- color.name
  g$labels$linetype <- lt.name
  if (isTRUE(delta)){
    g <- g + geom_line(
      data = data_tbl,
      aes(
        y = delta * y2.scale + y2_shift,
        # shape = !!ensym(compare.by),
        color = delta_color,
        linetype = 'Delta'
      ),
      size = 1
    ) +
      scale_y_continuous(
        breaks = y1_pretty,
        sec.axis =
          sec_axis(
            trans = ~ (. - y2_shift) / y2.scale,
            breaks = y2_pretty,
            labels = round(y2_pretty, 3),
            name = y2_name
          )
      )
  }
  #----adding highlight area and facet----
  # if (!is.null(facet.by)){
  #   g <- g + facet_wrap(~ get(facet.by), scales = facet.scale)
  # }
  if (!is.null(highlight)){
    # hl_count <- length(highlight)
    g <- g + annotate('rect',
                      xmin = highlight[[1]],
                      xmax = highlight[[2]],
                      ymin = -Inf, ymax = Inf,
                      fill =  a.fill,
                      alpha = a.alpha
    )
    if(!is.null(highlight.text)){
      g <- g + annotate(
        'text',
        x = highlight[[1]],
        y = y1_min + abs(y1_max - y1_min) / 2,
        label = highlight.text[[1]],
        angle = 90, vjust = 0, hjust = 0.5
      ) +
        annotate(
          'text',
          x = highlight[[2]],
          y = y1_min + abs(y1_max - y1_min) / 2,
          label = highlight.text[[2]],
          angle = 90, vjust = 0, hjust = 0.5
        )
    }
  }

  return(g)
}
