#' Plot long profile of selected case
#' @param Name Name of the DRV
#' @param case.list List of cases to plot, default is all
#' @param case.desc Standard naming of case.list
#' @param sobek.project Path to sobek project
#' @param param dicharge/waterlevel
#' @param lt.by Linetype defining by this value
#' @param color.by Coloring by this value
#' @param facet.by Facetting by this value
#' @param compare.by Calculating delta by this value
#' @param group.by Grouping for delta
#' @param color.name Name of color in the legend
#' @param lt.name Name of linetype in the legend
#' @param delta Should delta also plotted?
#' @param reserve.x Logical. If TRUE the x-axis will be reserved
#' @param x.lab x-axis label
#' @param y.lab y-axis label
#' @param to.upstream distance (km) to upstream of the DRV to be included in the graphic
#' @param to.downstream distance (km) to downstream of the DRV to be included in the graphic
#' @param y2.scale scale of y2-axis
#' @param plot.title Title of the graphic
#' @param text.size Size of text
#' @param text.x.top.angle Angle of text at top
#' @param text.x.bottom.angle Angle of text at bottom
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
  compare.by = NULL,
  group.by = NULL,
  color.name = 'Farbe',
  lt.name = 'Linienart',
  delta = FALSE,
  reserve.x = FALSE,
  x.lab = 'Lage (KM)',
  y.lab = ifelse(param == 'discharge',
                 'Abfluss (m³/s)', 'Wasserstand (m+NHN)'),
  to.upstream = 0,
  to.downstream = 0,
  y2.scale = 2,
  plot.title = NULL,
  text.size = 12,
  text.x.top.angle = 90L,
  text.x.top.size = 8L,
  text.x.bottom.angle = 0L,
  master.tbl = rhein_tbl,
  verbose = TRUE
){
  stopifnot(is.numeric(to.upstream) & is.numeric(to.downstream))
  if (is.null(plot.title)){
    plot.title <- paste('Längsschnitte',
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
  case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
  data_tbl <- merge(data_tbl, case_tbl, by = 'case', sort = FALSE)
  data_tbl[, besonderheit := gsub('DRV_', '', besonderheit)]
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
  if (param == 'discharge'){
    b_tick <- data_tbl[case == case.list[[1]] & ID_TYPE == 'qID' &
                         nchar(besonderheit) > 0,
                       c("km", "besonderheit")]
  } else{
    b_tick <- data_tbl[case == case.list[[1]] &
                       ID_TYPE == 'wID' & nchar(besonderheit) > 0,
                       c("km", "besonderheit")]
  }
  x_min <- data_tbl[, min(km, na.rm = TRUE)]
  x_max <- data_tbl[, max(km, na.rm = TRUE)]
  y1_min <- data_tbl[, min(scheitel, na.rm = TRUE)]
  y1_max <- data_tbl[, max(scheitel, na.rm = TRUE)]
  y2_max <- y1_max/y2.scale
  x_pretty <- pretty(x_min:x_max, 10)
  #----adding delta----
  #FIXME: correcting delta with compare.by
  if (isTRUE(delta) & !is.null(compare.by)){
    # if delta is TRUE, compare.by must be given

    #----delta == TRUE----
    col_get_delta <- 'scheitel'
    con_chk <-
      compare.by %in% c('zustand', 'vgf', 'hwe', 'notiz', 'zielpegel')
    stopifnot(con_chk)
    cmp_cols <- unique(data_tbl[, get(compare.by)])
    if (length(cmp_cols) != 2) {
      stop('compare.by must have two values not: ', cmp_cols)
    }
    if (is.null(group.by)) {
      # for (i in seq_along(cmp_cols)){
      #   data_tbl[get(compare.by) == cmp_cols[i], group := i]
      # }
      data_tbl[, group := seq_len(.N), by = eval(compare.by)]
      data_tbl_delta <- data_tbl[, .SD,
                                   .SDcols = c('group', col_get_delta,
                                               compare.by)]
      data_tbl_delta <-
        dcast(data_tbl_delta, group ~ get(compare.by)  ,
              value.var = col_get_delta)
      col_1 <- cmp_cols[1]
      col_2 <- cmp_cols[2]
      data_tbl_delta[, delta := get(col_1) - get(col_2)]
      data_tbl_delta[, eval(col_1) := NULL]
      data_tbl_delta[, eval(col_2) := NULL]
      data_tbl_delta <- merge(data_tbl, data_tbl_delta, by = 'group')
      data_tbl_delta$group = NULL
    }else{
      group_chk <- length(data_tbl[, get(group.by)])
      if (group_chk > 1) {
        data_tbl_delta <- data_tbl[, .SD,
                                     .SDcols = c(group.by, col_get_delta,
                                                 compare.by)]
        data_tbl_delta <-
          dcast(data_tbl_delta, get(group.by) ~ get(compare.by),
                value.var = col_get_delta)
        colnames(data_tbl_delta)[1] <- group.by
        for (i in seq_along(col_get_delta)) {
          col_i <- paste('delta_', col_get_delta[i], sep = "")
          col_1 <-
            paste(col_get_delta[i], cmp_cols[1], sep = "_")
          col_2 <-
            paste(col_get_delta[i], cmp_cols[2], sep = "_")
          id_data_delta[, eval(col_i) := get(col_1) - get(col_2)]
          id_data_delta[, eval(col_1) := NULL]
          id_data_delta[, eval(col_2) := NULL]
        }
        id_data_max <-
          merge(id_data_max, id_data_delta, by = group.by)
      }else{
        print(paste(
          'To get delta by',
          group.by,
          'grouping, number of groups must be more than 1'
        ))
      }
    }

  } else{
    #----delta == FALSE----
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
      scale_y_continuous(
        # breaks = pretty(y_min:y_max, 10),
        name = y.lab
        )
    if (isTRUE(reserve.x)){
      g <- g +
        scale_x_reverse(
          name = x.lab,
          # breaks = pretty(x_min:x_max, 10),
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
          # breaks = pretty(x_min:x_max, 10),
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
  }
  #----adding DRV rectangle----
  g <- g + annotate('rect',
                    xmin = drv_end_tick,
                    xmax = drv_begin_tick,
                    ymin = -Inf, ymax = Inf,
                    fill =  exl_std[3],
                    alpha = 0.1
                )

  return(g)
}
