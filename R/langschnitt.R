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
  x_pretty <- pretty(x_min:x_max, 10)
  # if (nrow(b_tick) > 0){
  #   x_ticks <-
  # }
  #----adding delta----
  #FIXME: correcting delta with compare.by
  if (isTRUE(delta)){
    x_min <- min(data_tbl$km, na.rm = TRUE)
    x_max <- max(data_tbl$km, na.rm = TRUE)
    y_min <- min(data_tbl$scheitel, na.rm = TRUE)
    y_max <- max(data_tbl$scheitel, na.rm = TRUE)
    # saving data file
    if(!is.null(save.tbl)){
      fwrite(data_m2, file = save.tbl,
             quote = FALSE,
             sep = "\t", dec = ",")
    }
    y2_scale <- y2.scale
    y2_min <- min(data_m2$Delta, na.rm = TRUE)
    y_pretty <- pretty(y_min:y_max, 5, 5)
    y2_shift <- y_pretty[2]
    y2_max <- y_max/y2_scale
    y2_pretty <- pretty(y2_min:y2_max, 5, 5)
    g <- ggplot(data = data_m2,
                aes(x = km,
                    y = get(zustand1),
                    color = zustand1,
                    linetype = VGF
                ))+
      theme_bw()+
      theme(legend.position = 'bottom',
            axis.text.x = element_text(angle = text.x.angle,
                                       hjust = 0,
                                       size = text.size)) +
      xlab(x.lab) + ylab(y.lab) +
      labs(title) +
      geom_line(size = 1) +
      geom_line(aes(y = get(zustand2), color = zustand2),
                size = 1) +
      scale_color_hue(name = 'Farbe')+
      scale_linetype_discrete(name = 'Linienart')+
      scale_x_reverse(
        name = x.lab,
        breaks = pretty(x_min:x_max, 20),
        sec.axis =  dup_axis(
          name = 'Station',
          breaks = b_tick$km,
          labels = b_tick$besonderheit)) +
      facet_wrap(.~hwe, ncol = ncol) +
      geom_line(aes(y = Delta * y2_scale + y2_shift,
                    color = 'Differenz'),
                size = 1) +
      scale_y_continuous(
        breaks = y_pretty,
        sec.axis = sec_axis(trans = ~. * 1 / y2_scale - y2_shift / y2_scale,
                            breaks =  (y_pretty - y2_shift) / y2_scale,
                            name = paste(sub("\\(.*)", "", y.lab),
                                         "Differenz",
                                         sep = ""
                            )
        )
      )
  } else{
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
