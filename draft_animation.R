so_prj <- 'C:/rhein.lit'
#----standard parameter-----
river = 'Rhein'
from.km = 614
to.km = 780
case.list = c(
  'Bezugszustand_ZPK_HW1988_Selten_1828_newReg',
  'Planzustand_ZPK_HW1988_Selten_ohne_Niederrhein'
  )
case.desc = case.list
sobek.project = so_prj
param = 'discharge'
lt.by = 'zustand'
color.by = 'vgf'
facet.by = 'hwe'
facet.scale = 'fixed'
compare.by = 'zustand'
group.by = compare.by
color.name = 'Farbe'
lt.name = 'Linienart'
delta = TRUE
reserve.x = FALSE
x.lab = 'Lage (KM)'
y.lab = ifelse(param == 'discharge',
               'Abfluss (m³/s)', 'Wasserstand (m+NHN)')
y2.scale = NULL
plot.title = NULL
text.size = 12
text.x.top.angle = 90L
text.x.top.size = 8L
text.x.bottom.angle = 0L
ntick.x = 10L
highlight = NULL
highlight.text = NULL
a.fill = exl_std[3]
a.alpha = 0.1
master.tbl = rhein_tbl
verbose = TRUE

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
#' @param facet.by Facetting by this value
#' @param facet.scale Facetting scale. Default 'fixed'
#' @param compare.by Calculating delta by this value
#' @param color.name Name of color in the legend
#' @param lt.name Name of linetype in the legend
#' @param delta Should delta also plotted?
#' @param reserve.x Logical. If TRUE the x-axis will be reserved
#' @param x.lab x-axis label
#' @param y.lab y-axis label
#' @param to.upstream distance (km) to upstream of the DRV to be included in the graphic
#' @param to.downstream distance (km) to downstream of the DRV to be included in the graphic
#' @param y2.scale scale of y2-axis. Default value will be automatic calculated
#' @param plot.title Title of the graphic
#' @param text.size Size of text
#' @param text.x.top.angle Angle of text at top
#' @param text.x.bottom.angle Angle of text at bottom
#' @param ntick.x Minimum number of ticks on x-axis. Default = 10
#' @param highlight A vector of two locations (km), ex. c(4, 10) to be highlighted
#' @param a.fill Fill color of the highlight area
#' @param a.alpha Transparent ratio (alpha blending) of the highlight area
#' @param master.tbl Master table
#' @param verbose Print some messages if TRUE
#' @return a ggplot2 graphic
#' @export
plot_longprofile_animated <- function(
  river = NULL,
  from.km = -Inf,
  to.km = Inf,
  case.list = NULL,
  case.desc = case.list,
  sobek.project = NULL,
  param = 'discharge',
  lt.by = 'zustand',
  color.by = 'vgf',
  facet.by = 'hwe',
  facet.scale = 'fixed',
  compare.by = 'zustand',
  group.by = compare.by,
  color.name = 'Farbe',
  lt.name = 'Linienart',
  delta = FALSE,
  reserve.x = FALSE,
  x.lab = 'Lage (KM)',
  y.lab = ifelse(param == 'discharge',
                 'Abfluss (m³/s)', 'Wasserstand (m+NHN)'),
  y2.scale = NULL,
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
  master.tbl = NULL,
  verbose = TRUE
){
  stopifnot(is.numeric(from.km) & is.numeric(to.km))
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
        stop("Combination of compare.by and group.by does not have the same length as case.list")
      }
    }
  }
  if (is.null(plot.title)){
    plot.title <- paste('Längsschnitt ',
                        str_extract(y.lab, 'Abfluss|Wasserstand'),
                        ' entlang ', river, ' von KM ', from.km,
                        ' bis KM ', to.km, sep = ''
    )
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
  id_tbl <- get_segment_id_tbl(
    river = river,
    from.km = from.km,
    to.km = to.km,
    case.list = case.list,
    case.desc = case.desc,
    master.tbl = master.tbl
  )
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
  data_tbl <- merge(data_tbl, case_tbl, by = 'case', sort = FALSE)
  data_tbl[, besonderheit := gsub('DRV_', '', besonderheit)]
  b_tick <- data_tbl[case == case.list[[1]] &
                       nchar(besonderheit) > 0,
                     c("km", "besonderheit")
                     ]
  
  x_min <- data_tbl[, min(km, na.rm = TRUE)]
  x_max <- data_tbl[, max(km, na.rm = TRUE)]
  y1_min <- data_tbl[, min(scheitel, na.rm = TRUE)]
  y1_max <- data_tbl[, max(scheitel, na.rm = TRUE)]
  # y2_max <- y1_max/y2.scale
  # y1_pretty <- pretty(y1_min:y1_max, 5, 5)
  # y2_shift <- y1_pretty[1]
  # if(!is.null(y2.scale)){
  #   y2_min <- y2_shift/y2.scale
  # } else{
  #   y2_min <- NULL
  # }
  
  # rounding to integer or to multiple of ten
  # if (y2_max - y2_min > 10) {
  #   y2_min <- round(y2_min, -1)
  # } else {
  #   y2_min <- floor(y2_min)
  # }
  x_pretty <- pretty(x_min:x_max, ntick.x, ntick.x)
  #----delta == TRUE----
  if (isTRUE(delta)){
    data_tbl[, group := seq_len(.N), by = get(compare.by)]
    y2_name <- paste('Delta', str_to_sentence(compare.by),
                     ifelse(param == 'discharge', '(m³)', '(m)')
    )
    
    if (compare.by != group.by){
      y2_name <- paste('Delta', str_to_sentence(compare.by),
                       'nach', str_to_sentence(group.by), 'gruppiert',
                       ifelse(param == 'discharge', '(m³)', '(m)')
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
    y2_min <- min(data_tbl$delta, na.rm = TRUE)
    y2_max <- max(data_tbl$delta, na.rm = TRUE)
    if (y2_max - y2_min > 10) {
      y2_min <- round(y2_min,-1)
    } else {
      y2_min <- floor(y2_min)
    }
    if (is.null(y2.scale)){
      y2_length <- y2_max - y2_min
      y1_length <- y1_max - y1_min
      y2.scale <- round(y1_length/y2_length, 3)
      if (isTRUE(verbose)) print(paste('tried with y2.scale =', y2.scale))
    } else{
      y2_max <- y1_max/y2.scale
    }
    
    y1_pretty <- pretty(y1_min:y1_max, 5, 5)
    if (y1_pretty[5] < y1_max){
      y1_max <- ceiling(y1_max)
      y1_pretty <- pretty(y1_min:y1_max, 5, 5)
    }
    y2_shift <- floor(y1_pretty[1] - y2_min*y2.scale)
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
    #----animation----
  # labs(title = 'Time: {frame_time}', x = x.lab, y = y.lab) +
    # transitCion_states(km) +
    # ease_aes('linear')
  
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
    y2_pretty <- (y1_pretty - y2_shift)/y2.scale
    g <- g + geom_line(
      data = data_tbl,
      aes(
        y = delta * y2.scale + y2_shift,
        # shape = !!ensym(compare.by),
        color = delta_color,
        linetype = !!ensym(compare.by)
      ),
      size = 1
    ) +
      scale_y_continuous(
        breaks = y1_pretty,
        sec.axis =
          sec_axis(
            trans = ~ . * 1 / y2.scale - y2_shift / y2.scale,
            breaks = y2_pretty,
            labels = round(y2_pretty, 2),
            name = y2_name
          )
      )
  }
  #----adding highlight area and facet----
  if (!is.null(highlight)){
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
  g + transition_filter(transition_length = 1, 
                        filter_length = 0.2,
                        km > from.km, km < to.km,
                        wrap = TRUE, 
                        keep = FALSE)
  if (!is.null(facet.by)){
    g <- g + facet_wrap(~ get(facet.by), scales = facet.scale)
  }
  
  return(g)
}


