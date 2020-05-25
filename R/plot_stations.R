#'@export
plot_station_elbe <- function(
  dta = elbe_fav,
  limits = c(0, 600),
  txt_size = 16,
  p_size = 3,
  m_size = 3,
  p_shape = 18,
  hwr_shape = 16,
  m_shape = 24,
  p_color = 'black',
  hwr_color = 'black',
  d_color = 'red',
  m_color = 'blue',
  x_reverse = FALSE,
  x_breaks = NULL,
  drv_ln = 1.8,
  drv_txt = drv_ln - 0.1,
  hwr_pts = 1.8,
  hwr_txt = hwr_pts - 0.1,
  p_pts = 1.15,
  p_txt = p_pts + 0.1,
  m_pts = 1.15,
  m_txt = m_pts + 0.1
) {
  x_reverse <- isTRUE(x_reverse)
  if (is.null(limits)) limits <- range(dta$km)
  if (is.null(x_breaks)) x_breaks <- pretty(limits, 10, 10)
  y_limits <- c(drv_ln, drv_txt, hwr_pts, hwr_txt, p_pts, p_txt, m_pts, m_txt)
  # filtering data
  # FP - Flusspolder (DRV)
  drv_dta <- dta[type == "d"]
  drv_dta[, x_end := max(km, na.rm = TRUE), by = label]
  drv_dta <- drv_dta[km != x_end]
  drv_chk <- ifelse(nrow(drv_dta) > 0, TRUE, FALSE)
  # HWR
  hwr_dta <- dta[type == "gs" & !is.na(akz)]
  hwr_chk <- ifelse(nrow(hwr_dta) > 0, TRUE, FALSE)
  # Zufluss
  m_dta <- dta[type == "m"]
  m_chk <- ifelse(nrow(m_dta) > 0, TRUE, FALSE)
  # Pegel
  p_dta <- dta[type == "p"]
  p_chk <- ifelse(nrow(p_dta) > 0, TRUE, FALSE)
  g <- ggplot(
    data = dta,
    mapping = aes(x = km)
  ) +
    theme_bw(base_size = txt_size) +
    theme(
      axis.text = element_blank(),
      panel.grid = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "none"
    )
  if (x_reverse) {
    g <- g + scale_x_reverse(limits = limits, breaks = x_breaks)
  } else {
    g <- g + scale_x_continuous(limits = limits, breaks = x_breaks)
  }
  if (hwr_chk) {
    g <- g +
      # adding Polder
      geom_point(aes(y = hwr_pts, shape = 'Polder'),
                 color = hwr_color,
                 data = hwr_dta,
                 size = 3) +
      geom_text(aes(y = hwr_txt, label = akz),
                data = hwr_dta,
                color = hwr_color,
                check_overlap = TRUE,
                vjust = 0.5, hjust = 0.5
      )
  }
  if (drv_chk) {
    g <- g +
      # add DRV
      geom_segment(aes(y = drv_ln, yend = drv_ln, xend = x_end),
                   color = d_color,
                   linetype = "solid",
                   data = drv_dta,
                   size = 3
      ) +
      geom_text(aes(x = (km + x_end) / 2, y = drv_txt, label = akz),
                data = drv_dta, check_overlap = TRUE,
                color = d_color,
                # angle = 90,
                vjust = 0.5,
                hjust = 0.5
      )
  }
  if (p_chk) {
    g <- g +
      # add pegel
      geom_point(aes(y = p_pts, shape = "Pegel"),
                 color = p_color,
                 data = p_dta,
                 size = p_size
      ) +
      geom_text(aes(y = p_txt, label = akz),
                color = p_color,
                angle = 90,
                data = p_dta[akz != 'BB'],
                vjust = 0.5, hjust = 0
      ) +
      geom_text(aes(y = p_txt, label = akz),
                color = p_color,
                angle = 90,
                data = p_dta[akz == 'BB'],
                vjust = 1, hjust = 0
      )
  }
  if (m_chk) {
    g <- g +
      # add zuflüsse
      geom_point(aes(y = m_pts, shape = "Muendung"),
                 color = m_color,
                 data = m_dta,
                 fill = m_color,
                 size = m_size
      ) +
      geom_text(aes(y = m_txt, label = akz),
                color = m_color,
                show.legend = FALSE,
                angle = 90,
                vjust = 0.5, hjust = 0,
                data = m_dta
      )
  }
  # using c(rbind(c1, c2)) is also good
  y_limits <- range(y_limits[c(drv_chk, drv_chk, hwr_chk, hwr_chk, p_chk, p_chk, m_chk, m_chk)])
  g <- g + scale_y_continuous(limits = y_limits, name = "")
  g + scale_shape_manual(
    breaks = c("Pegel", "Muendung", 'Polder'),
    values = c(Pegel = p_shape, Muendung = m_shape, Polder = hwr_shape)
  )
}


#' Plot graphic for stations
#'
#'
#' @export
plot_station <- function(
  dta,
  limits = NULL,
  txt_size = 18,
  p_size = 4,
  m_size = 3,
  hwr_size = 3,
  p_shape = 18,
  m_shape = 24,
  hwr_shape = 16,
  p_color = 'black',
  hwr_color = 'black',
  d_color = 'red',
  m_color = 'blue',
  x_reverse = FALSE,
  x_breaks = NULL,
  drv_ln = 1.8,
  drv_txt = drv_ln - 0.1,
  hwr_pts = 1.8,
  hwr_txt = hwr_pts - 0.1,
  p_pts = 1.15,
  p_txt = p_pts + 0.1,
  m_pts = 1.15,
  m_txt = m_pts + 0.1,
  m_txt_size = 4,
  p_txt_size = 4,
  d_txt_size = 5,
  hwr_txt_size = 5,
  p_angle = 90,
  hwr_angle = 0,
  drv_angle = 0,
  m_angle = 90,
  m_txt_repel = FALSE,
  mea_same = FALSE,
  p2move = NULL,
  m2move = NULL,
  hwr2move = NULL,
  dp = 5,
  dm = dp,
  dhwr = dp,
  x_expand = expansion(0.02)
) {
  x_reverse <- isTRUE(x_reverse)
  if (is.null(limits)) limits <- range(dta$km)
  if (is.null(x_breaks)) x_breaks <- pretty(limits, 10, 10)
  y_limits <- c(drv_ln, drv_txt, hwr_pts, hwr_txt, p_pts, p_txt, m_pts, m_txt)
  # filtering data
  # FP - Flusspolder (DRV)
  dta[, km_txt := km]
  drv_dta <- dta[type == "d"]
  drv_chk <- ifelse(nrow(drv_dta) > 0, TRUE, FALSE)
  if (drv_chk) {
    drv_dta[, x_end := max(km, na.rm = TRUE), by = label]
    drv_dta <- drv_dta[km != x_end]
  }
  # HWR
  hwr_dta <- dta[type == "gs" & !is.na(akz)]
  hwr_dta[akz %in% hwr2move, km_txt := km + dhwr]
  hwr_chk <- ifelse(nrow(hwr_dta) > 0, TRUE, FALSE)
  # Zufluss
  m_dta <- dta[type == "m"]
  m_dta[akz %in% m2move, km_txt := km + dm]
  m_chk <- ifelse(nrow(m_dta) > 0, TRUE, FALSE)
  # Pegel
  p_dta <- dta[type == "p"]
  p_dta[akz %in% p2move, km_txt := km + dp]
  p_chk <- ifelse(nrow(p_dta) > 0, TRUE, FALSE)
  g <- ggplot(
    data = dta,
    mapping = aes(x = km)
  ) +
    theme_bw(base_size = txt_size) +
    theme(
      axis.text = element_blank(),
      panel.grid = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "none"
    )
  if (x_reverse) {
    g <- g + scale_x_reverse(
      expand = x_expand,
      limits = sort(limits, decreasing = TRUE),
      breaks = sort(x_breaks, decreasing = TRUE)
      )
  } else {
    g <- g + scale_x_continuous(limits = limits, breaks = x_breaks, expand = x_expand)
  }
  # plot all measurements on same layer
  mea_chk <- FALSE
  if (mea_same) {
    mea_dta <- rbind(hwr_dta, drv_dta, fill = TRUE, use.names = TRUE)
    mea_dta[!is.na(x_end), km := (km + x_end) / 2]
    mea_dta[!is.na(x_end), km_txt := (km_txt + x_end) / 2]
    mea_chk <- nrow(mea_dta) > 0
    if (drv_chk) {
      g <- g +
        # add DRV
        geom_segment(aes(y = drv_ln, yend = drv_ln, xend = x_end),
                     color = d_color,
                     linetype = "solid",
                     data = drv_dta,
                     size = 3
        )
    }
    if (hwr_chk) {
      g <- g +
        # adding Polder
        geom_point(aes(y = hwr_pts, shape = 'Polder'),
                   color = hwr_color,
                   data = hwr_dta,
                   size = 3)
    }
    # add text
    if (mea_chk) {
      g <- g +
        geom_text(aes(x = km_txt, y = drv_txt,
                                     label = akz,
                                     color = type),
                  data = mea_dta,
                  show.legend = FALSE,
                  size = d_txt_size,
                  angle = drv_angle,
                  vjust = 0.5,
                  hjust = 0.5
        ) +
        scale_color_manual(
          values = c(d_color, hwr_color)
        )
    }
  } else {
    if (drv_chk) {
      g <- g +
        # add DRV
        geom_segment(aes(y = drv_ln, yend = drv_ln, xend = x_end),
                     color = d_color,
                     linetype = "solid",
                     data = drv_dta,
                     size = 3
        ) +
        geom_text(aes(x = (km_txt + x_end) / 2, y = drv_txt, label = akz),
                  data = drv_dta, check_overlap = TRUE,
                  size = d_txt_size,
                  color = d_color,
                  angle = drv_angle,
                  vjust = 0.5,
                  hjust = 0.5
        )
    }
    if (hwr_chk) {
      g <- g +
        # adding Polder
        geom_point(aes(y = hwr_pts, shape = 'Polder'),
                   color = hwr_color,
                   data = hwr_dta,
                   size = hwr_size) +
        geom_text(aes(x = km_txt, y = hwr_txt, label = akz),
                  data = hwr_dta,
                  color = hwr_color,
                  size = hwr_txt_size,
                  angle = hwr_angle,
                  check_overlap = TRUE,
                  vjust = 0, hjust = 0.5
        )
    }
  }
  if (p_chk) {
    p_hjust <- ifelse(p_angle == 90, 0, 0.5)
    g <- g +
      # add pegel
      geom_point(aes(y = p_pts, shape = "Pegel"),
                 color = hwr_color,
                 data = p_dta,
                 size = p_size
      ) +
      geom_text(aes(x = km_txt, y = p_txt, label = akz),
                color = hwr_color,
                angle = p_angle,
                size = p_txt_size,
                # segment.color = 'white',
                data = p_dta,
                vjust = 0.5, hjust = p_hjust
      )
  }
  if (m_chk) {
    g <- g +
      # add zuflüsse
      geom_point(aes(y = m_pts, shape = "Muendung"),
                 color = m_color,
                 data = m_dta,
                 fill = m_color,
                 size = m_size
      )
    if (m_txt_repel) {
      g <- g + ggrepel::geom_text_repel(aes(x = km_txt, y = m_txt, label = akz),
                color = m_color,
                show.legend = FALSE,
                size = m_txt_size,
				segment.color = 'white',
                angle = m_angle,
                vjust = 0.5, hjust = 0,
                data = m_dta
      )
    } else {
      g <- g + geom_text(aes(x = km_txt, y = m_txt, label = akz),
                      color = m_color,
                      show.legend = FALSE,
                      angle = m_angle,
                      size = m_txt_size,
                      vjust = 0.5, hjust = 0,
                      data = m_dta
      )
    }
  }
  y_limits <- range(y_limits[c(drv_chk, drv_chk, hwr_chk, hwr_chk, p_chk, p_chk, m_chk, m_chk)])
  g <- g + scale_y_continuous(limits = y_limits, name = "") +
    scale_shape_manual(
    breaks = c("Pegel", "Muendung", 'Polder'),
    values = c(Pegel = p_shape, Muendung = m_shape, Polder = hwr_shape)
  )
  g
}


format_elbe_long <- function(g, txt_size = 16, mpos = 500, fix_y = TRUE) {
  if (fix_y) {
    g <- g +
      scale_y_continuous(
        breaks = pretty(0:6000, 10, 10),
        limits = c(0, 6000)
      )
  }
  g <- g +
    geom_vline(aes(xintercept = km),
               linetype = 'dashed',
               data = elbe_fav[type == 'm']) +
    geom_text(aes(x = km, y = mpos, label = label, color = NA, linetype = NA),
              colour = 'blue',
              hjust = 0, vjust = 0,
              size = 6,
              angle = 90,
              data = elbe_fav[type == 'm']) +
    theme(
      plot.title = element_blank(),
      legend.key.width  = unit(1.2, 'cm'),
      legend.box.just = 'center',
      legend.margin = margin(l = 4),
      legend.position = 'bottom',
      legend.title = element_text(size = txt_size + 2),
      legend.text = element_text(size = txt_size),
      axis.text = element_text(size = txt_size),
      axis.text.x.bottom = element_text(size = txt_size),
      axis.title.x.top = element_blank(),
      axis.text.x.top =  element_blank(),
      axis.ticks.x.top = element_blank()
    ) +
    guides(
      colour = guide_legend(nrow = 2, title = 'Zielpegel'),
      linetype = guide_legend(nrow = 1, title = 'Skalierung'),
      shape = guide_legend(nrow = 2, title = 'Statistik')
    ) +
    scale_color_manual(
      values = c(exl_std[c(9, 6, 7, 3, 2, 10)]),
      labels = zp_label,
      name = 'Zielpegel'
    ) +
    scale_linetype_manual(
      values = c('dotdash', 'dashed', 'solid'),
      labels = vgf_label,
      name = 'Skalierung'
    ) +
    # add Pegel
    geom_point(
      mapping = aes(y = 0),
      color = 'black',
      shape = 'diamond',
      size = 3,
      show.legend = FALSE,
      data = elbe_pegel
    ) +
    geom_text(
      mapping = aes(y = 200, label = akz),
      color = 'black',
      size = 5,
      show.legend = FALSE,
      data = elbe_pegel
    )
  g
}


#' Reformat long profile graphic to match typical figure 3.10
#'
#'
#' @param g Long profile graphic, result from plot_long_profile
#' @param txt_size Text size
#' @param mpos Horizontal position of the Mündungen
#' @param y_lims Limits of y-axis
#' @param y_breaks Breaks of y_axis
#' @param x_lims Limits of x-axis
#' @param x_breaks Breaks of x_axis
#' @return ggplot graphic
#' @export
add_fav <- function(g,
                          fav = rhein_fav,
                          hqs = NULL,
                          txt_size = 20,
                          mpos = -100,
                          m2move = "Mosel",
                          mtxt_size = 7,
                          dmove = 0.012,
                          ptxt = NULL,
                          ppos = NULL,
                          pvjust = 1,
                          p2move = "EM",
                          pmove = dmove,
                          y_lims = NULL,
                          y_breaks = NULL,
                          y_expand = c(0.02, 0.02),
                          x_lims = NULL,
                          x_breaks = NULL,
                          x_reverse = FALSE,
                          shape.nrow = 1,
                          shape.size = 3,
                          lt.nrow = 2,
                          color.nrow = lt.nrow,
                    lt_breaks = NULL
                          ) {
  if (!is.null(hqs)) {
    hqs <- melt(hqs[, .SD, .SDcols = c("km", grep("^HQ", colnames(hqs), value = TRUE))],
                id.vars = "km")
  }
  fav[, compare__by := NA][, group__by := NA]
  m2move_chk <- nrow(fav[type == "m" & akz %in% m2move]) > 0
  p2move_chk <- nrow(fav[type == "p" & akz %in% p2move]) > 0
  fav[, km_m := km]
  new_y <- any(!is.null(y_lims), !is.null(y_breaks))
  new_x <- any(!is.null(x_lims), !is.null(x_breaks))
  gbld <- ggplot_build(g)
  color_lbl <- gbld$plot$labels$colour
  lt_lbl <- gbld$plot$labels$linetype
  if (is.null(lt_breaks)) lt_breaks <- gbld$plot$scales$get_scales("linetype")$get_breaks()
  if (is.null(y_lims)) y_lims <- range(gbld$layout$panel_params[[1]]$y.range)
  if (!is.null(hqs)) y_lims <- range(y_lims, hqs[, max(value, na.rm = TRUE)])
  if (is.null(y_breaks)) y_breaks <- pretty(y_lims, 7, 7)
  if (is.null(x_lims)) x_lims <- gbld$layout$panel_params[[1]]$x.range
  if (is.null(x_breaks)) x_breaks <- gbld$layout$panel_params[[1]]$x$breaks
  if (is.null(ppos)) ppos <- max(y_lims)
  if (is.null(ptxt)) ptxt <- ppos - 300
  if (x_reverse) {
    g <- g + scale_x_reverse(
      breaks = x_breaks,
      limits = x_lims
    )
  }
  g <- g +
    geom_vline(aes(xintercept = km),
               linetype = 'dashed',
               data = fav[type == 'm'])
  if (m2move_chk) {
    x_range <- range(x_breaks, na.rm = TRUE)
    x_length <- abs(x_lims[1] - x_lims[2])
    d_txt <- dmove * x_length
    g <- g +
      geom_text(aes(x = km, y = mpos, label = label, color = NA, linetype = NA),
                       colour = 'blue',
                       hjust = 0, vjust = 0,
                       size = mtxt_size,
                       angle = 90,
                       data = fav[type == 'm' & !akz %in% m2move]) +
      geom_text(aes(x = km + d_txt, y = mpos, label = label, color = NA, linetype = NA),
                colour = 'blue',
                hjust = 0, vjust = 0,
                size = mtxt_size,
                angle = 90,
                data = fav[type == 'm' & akz %in% m2move])

  } else {
    g <- g +
      geom_text(aes(x = km, y = mpos, label = label, color = NA, linetype = NA),
                colour = 'blue',
                hjust = 0, vjust = 0,
                size = mtxt_size,
                angle = 90,
                data = fav[type == 'm'])
  }
  g <- g +
    # add Pegel
    geom_point(
      mapping = aes(y = ppos),
      color = blind_colors[5],
      shape = 'diamond',
      vjust = pvjust,
      size = 4,
      show.legend = FALSE,
      data = fav[type == "p"]
    )
  if (p2move_chk) {
    x_range <- range(x_breaks, na.rm = TRUE)
    x_length <- abs(x_lims[1] - x_lims[2])
    d_txt <- pmove * x_length
    g <- g +
      geom_text(
        mapping = aes(y = ptxt, label = akz),
        color = blind_colors[5],
        vjust = 1,
        size = mtxt_size,
        show.legend = FALSE,
        data = fav[type == "p" & !akz %in% p2move]
      ) +
      geom_text(
        mapping = aes(x = km + d_txt, y = ptxt, label = akz),
        color = blind_colors[5],
        vjust = 1,
        size = mtxt_size,
        show.legend = FALSE,
        data = fav[type == "p" & akz %in% p2move]
      )
  } else {
    g <- g +
      geom_text(
        mapping = aes(y = ptxt, label = akz),
        color = blind_colors[5],
        vjust = 1,
        size = mtxt_size,
        show.legend = FALSE,
        data = fav[type == "p"]
      )
  }
  g <- g +
      theme(
        plot.title = element_blank(),
        legend.key.width  = unit(1.2, 'cm'),
        legend.box.just = 'center',
        legend.margin = margin(l = 4),
        legend.position = 'bottom',
        legend.title = element_text(size = txt_size + 2),
        legend.text = element_text(size = txt_size),
        axis.text = element_text(size = txt_size),
        axis.title = element_text(size = txt_size),
        axis.text.x.bottom = element_text(size = txt_size),
        axis.title.x.top = element_blank(),
        axis.text.x.top =  element_blank(),
        axis.ticks.x.top = element_blank()
      ) +
      scale_y_continuous(limits = y_lims,
                         breaks = y_breaks,
                         expand = y_expand) +
      scale_color_manual(
        # values = c(exl_std[c(9, 6, 7, 3, 2, 10)]),
        breaks = gbld$plot$scales$get_scales("colour")$get_breaks(),
        values = gbld$plot$scales$get_scales("colour")$palette.cache,
        name = color_lbl,
        labels = zp_label
      ) +
      scale_linetype_manual(
        values = gbld$plot$scales$get_scales("linetype")$palette.cache,
        breaks = lt_breaks,
        name = lt_lbl,
        labels = vgf_label
      ) +
      guides(
        colour = guide_legend(nrow = color.nrow),
        linetype = guide_legend(nrow = lt.nrow),
        shape = guide_legend(nrow = shape.nrow, title = 'Statistik')
      )
  if (!is.null(hqs)) {
    g <- g +
      geom_point(
        mapping = aes(y = value, shape = variable),
        color = "grey40",
        fill = "grey40",
        size = shape.size,
        data = hqs) +
      scale_shape_manual(values = hqs_shape)
  }
  g
}


#' @export
plot_rhein_long <- function(
  case.list = NULL,
  case.desc = case.list,
  compare.by = "zustand",
  group.by = c("vgf", "zielpegel", "hwe"),
  fav = rhein_fav,
  param = "discharge",
  color.by = "hwe",
  lt.by = "vgf",
  color.name = color.by,
  lt.name = lt.by,
  color = col_all,
  lt = vgf_lt,
  color_breaks = ggplot2::waiver(),
  lt_breaks = ggplot2::waiver(),
  delta = FALSE,
  lubw_long = NULL,
  river = NULL,
  sobek.project = so_prj,
  do.par = TRUE,
  txt_size = 22,
  line_size = 1.3,
  ts.trim.left = NULL,
  input.data = NULL,
  x_lims = c(150, 860),
  rm_legend = FALSE,
  x_breaks = pretty(x_lims, 20, 20),
  x_expand = expansion(0.02),
  y_expand = expansion(0.02),
  y_lims = NULL,
  skl_title = NULL,
  hist_title = NULL,
  y_breaks = ggplot2::waiver(),
  heights = c(1.2, 3, 9),
  skl_only = FALSE,
  lst.return = FALSE,
  master.tbl = rhein_tbl
) {
  eval({
    color_breaks
    lt_breaks
    x_expand
    y_breaks
    })
  if (is.null(input.data)) {
    dta <- get_segment_data(
      case.list = case.list,
      case.desc = case.desc,
      sobek.project = sobek.project,
      param = param,
      ts.trim.left = ts.trim.left,
      get.max = TRUE,
      do.par = do.par,
      master.tbl = master.tbl
    )
  } else {
    dta <- copy(input.data)
  }
  if (!is.null(case.list) && !is.null(case.desc)) {
    dta <- dta[, c("km", "scheitel", "case")]
    case_tbl <- parse_case(case.desc, case.list)
    dta <- merge(dta, case_tbl, by = "case")
  }
  # dta[zustand == "Planzustand", zustand := "Alle Maßnahmen"]
  # dta[, vgf := stri_replace_all_fixed(vgf, "Selten", "Selten 2")]
  # dta[, vgf := stri_replace_all_fixed(vgf, "Mittel", "Selten 1")]
  cdesc_lst <- dta[, unique(paste(zustand, zielpegel, hwe, vgf, sep = "_"))]
  # processing unstability of the model by HW1988, ZP0
  dta[hwe == "HW1988" & zielpegel == "ZP0" & km %between% c(838.8, 865.5),
      scheitel := NA]
  dta[hwe == "HW1988" & zielpegel == "ZP0" & km > 838,
      scheitel := approx(.I, scheitel, .I)$y, by = case]
  if (!is.null(lubw_long)) {
    lubw_long[, case_desc := paste(zustand, zielpegel, hwe, vgf)]
    lubw_long[zielpegel == "Köln", zielpegel := "ZPK"]
    lubw_long[zielpegel == "Worms", zielpegel := "ZPW"]
    dta <- rbind(
      dta,
      lubw_long[paste(zustand, zielpegel, hwe, vgf, sep = "_") %in% cdesc_lst]
    )
  }
  if (param == "waterlevel") {
    # graphic for favorites
    rhein_tit <- plot_station(dta = fav,
                              mea_same = TRUE,
                              drv_txt = 1.6,
                              hwr_txt = 1.6,
                              p2move = c("RU", "WE", "MZ"), dp = 8,
                              hwr2move = c("6", "10"), dhwr = -5,
                              x_expand = x_expand,
                              m2move = c("Sieg", "Kinzig"), dm = 4,
                              limits = x_lims, x_breaks = x_breaks) +
      theme(
        plot.margin = margin(t = 0.2, b = 0.2, r = 0.2, unit = "cm")
      )

    # graphic for skalierung
    delta_skl <- cal_delta(dta[zielpegel != "ZP0"], compare.by, group.by)
    legend_pos <- ifelse(rm_legend, "none", "bottom")
    y_lbl <- ifelse(rm_legend, "Scheitelreduktion [m]", "")
    txt_format <- element_text(family = "Times New Roman", size = txt_size)
    g_skl <- ggplot(data = delta_skl[zustand == "Bezugszustand" &
                                       km %between% x_lims],
                    aes(x = km)) +
      geom_line(aes(
        x = km,
        y = delta,
        color = zielpegel,
        linetype = vgf
      ),
      size = line_size) +
      scale_x_continuous(name = "Rhein-km",
                         limits = x_lims,
                         expand = x_expand,
                         breaks = x_breaks) +
      scale_y_continuous(name = y_lbl, limits = y_lims, breaks = y_breaks,
                         expand = y_expand) +
      theme_bw(base_size = txt_size) +
      theme(
        legend.position = legend_pos,
        title = txt_format,
        text = txt_format,
        plot.title = element_text(size = txt_size, margin = margin()),
        plot.margin = margin(r = 0.2, b = 0.2, unit = "cm"),
        legend.text = txt_format,
        legend.title =  txt_format,
        panel.grid.major = element_line(color = "grey60", size = 0.3),
        panel.grid.minor = element_line(color = "grey60", size = 0.2),
        panel.spacing = unit(0.75, "cm"),
        legend.key.width = unit(1.2, "cm")
        ) +
      scale_color_manual(name = "Zielpegel",
                         breaks = color_breaks,
                         values = color) +
      scale_linetype_manual(name = "Skalierung",
                         breaks = lt_breaks,
                         labels = vgf_label,
                         values = lt) +
      ggtitle(skl_title) +
      facet_grid(rows = vars(hwe))
    if (rm_legend) {
      g_skl <- g_skl + theme(
        strip.background = element_blank(),
        strip.text = element_blank()
      )
    }
    # graphic for hist
    g_skl_bld <- ggplot_build(g_skl)
    y_range <- range(g_skl_bld$layout$panel_params[[1]]$y.range)
    y_breaks <- g_skl_bld$layout$panel_params[[1]]$y$breaks
    dta_his <- dta[zielpegel == "ZP0"]
    delta_his <- cal_delta(dta_his, "zustand", "hwe")
    if (rm_legend) legend_pos <- "none" else legend_pos <- c(0.5, 0.2)

    g_his <- ggplot(data = delta_his[zustand == "Bezugszustand" &
                                       km %between% x_lims],
                    aes(x = km)) +
      geom_line(aes(
        x = km,
        y = delta,
        color = group__by
      ),
      size = line_size) +
      scale_x_continuous(name = "",
                         limits = x_lims,
                         expand = x_expand,
                         breaks = x_breaks) +
      scale_y_continuous(limits = y_range, breaks = y_breaks, expand = y_expand) +
      theme_bw(base_size = txt_size) +
      theme(legend.position = legend_pos,
            title = txt_format,
            text = txt_format,
            plot.title = element_text(size = txt_size, margin = margin()),
            legend.text = txt_format,
            legend.title =  txt_format,
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.major = element_line(color = "grey60", size = 0.3),
            panel.grid.minor = element_line(color = "grey60", size = 0.2),
            plot.margin = margin(),
            axis.title = element_blank(),
            legend.direction = "horizontal",
            legend.key.height = unit(0.5, "cm"),
            legend.key.width = unit(1, "cm")) +
      ggtitle(hist_title) +
      scale_color_manual(name = "",
                         breaks = color_breaks,
                         values = color) +
      guides(colour = guide_legend(nrow = 1))
    # combining graphics
    if (skl_only) {
      g <- g_skl
    } else {
      g <- patchwork::wrap_plots(rhein_tit, g_his, g_skl,
                                 ncol = 1, heights = heights)
    }

  } else {
    # Q graphics ----------------------------------------------------------------
    if (delta) {
      dta[, group__by := do.call(paste, .SD), .SDcols = group.by]
      dta[, compare__by := do.call(paste, .SD), .SDcols = compare.by]
      # grp_vars <- dta[, unique(group__by)]
      # cmp_vars <- dta[, unique(compare__by)]
      g <- ggplot(data = dta, aes(x = km)) +
        geom_line(aes(
          x = km,
          y = delta,
          color = group__by,
          linetype = compare__by
        ),
        size = line_size)
    } else {
      dta[, group__by := do.call(paste, .SD), .SDcols = color.by]
      dta[, compare__by := do.call(paste, .SD), .SDcols = lt.by]
      g <- ggplot(data = dta, aes(x = km)) +
        geom_line(aes(
          x = km,
          y = scheitel,
          color = group__by,
          linetype = compare__by
        ),
        size = line_size)
    }
    g <- g +
      scale_x_continuous(name = "Rhein-km",
                         limits = x_lims,
                         expand = x_expand,
                         breaks = x_breaks) +
      theme_bw(base_size = txt_size) +
      theme(
        legend.position = "bottom",
        axis.text = element_text(size = txt_size),
        axis.title = element_text(size = txt_size),
        legend.text = element_text(size = txt_size),
        # panel.grid.major = element_line(color = "grey60", size = 0.3),
        # panel.grid.minor = element_line(color = "grey40", size = 0.15),
        legend.title =  element_text(size = txt_size),
        legend.key.width = unit(1.2, "cm")
      ) +
      scale_color_manual(name = str_to_sentence(color.name),
                         breaks = color_breaks,
                         values = color) +
      scale_linetype_manual(name = str_to_sentence(lt.name),
                            breaks = lt_breaks,
                            values = lt)
    g$labels$colour <- str_to_sentence(color.name)
    g$labels$linetype <- str_to_sentence(lt.name)
    g$labels$y <- ifelse(param == "discharge",
                         "Scheitelablfuss [m³/s]", "Wasserstand [m³/s]")
  }
  if (lst.return && param == "waterlevel") {
    return(list(g_his, g_skl))
  } else {
    return(g)
  }

}


#' @export
plot_rhein_dw <- function(
  zt = "Alle",
  input.data = NULL,
  lubw_long = NULL,
  ...
) {
  zt <- paste0("Bezug|", zt)
  if (!is.null(input.data)) input.data <- input.data[grepl(zt, zustand)]
  if (!is.null(lubw_long)) lubw_long <- lubw_long[grepl(zt, zustand)]
  g1 <- plot_rhein_long(
                        x_lims = c(160, 334),
                        x_breaks = pretty(160:334, 3),
                        rm_legend = TRUE,
                        param = "waterlevel",
                        hist_title = "  ",
                        y_lims = c(-2, 1.7),
                        y_breaks = pretty(c(-2, 1.7), 5),
                        skl_title = "  ",
                        input.data = input.data,
                        lubw_long = lubw_long,
                        ...
  )
  g2 <- plot_rhein_long(
    x_lims = c(334.1, 860),
    x_breaks = pretty(c(334.1, 860), 10),
    y_lims = c(-0.75, 0.15),
    y_breaks = pretty(c(-0.75, 0.15), 5),
    color_breaks = names(col_all),
    rm_legend = FALSE,
    param = "waterlevel",
    hist_title = "Historische Hochwasser (nicht skaliert)",
    skl_title = "Modellhochwasser (skaliert)",
    input.data = input.data,
    lubw_long = lubw_long,
    ...
  )
  g <- patchwork::wrap_plots(g1, g2, ncol = 2, widths = c(1, 4))

  return(g)
}


cal_delta <- function(
  tbl, cmp, grp
) {
  tbl[, group__by := do.call(paste, .SD), .SDcols = grp]
  tbl[, compare__by := do.call(paste, .SD), .SDcols = cmp]
  grp_vars <- tbl[, unique(group__by)]
  cmp_vars <- tbl[, unique(compare__by)]
  km_4_delta <- tbl[!is.na(scheitel), unique(km)]
  grp_chk <- !identical(paste(cmp, collapse = " "),
                        paste(grp, collapse = " "))
  if (grp_chk) {
    dta_delta <-
      dcast.data.table(tbl[!is.na(scheitel) & km %in% km_4_delta],
                       km ~ compare__by + group__by,
                       value.var = 'scheitel')
    for (i in grp_vars) {
      col_1 <- paste(cmp_vars[1], i, sep = '_')
      col_2 <- paste(cmp_vars[2], i, sep = '_')
      dta_delta[, eval(i) := get(col_2) - get(col_1)]
      dta_delta[, eval(col_1) := NULL]
      dta_delta[, eval(col_2) := NULL]
    }
    dta_delta <- melt(dta_delta, id.vars = 'km',
                      variable.name = 'group__by',
                      value.name = 'delta',
                      sort = FALSE)
    tbl <- merge(tbl, dta_delta, by = c('km', 'group__by'),
                 all = TRUE,
                 sort = FALSE)
  } else{
    dta_delta <-
      dcast(tbl[!is.na(scheitel) & km %in% km_4_delta],
            km  ~ compare__by,
            value.var = 'scheitel'
      )
    col_1 <- cmp_vars[1]
    col_2 <- cmp_vars[2]
    dta_delta[, delta := get(col_1) - get(col_2)]
    dta_delta[, eval(col_1) := NULL]
    dta_delta[, eval(col_2) := NULL]
    tbl <-
      merge(tbl, dta_delta, by.x = c("km", cmp), by.y = c("km", "compare__by"),
            all = TRUE, sort = FALSE)
  }
  tbl[, delta := round(delta, 3)]
  tbl
}


# hist_color_donau <- c( # falsch
#   HW1999 = "#4477AA",
#   HW2005 = "#228833",
#   HW2013 = "#EE6677",
#   HW2002 = "grey60",
#   HW2011 = "#CCBB44")

hist_color_donau <- c(
  HW2011 = "#4477AA",
  HW2005 = "#228833",
  HW2013 = "#EE6677",
  HW2002 = "grey60",
  HW1999 = "#CCBB44")

#' @export
plot_long_inn <- function(
  color.by = 'hwe',
  lt.by = 'vgf',
  color.name = '',
  lt.name = color.name,
  txt.size = 20,
  x.lab = 'Inn-km',
  y.lab = 'Abfluss [m³/s]',
  y_lims = c(0, 10000),
  x_lims = c(0, 212),
  lt = 1:2,
  mpos = 0,
  m_txt_size = 8,
  m2move = "Salzach",
  dm2move = 5,
  p2move = "WABG",
  dp2move = dm2move,
  x_expand = expansion(0.02),
  y_expand = expansion(0.05),
  ppos = 7500,
  ptxt = ppos - 200,
  p_txt_size = 7,
  color = hist_color_donau,
  heights = c(1.1, 6),
  facet.by = NULL,
  hqs = hqs_inn[, -c('Pegel', 'BHQ')],
  fav = NULL,
  dta = data_tbl_his[X1 == 'bezug']
) {
  eval({x_expand
    y_expand})
  if (!is.null(facet.by)) {
    facet_lst <- data_tbl[, unique(get(facet.by))]
    n_facet <- length(facet_lst)
    hqs_lst <- list()
    for (i in seq_along(facet_lst)) {
      hqs_lst[[i]] <- copy(hqs)
      hqs_lst[[i]][, eval(facet.by) := facet_lst[[i]]]
    }
    hqs <- rbindlist(hqs_lst)
  }
  hqs_cols <- c(grep("HQ", colnames(hqs), value = TRUE), "Fkm", facet.by)
  hqs <- melt(hqs[, .SD, .SDcols = hqs_cols], id.vars = c('Fkm', facet.by),
              variable.name = 'HQ-Statistik',
              value.name = 'value')
  g <- ggplot(
    dta,
    aes(Fkm, value)
  ) +
    geom_line(aes(color = get(color.by),
                  linetype = get(lt.by)),
              size = 1.3) +
    geom_point(
      aes(shape = `HQ-Statistik`),
      color = 'black',
      size = 4,
      data = hqs)

  g <- g +
    theme_bw(base_size = txt.size) +
    theme(
      axis.title = element_text(size = txt.size),
      axis.text = element_text(size = txt.size),
      legend.text = element_text(size = txt.size),
      legend.title = element_text(size = txt.size),
      legend.position = 'bottom',
      panel.grid.major = element_line(color = "grey60", size = 0.3),
      legend.direction = 'horizontal',
      legend.key.width = unit(1.5, 'cm'),
      legend.key.height = unit(0.75, 'cm'),
      legend.box.spacing = unit(0.5, "cm")
    ) +
    labs(x = x.lab, y = y.lab) +
    scale_linetype_manual(
      name = lt.name,
      values = lt
    ) +
    scale_color_manual(
      name = color.name,
      values = color
    ) +
    scale_x_reverse(
      breaks = pretty(x_lims, 10, 10),
      limits = sort(x_lims, TRUE),
      expand = x_expand,
    ) +
    scale_y_continuous(breaks = pretty(y_lims, 7, 7),
                       expand = y_expand,
                       limits = y_lims)
  if (!is.null(facet.by)) g <- g + facet_grid(rows = vars(get(facet.by)))
  if (!is.null(fav)) {
    fav[, km_m := km]
    fav[, km_p := km]
    if (!is.null(m2move)) {
      fav[akz %in% m2move, km_m := km - dm2move]
    }
    if (!is.null(p2move)) {
      fav[akz %in% p2move, km_p := km - dp2move]
    }
    g <- g +
      geom_point(aes(x = km, y = -Inf),
                 data = fav[type == "m"],
                 size = 5,
                 color = "#4477AA", fill = "#4477AA",
                 shape = 24) +
      geom_text(aes(x = km_m, y = mpos, label = label),
                size = m_txt_size,
                color = "#4477AA",
                data = fav[type == "m"]) +
      geom_point(aes(x = km, y = ppos),
                 data = fav[type == "p"],
                 color = "#EE6677", fill = "#EE6677",
                 size = 5,
                 shape = "diamond") +
      geom_text(aes(x = km_p, y = ptxt, label = akz),
                size = p_txt_size,
                data = fav[type == "p"]) +
      geom_point(aes(x = km, y = ppos),
                 data = fav[type == "gs"],
                 size = 5,
                 shape = "circle") +
      geom_text(aes(x = km, y = ptxt, label = akz),
                size = p_txt_size,
                data = fav[type == "gs"])

  } else {
    p_inn_q <- plot_station(inn_fav[dar == 1],
                            txt_size = 20,
                            hwr_txt = 1.5, drv_txt = 1.5,
                            x_breaks = seq.int(220, 0, -20),
                            p_size =  4,
                            hwr_size = 4,
                            p_txt_size = 6,
                            m_txt_size = 6,
                            hwr_txt_size = 7,
                            x_expand = x_expand,
                            m2move = c("Mangfall"),
                            dm = -2,
                            x_reverse = TRUE,
                            limits = x_lims
    )
    g <- patchwork::wrap_plots(p_inn_q, g, ncol = 1, heights = heights)
  }
  g <- g + guides(
    shape = guide_legend(keywidth = unit(0.4, "cm"))
  )
  g
}


#' @export
plot_long_delta <- function(
  dta,
  case.list,
  case.desc = donau_case_tbl[case_name %in% case.list, case_desc],
  compare.by = "zustand",
  group.by = compare.by,
  color.by = 'hwe',
  lt.by = 'hwe',
  facet.by = NULL,
  facet.scale = "fixed",
  x.lim = c(2600, 2200),
  x_breaks = pretty(2600:2200, 10, 10),
  y.lim = c(-0.95, 0.15),
  color = hist_donau_color,
  x_expand = expansion(0.02),
  y_expand = expansion(0.01),
  txt.size = 20,
  x.lab = 'Donau-km',
  y.lab = 'Scheitelreduktion [m]',
  title = "",
  reverse.x = FALSE
) {
  case_tbl <- parse_case(case.desc, orig.name = case.list)
  case_tbl[, compare__by := do.call(paste, .SD), .SDcols = compare.by]
  case_tbl[, group__by := do.call(paste, .SD), .SDcols = group.by]
  grp_vars <- sort(unique(case_tbl[, group__by]))
  cmp_vars <- sort(unique(case_tbl[, compare__by]))
  stopifnot(length(cmp_vars) == 2)
  cmp_sort_chk <- isTRUE(grep('bezug', cmp_vars, ignore.case = TRUE) == 1)
  if (cmp_sort_chk) cmp_vars <- cmp_vars[c(2, 1)]
  km_4_delta <- unique(dta[!is.na(scheitel)]$km)
  dta <- merge(dta, case_tbl, by = "case")
  if (!identical(grp_vars, cmp_vars)) {
    dta_delta <-
      dcast.data.table(dta[!is.na(scheitel) & km %in% km_4_delta],
                       km ~ compare__by + group__by,
                       value.var = "scheitel"
      )
    for (i in grp_vars) {
      col_1 <- paste(cmp_vars[1], i, sep = "_")
      col_2 <- paste(cmp_vars[2], i, sep = "_")
      dta_delta[, eval(i) := get(col_1) - get(col_2)]
      dta_delta[, eval(col_1) := NULL]
      dta_delta[, eval(col_2) := NULL]
    }
    rm(i)
    dta_delta <- melt(dta_delta,
                      id.vars = "km",
                      variable.name = "group__by",
                      value.name = "delta",
                      sort = FALSE
    )
  }
  dta_delta <- merge(dta, dta_delta, by = c('group__by', 'km'))
  dta_delta <- dta_delta[zustand == cmp_vars[1]]
  g <- ggplot(dta_delta,
              aes(
                x = km,
                y = delta,
                color = !!ensym(color.by)
                # linetype = !!ensym(lt.by)
              )) +
    geom_line(size = 1.3) +
    ggtitle(title) +
    theme_bw(base_size = txt.size) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(1.5, 'cm'),
      legend.key.height = unit(0.3, 'cm'),
      # legend.background = element_blank(),
      axis.text = element_text(size = txt.size),
      axis.title = element_text(size = txt.size),
      panel.grid.major = element_line(color = "grey60", size = 0.3),
      panel.grid.minor = element_line(color = "grey60", size = 0.2),
      plot.title = element_text(size = txt_size - 2, margin = margin()),
      plot.caption = element_blank(),
      plot.margin = margin(l = 0.25, r = 0.25, t = 0, b = 0.2, unit = "cm"),
      legend.title = element_text(size = txt.size),
      legend.text = element_text(size = txt.size, margin = margin(l = 0.1, r = 0.1, unit = "cm")),
      panel.spacing = unit(0.5, "cm")

    ) +
    geom_hline(yintercept = 0) +
    scale_color_manual(values = hist_color_donau)
  if (is.null(x.lim)) x.lim <- range(dta$km)
  if (reverse.x) {
    x.lim <- sort(x.lim, decreasing = TRUE)
    g <- g +
      scale_x_reverse(name = x.lab,
                      limits = x.lim,
                      expand = x_expand,
                      breaks = x_breaks)
  } else {
    g <- g +
      scale_x_continuous(name = x.lab,
                         limits = x.lim,
                         expand = x_expand,
                         breaks = x_breaks)
  }
  g <- g + geom_line(size = 1)
  if (!is.null(facet.by)) {
    g <- g + facet_grid(rows = facet.by, scales = facet.scale)
  }
  g <- g + scale_y_continuous(
    name = y.lab,
    limits = y.lim,
    breaks = pretty(y.lim, 4), expand = y_expand)
  g
}

#' @export
plot_long <- function(
  case.list,
  case.desc = case.list,
  compare.by = "zustand",
  group.by = c("vgf", "zielpegel", "hwe"),
  fav = rhein_fav,
  param = "discharge",
  color.by = "hwe",
  lt.by = "vgf",
  color.name = color.by,
  lt.name = lt.by,
  color = col_all,
  lt = vgf_lt,
  color_breaks = ggplot2::waiver(),
  lt_breaks = ggplot2::waiver(),
  delta = FALSE,
  lubw_long = NULL,
  river = NULL,
  sobek.project = so_prj,
  do.par = TRUE,
  txt_size = 22,
  line_size = 1.3,
  ts.trim.left = NULL,
  input.data = NULL,
  x_lims = c(150, 860),
  x_title = "Rhein-km",
  rm_legend = FALSE,
  x_breaks = pretty(x_lims, 20, 20),
  x_expand = ggplot2::waiver(),
  y_lims = NULL,
  skl_title = NULL,
  hist_title = NULL,
  y_breaks = ggplot2::waiver(),
  heights = c(1.2, 3, 9),
  master.tbl = rhein_tbl
) {
  eval({
    color_breaks
    lt_breaks
    x_expand
    y_breaks
  })
  if (is.null(input.data)) {
    dta <- get_segment_data(
      case.list = case.list,
      case.desc = case.desc,
      sobek.project = sobek.project,
      param = param,
      ts.trim.left = ts.trim.left,
      get.max = TRUE,
      do.par = do.par,
      master.tbl = master.tbl
    )
  } else {
    dta <- copy(input.data)
  }
  dta <- dta[, .(km, scheitel, case)]
  case_tbl <- parse_case(case.desc, case.list)
  dta <- merge(dta, case_tbl, by = "case")
  dta[zustand == "Planzustand", zustand := "Alle Maßnahmen"]
  dta[, vgf := stri_replace_all_fixed(vgf, "Selten", "Selten 2")]
  dta[, vgf := stri_replace_all_fixed(vgf, "Mittel", "Selten 1")]
  cdesc_lst <- dta[, unique(paste(zustand, zielpegel, hwe, vgf, sep = "_"))]
  # processing unstability of the model by HW1988, ZP0
  if (grepl("rhein", x_title[[1]], ignore.case = TRUE)) {
    dta[hwe == "HW1988" & zielpegel == "ZP0" & km %between% c(838.8, 865.5), scheitel := NA]
    dta[hwe == "HW1988" & zielpegel == "ZP0" & km > 838,
        scheitel := approx(.I, scheitel, .I)$y, by = case]
  }

  if (!is.null(lubw_long)) {
    lubw_long[, notiz := "LUBW"]
    if ("datum" %in% colnames(lubw_long)) lubw_long[, datum := NULL]
    lubw_long[, case_desc := paste(zustand, zielpegel, hwe, vgf)]
    lubw_long[zielpegel == "Köln", zielpegel := "ZPK"]
    lubw_long[zielpegel == "Worms", zielpegel := "ZPW"]
    dta <- rbind(
      dta,
      lubw_long[paste(zustand, zielpegel, hwe, vgf, sep = "_") %in% cdesc_lst]
    )
  }
  if (param == "waterlevel") {
    # graphic for favorites
    fav_tit <- plot_station(dta = fav,
                            mea_same = TRUE,
                            drv_txt = 1.6,
                            hwr_txt = 1.6,
                            p2move = c("BB"), dp = 3,
                            hwr2move = c("6"), dhwr = 3,
                            m2move = c("SwE"), dm = 4,
                            limits = x_lims, x_breaks = x_breaks)

    # graphic for skalierung
    delta_skl <- cal_delta(dta[zielpegel != "ZP0"], compare.by, group.by)
    legend_pos <- ifelse(rm_legend, "none", "bottom")
    y_lbl <- ifelse(rm_legend, "Scheitelreduktion [m]", "")
    txt_format <- element_text(family = "Times New Roman", size = txt_size)
    g_skl <- ggplot(data = delta_skl[zustand == "Bezugszustand" & km %between% x_lims],
                    aes(x = km)) +
      geom_line(aes(
        x = km,
        y = delta,
        color = zielpegel,
        linetype = vgf
      ),
      size = line_size) +
      scale_x_continuous(name = x_title,
                         limits = x_lims,
                         breaks = x_breaks) +
      scale_y_continuous(name = y_lbl, limits = y_lims, breaks = y_breaks) +
      theme_bw(base_size = txt_size) +
      theme(
        legend.position = legend_pos,
        title = txt_format,
        text = txt_format,
        plot.title = txt_format,
        legend.text = txt_format,
        legend.title =  txt_format,
        panel.grid.major = element_line(color = "grey60", size = 0.3),
        panel.grid.minor = element_line(color = "grey60", size = 0.2),
        panel.spacing = unit(0.75, "cm"),
        legend.key.width = unit(1.2, "cm")
      ) +
      scale_color_manual(name = "Zielpegel",
                         breaks = color_breaks,
                         values = color) +
      scale_linetype_manual(name = "Skalierung",
                            breaks = color_breaks,
                            values = lt) +
      ggtitle(skl_title) +
      facet_grid(rows = vars(hwe))
    if (rm_legend) {
      g_skl <- g_skl + theme(
        strip.background = element_blank(),
        strip.text = element_blank()
      )
    }
    # graphic for hist
    g_skl_bld <- ggplot_build(g_skl)
    y_range <- range(g_skl_bld$layout$panel_params[[1]]$y.range)
    y_breaks <- g_skl_bld$layout$panel_params[[1]]$y$breaks
    dta_his <- dta[zielpegel == "ZP0"]
    delta_his <- cal_delta(dta_his, "zustand", "hwe")
    if (rm_legend) legend_pos <- "none" else legend_pos <- c(0.5, 0.15)

    g_his <- ggplot(data = delta_his[zustand == "Bezugszustand" & km %between% x_lims],
                    aes(x = km)) +
      geom_line(aes(
        x = km,
        y = delta,
        color = group__by
      ),
      size = line_size) +
      scale_x_continuous(name = "",
                         limits = x_lims,
                         breaks = x_breaks) +
      scale_y_continuous(limits = y_range, breaks = y_breaks) +
      theme_bw(base_size = txt_size) +
      theme(legend.position = legend_pos,
            title = txt_format,
            text = txt_format,
            plot.title = txt_format,
            legend.text = element_text(size = txt_size - 2),
            legend.title =  txt_format,
            axis.text.x = element_blank(),
            panel.grid.major = element_line(color = "grey60", size = 0.3),
            panel.grid.minor = element_line(color = "grey60", size = 0.2),
            axis.title = element_blank(),
            legend.direction = "horizontal",
            legend.margin = margin(),
            legend.key.width = unit(0.5, "cm")) +
      ggtitle(hist_title) +
      scale_color_manual(name = "",
                         breaks = color_breaks,
                         values = color) +
      guides(colour = guide_legend(nrow = 1))
    # combining graphics
    g <- patchwork::wrap_plots(fav_tit, g_his, g_skl,
                               ncol = 1, heights = heights)
  } else {
    # Q graphics ----------------------------------------------------------------
    if (delta) {
      dta[, group__by := do.call(paste, .SD), .SDcols = group.by]
      dta[, compare__by := do.call(paste, .SD), .SDcols = compare.by]
      grp_vars <- dta[, unique(group__by)]
      cmp_vars <- dta[, unique(compare__by)]
      g <- ggplot(data = dta, aes(x = km)) +
        geom_line(aes(
          x = km,
          y = delta,
          color = group__by,
          linetype = compare__by
        ),
        size = line_size)
    } else {
      dta[, group__by := do.call(paste, .SD), .SDcols = color.by]
      dta[, compare__by := do.call(paste, .SD), .SDcols = lt.by]
      g <- ggplot(data = dta, aes(x = km)) +
        geom_line(aes(
          x = km,
          y = scheitel,
          color = group__by,
          linetype = compare__by
        ),
        size = line_size)
    }
    g <- g +
      scale_x_continuous(name = "Rhein-km",
                         breaks = pretty(150:850, 10, 10)) +
      theme_bw(base_size = txt_size) +
      theme(
        legend.position = "bottom",
        axis.text = element_text(size = txt_size),
        axis.title = element_text(size = txt_size),
        legend.text = element_text(size = txt_size),
        panel.grid.major = element_line(color = "grey60", size = 0.3),
        panel.grid.minor = element_line(color = "grey60", size = 0.2),
        legend.title =  element_text(size = txt_size),
        legend.key.width = unit(1.2, "cm")
      ) +
      scale_color_manual(name = str_to_sentence(color.name),
                         breaks = color_breaks,
                         values = color) +
      scale_linetype_manual(name = str_to_sentence(lt.name),
                            breaks = lt_breaks,
                            values = lt)
    g$labels$colour <- str_to_sentence(color.name)
    g$labels$linetype <- str_to_sentence(lt.name)
    g$labels$y <- ifelse(param == "discharge",
                         "Scheitelablfuss [m³/s]", "Wasserstand [m]")
  }
  g
}


