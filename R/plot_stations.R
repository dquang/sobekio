#'@export
plot_station_elbe <- function(
  dta = elbe_fav,
  limits = NULL,
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
                data = p_dta,
                vjust = 0.5, hjust = 0
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
  txt_size = 16,
  p_size = 3,
  m_size = 3,
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
  p_angle = 90,
  hwr_angle = 0,
  drv_angle = 0,
  m_angle = 90,
  m_txt_repel = FALSE

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
    g <- g + scale_x_reverse(limits = sort(limits, decreasing = TRUE), breaks = x_breaks)
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
                angle = hwr_angle,
                check_overlap = TRUE,
                vjust = 0, hjust = 0.5
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
                angle = drv_angle,
                vjust = 0,
                hjust = 0.5
      )
  }
  if (p_chk) {
    g <- g +
      # add pegel
      geom_point(aes(y = p_pts, shape = "Pegel"),
                 color = hwr_color,
                 data = p_dta,
                 size = p_size
      ) +
      geom_text(aes(y = p_txt, label = akz),
                color = hwr_color,
                angle = p_angle,
                data = p_dta,
                vjust = 0.5, hjust = 0
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
      g <- g + geom_text_repel(aes(y = m_txt, label = akz),
                color = m_color,
                show.legend = FALSE,
                angle = m_angle,
                vjust = 0.5, hjust = 0,
                data = m_dta
      )
    } else {
      g <- g + geom_text(aes(y = m_txt, label = akz),
                      color = m_color,
                      show.legend = FALSE,
                      angle = m_angle,
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
