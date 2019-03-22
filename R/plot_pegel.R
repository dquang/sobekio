#' Ganglinien an jedem Pegel (3 Linie pro Darstellung)
#' @param pos Name of Pegel
#' @param indt Input data.table
#' @param x.lab X-Axis label, default 'Zeit'
#' @param y.lab Y-Axis label, default 'Abfluss (m³/s)'
#' @param y2.scale Scale between y_max/y2_max
#' @param title
#' @param facet
#' @param facet_list List of facetting parameters (ex. list(hwe = 'free_x'))
#' This parameter is currently not fully implemented
#' @param vgf.pattern
#' @param zustand1
#' @param zustand2
#' @param outliner.prob Probability for removing Outliner (by sd function)
#' @param sep.delta Should Delta be drew on a second graphic underneath
#' @return A ggplot2 graphic
plot_pegel <- function(
  pos = 'Trunstadt',
  indt,
  x.lab = 'Zeit',
  y.lab = 'Abfluss (m³/s)',
  y2.scale = 2,
  title = paste(str_extract(y.lab, "[^ ]*"),
    'Ganglinien am Pegel:', pos),
  facet = TRUE,
  facet_list = list(hwe = 'free_x'),
  vgf.pattern = "Selten|Mittel",
  zustand1 = 'Bezugszustand',
  zustand2 = 'Planzustand',
  outliner.prob = 0.02,
  sep.delta = FALSE
){
  data_tb <- data.table(indt)
  data_tb <- data_tb[, .SD, .SDcols = c('ts', pos, "case")]
  colnames(data_tb) <- c('ts', "pegel", "case")
  # if (!is.null(outliner.prob)) {
    data_tb[, ts_min := min(ts), by = case]
    data_tb[year(ts)==1988, ts_min := ts_min + 24*5*3600]
    data_tb[year(ts)==1995, ts_min := ts_min + 24*2*3600]
    data_tb[year(ts)==2002, ts_min := ts_min + 24*3*3600]
    data_tb[year(ts)==2003, ts_min := ts_min + 24*3*3600]
    data_tb <- data_tb[ts > ts_min]
    # }
  zustand_pattern = paste(zustand1, zustand2, sep = "|")
  data_tb[, Zustand := str_extract(case, zustand_pattern)]
  data_tb[, VGF := str_extract(case, vgf.pattern)]
  data_tb <- dcast(data_tb, ts + VGF ~ Zustand,
                   value.var = 'pegel'
                   )
  data_tb[, hwe := year(ts)]
  data_tb[hwe == 2002, hwe := 2003]
  data_tb[, Delta := get(zustand2) - get(zustand1)]
  # finding Delta am Scheitel
  data_tb[, max_value := max(get(zustand1)), by = c("hwe", 'VGF')]
  data_tb[max_value == get(zustand1), max_ts := ts]
  data_tb[max_value == get(zustand1),
          max_delta := Delta
          ]

  # if (!is.null(outliner.prob)) {
  #   data_tb <- data_tb[
  #     abs(Delta) < abs(quantile(Delta, probs = outliner.prob)[[1]])
  #                      ]
  # }

  # print(colnames(data_tb))
  # finding scale for both axis
  y2_scale <- y2.scale
  y_min1 <- min(data_tb[, .SD, .SDcols = c(zustand1, zustand2)], na.rm = TRUE)
  y_max <- max(data_tb[, .SD, .SDcols = c(zustand1, zustand2)], na.rm = TRUE)
  y2_min <- min(data_tb$Delta, na.rm = TRUE)
  y_min <- y_min1 #- abs(y2_min)*y2_scale
  y_pretty <- pretty(y_min:y_max, 5, 5)
  y2_shift <- y_pretty[2]
  y2_max <- y_max/y2_scale
  # y2_min_shift <- y2_min - y2_shift/y2_scale
  y2_pretty <- pretty(y2_min:y2_max, 5, 5)
  # plotting
  g <- ggplot(data = data_tb,
              aes(x = ts,
                  y = get(zustand1),
                  color = zustand1,
                  linetype = VGF
                  ))+
    scale_x_datetime(date_breaks = '3 days',
                     date_labels = "%d.%m") +
    theme_bw() +
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90)) +
    xlab(x.lab) + ylab(y.lab) +
    labs(title = title,
            caption = ifelse(!is.null(outliner.prob),
                              "Notiz: sprunghafte Werte am Anfang wurde gelöst",
                              waiver()
                             )
            ) +
    geom_line(size = 1) +
    geom_line(aes(y = get(zustand2), color = zustand2),
              size = 1) +
    scale_color_hue(name = 'Farbe') +
    scale_linetype_discrete(name = 'Linienart')
  if (isTRUE(sep.delta)){
    g_delta <- ggplot(data = data_tb,
                      aes(x = ts,
                          y = Delta,
                          linetype = VGF
                      )) +
      scale_x_datetime(date_breaks = '3 days',
                       date_labels = "%d.%m") +
      theme_bw() +
      theme(legend.position = 'bottom',
            axis.text.x = element_text(angle = 90)) +
      xlab(x.lab) + ylab(paste(
        str_extract(y.lab, '.*\\ '), 'Differenz',
        str_extract(y.lab, '\\ (.*)')
      )
      ) +
      geom_line(size = 1, color = 'black') +
      scale_linetype_discrete(name = 'Linienart')
    if(facet == TRUE){
      g_delta <- g_delta + facet_wrap(.~ hwe, scales = 'free_x')
      g <- g + facet_wrap(.~ hwe, scales = 'free_x')
    }
    g_result <- gridExtra::arrangeGrob(g, g_delta, nrow = 2)
  } else{
    g_result <- g +
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
    if(facet == TRUE){
      g_result <- g_result + facet_wrap(.~ hwe, scales = 'free_x')
    }
  }
  return(g_result)
}
