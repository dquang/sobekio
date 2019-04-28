#' Plot long profile of selected case
#' @param indt Input data.table, which is the output from his_from_case function
#' @param case.list List of cases to plot, default is all
#' @param case.name Case display names, same values as case.list if not specified
#' @param delta Should delta also plotted?
#' @return a ggplot2 graphic
#' @export
plot_long_profile <- function(
  indt = qt,
  case.list = NULL,
  case.desc = case.list,
  linetype.by = 'case',
  color.by = 'case',
  facet.by = NULL,
  delta = F,
  x.lab = 'Lage (KM)',
  y.lab = 'Abfluss (m³/s)',
  x.lim = NULL,
  y.lim = NULL,
  y2.scale = 2,
  title = '',
  ncol = 1L,
  x.text.size = 8,
  master.tbl = rhein_tbl
){
  data_tbl <- copy(indt)
  if (!is.null(case.list)){
    case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
    data_tbl <- merge(data_tbl, case_tbl, by = 'case', sort = FALSE)
  }
  b_tick <- data_m[!is.na(besonderheit), c("km", "besonderheit")]
  b_tick <- b_tick[nchar(besonderheit) > 0, ]
  if (isTRUE(delta)){
    x_min <- min(data_m$km, na.rm = TRUE)
    x_max <- max(data_m$km, na.rm = TRUE)
    y_min <- min(data_m$value, na.rm = TRUE)
    y_max <- max(data_m$value, na.rm = TRUE)
    data_m2 <- dcast(data_m, km + besonderheit + hwe + ID + VGF ~ Zustand,
                     value.var = 'value')

    data_m2[, Delta := get(zustand2) - get(zustand1)]
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
            axis.text.x = element_text(angle = 90,
                                       hjust = 0,
                                       size = x.text.size)) +
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
    g <- ggplot(data = data_m,
                aes(x = km,
                    linetype = VGF,
                    y = value)
    ) +
      geom_line(aes(color = Zustand), size = 1)+
      theme_bw() +
      theme(legend.position = 'bottom',
            axis.text.x = element_text(angle = 90,
                                       hjust = 0,
                                       size = x.text.size)) +
      labs(title = title) +
      scale_x_reverse(
        name = x.lab,
        # breaks = pretty(x_min:x_max, 10),
        sec.axis =  dup_axis(
          name = 'Station',
          breaks = b_tick$km,
          labels = b_tick$besonderheit)
        ) +
      scale_y_continuous(
        # breaks = pretty(y_min:y_max, 10),
        name = y.lab
        ) +
      scale_color_hue(name = 'Farbe') +
      scale_linetype_discrete(name = 'Linienart')+
      facet_wrap(.~hwe, ncol = ncol)
  }

  return(g)
}
