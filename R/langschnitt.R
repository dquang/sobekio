#' Plot long profile of selected case
#' @param indt Input data.table, which is the output from his_from_case function
#' @case.list List of cases to plot, default is all
#' @case.name Case display names, same values as case.list if not specified
#' @param delta Should delta also plotted?
#' @param code.tbl Coding table
#'
plot_long_profile <- function(
  indt = qt,
  case.list = NULL,
  case.name = case.list,
  delta = F,
  code.tbl = qids_f,
  ID.col = 'ID',
  x.lab = 'Lage (KM)',
  y.lab = 'Abfluss (m³/s)',
  x.lim = NULL,
  y.lim = NULL,
  y2.scale = 2,
  title = '',
  vgf.pattern = "Selten|Mittel",
  zustand1 = 'Bezugszustand',
  zustand2 = 'Planzustand',
  ncol = 1L,
  x.text.size = 8,
  save.tbl = NULL
){
  if (!is.data.table(indt)|FALSE %in% c('ts','case') %in% colnames(indt)){
    stop(indt, ' has wrong format')
  }
  if (!is.null(case.list)){
    data_tb <- subset(indt, case %in% case.list)
    for (i in seq_along(case.list)) {
      data_tb[case == case.list[i], case := case.name[i]]
    }
  } else {
    data_tb <- copy(indt)
    if (!is.null(case.name)){
      for (i in seq_along(case.list)) {
        data_tb[case == case.list[i], case := case.name[i]]
      }
    }
  }

  # lage_all <- colnames(indt[, -c('ts', 'case')])
  if (file.exists(code.tbl)) {
    code_tbl <- fread(file = code.tbl,
                      sep = "\t", header = T,
                      dec = ",")
    code_tbl[, km := as.numeric(km)]
  } else{
    stop(code.tbl, ' not found')
  }
  data_m <- data_tb[, lapply(.SD, max, na.rm = TRUE),
                    .SDcols = -c('ts'), by = case]
  data_m <- melt(data_m, id.vars = 'case',
                 variable.name = 'ID')
  zustand_pattern = paste(zustand1, zustand2, sep = "|")
  data_m[, Zustand := str_extract(case, zustand_pattern)]
  data_m[, VGF := str_extract(case, vgf.pattern)]
  data_m[, hwe := str_extract(case, 'HW[0-9]{4}')]
  data_m <- merge(data_m, code_tbl, by.x = 'ID', by.y = ID.col)
  if(!is.null(save.tbl)){
    fwrite(data_m, file = save.tbl,
           sep = "\t", dec = ",")
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
