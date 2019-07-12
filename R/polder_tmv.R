#' Search for a polder TMV
#' 
#' @param volume Volume of the Polder
#' @param indt The discharge data.table. Only the first two columns will be used. It should have the format 'ts|value'
#' @param n.days Maximum number of days under the flood peak should the topping value being searched
#' @param value.step The discharge will be searched from the minimum to maximum by this value.step
#' @param tolerance The acceptable tolerance of calculated Volume and given Volume
#' @param clipboard Should the output table write to clipboard, ready for pasting in Sobek? Default TRUE
#' @param print.plot Should a plot be showed? Default FALSE
#' @param ... If indt is NULL these parameters will be passed to his_from_case function for reading the discharge
#' @export
polder_tmw <- function(
  volume = NULL,
  indt = NULL,
  n.days = 2,
  value.step = 0.1,
  tolerance = 0.01,
  clipboard = TRUE,
  print.plot = FALSE,
  ...
){
  if (is.null(indt)){
    # print(substitute(his_from_case(...)))
    qt <- his_from_case(...)
    qt <- qt[, c(1, 2)]
  } else{
    qt <- indt[, c(1, 2)]
  }
  colnames(qt) <- c('ts', 'value')
  # qt_peak <- max(qt$value, na.rm = TRUE)
  qt[, dt := shift(ts, 1)]
  qt <- qt[!is.na(dt)]
  qt[, t_step := difftime(ts, dt, units = 'secs')]
  qt[, t_step := as.numeric(t_step)]
  # finding value_max
  value_max <- max(qt$value, na.rm = TRUE)
  t_value_max <- qt[value == value_max, ts]
  t_search_min <- t_value_max[[1]] - n.days*24*3600
  qt2 <- qt[ts > t_search_min]
  value_min <- min(qt2$value, na.rm = TRUE)
  q0_seq <- seq(value_min, value_max, value.step)
  q0_seq_length <- length(q0_seq)
  v_max <- 0
  print('Searching for topping discharge q0....')
  i_cur <- 0
  # q0 <- q0_seq[i_cur]
  i_min <- 1
  i_max <- q0_seq_length
  i_cur <- (i_max - i_min + 1) %/% 2
  cont = TRUE
  while (cont){
    # print(i_cur)
    q0 <- q0_seq[i_cur]
    ret <- qt2[value >= q0]
    ret <- ret[, qin := value - q0]
    # ret[value <= selected.value, qin := 0]
    ret[, vt := qin*t_step]
    ret$cs <- cumsum(ret$vt)
    v_max <- ret[.N, cs]/10^6
    # print(v_max)
    if (!near(v_max, volume, tolerance)){
      if (v_max > volume){
        # if v_max is bigger than volume, then going up, to reduce v_max
        i_min <- i_cur
        i_cur <- (i_max - i_min + 1) %/% 2 + i_cur
      } else {
        # if v_max is less than volume, then going down, to increase v_max
        i_max <- i_cur
        i_cur <- (i_max - i_min + 1) %/% 2
      }
    } else{
      print(paste('Found!, q0 = ', round(q0, 1), ". V_in = ", round(v_max, 3),
                  sep = ""))
      cont = FALSE
    }
  }
  ret[, date := strftime(ts, format = '%d.%m.%Y', tz = 'GMT')]
  ret[, time := strftime(ts, format = '%H:%M:%S', tz = 'GMT')]
  ret <- ret[, .SD, .SDcols = c('date', 'time', 'qin')]
  if (isTRUE(clipboard)){
    write.table(
      ret,
      'clipboard-4096',
      quote = FALSE,
      sep = "\t",
      row.names = FALSE,
      col.names = FALSE,
      dec = ","
    )
  }

  if (isTRUE(print.plot)){
    qt <- qt[, .SD, .SDcols = c('ts', 'value')]
    qt <- qt[ts > t_value_max - 5*24*3600 &
               ts < t_value_max + 5*24*3600]
    qt[value > q0, value_gekappt := q0]
    qt[value <= q0, value_gekappt := value]
    qt <- melt(qt, id.vars = 'ts', variable.name = 'Ganglinien')
    g <- ggplot(qt, aes(x = ts, y = value, color = Ganglinien)) +
      scale_x_datetime() +
      theme(legend.position = 'bottom') +
      geom_line(size = 1) +
      geom_hline(yintercept = q0) +
      geom_text(aes(min(qt$ts), q0, label = round(q0, 1), vjust = 1))
    print(g)
  }
  print(paste('max reduction =', round(max(ret$qin, na.rm = TRUE), 1)))
  return(ret)
}
