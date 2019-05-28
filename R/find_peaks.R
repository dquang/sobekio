#' Find the center of (mass) volume and peaks
#' @param indt input data.table
#' @param peak.percent peak area as percent, ex. 0.05 means that the 5% on the top is consider as peak area.
#' @param peak.min minimum value given as the starting value of area of peak.
#' @param column Name of the column in the indt to work with
#' @param nups minimum number of increasing steps before a peak is reached
#' @param ndowns minimum number of decreasing steps after the peak
#' @return a list of (plot = graphic, peaks = matrix of peaks, and m_center = row index for the center)
#' @export
volume_center <- function(
  indt = qt,
  peak.percent = 0.05,
  peak.min = NULL,
  column = 'P_Mainz',
  nups = 3,
  ndowns = nups
){
  # stopifnot(column %in% colnames(indt)
  indt <- indt[, c('ts', column)]
  colnames(indt) <- c('ts', 'value')
  indt[, orig_row := .I]
  if (is.null(peak.min)){
    peak_min <- max(indt$value, na.rm = TRUE) * (1 - peak.percent)
  } else{
    peak_min <- peak.min
  }
  indt_peak <- indt[value >= peak_min]
  # find peaks using findpeaks from pracma package
  f_peaks <- pracma::findpeaks(indt_peak$value, nups = nups, ndowns = ndowns)
  # find the center of mass
  m_center <- sum(cumsum(indt_peak$value) <= 0.5 * sum(indt_peak$value))
  g <- ggplot(indt, aes(x = orig_row, y = value)) + geom_line() +
    geom_vline(xintercept = indt_peak[m_center, orig_row], color = 'blue',
               linetype = 2) +
    theme_bw() +
    geom_segment(aes(
                x = indt_peak[1, orig_row],
                y = peak_min,
                xend = indt_peak[.N, orig_row],
                yend = peak_min
                ), color = 'red')
  for (i in seq_along(f_peaks[, 2])){
    g <- g + geom_vline(xintercept = indt_peak[f_peaks[i,2], orig_row],
               color = 'blue')
  }
  print(g)
  return(list(plot = g, peaks = f_peaks,
              m_center = indt_peak[m_center, orig_row]))
}

