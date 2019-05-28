#' Plot the W~Q curve at a measurement station (pegel)
#'
#' This function take an mID and draw the waterlevel-discharge at its location
#'
#' @param pegel Name of the measurement station (for the plot title)
#' @param ... other parameter to pass to his_from_case function
#'
#' @return a ggplot2 graphic
#' @export
w_q_pegel <- function(
  pegel = NULL,
  ...
  # mID = NULL,
  # qID = NULL,
  # wID = NULL,
  # case.name = NULL
  # sobek.project = NULL,
){
  qt <- his_from_case(...,
                      param = 'discharge')
  wt <- his_from_case(...,
                      param = 'waterlevel')
  colnames(qt) <- c('ts', 'Q', 'Legende')
  colnames(wt) <- c('ts', 'W', 'Legende')
  qwt <- merge(qt, wt, by = c('ts', 'Legende'), sort = FALSE)
  qwt[, Legende := str_extract(Legende, 'HW\\d{4}_.{6}')]
  qwt <- qwt[Q > min(Q, na.rm = TRUE) * 1.05]
  q_min <- min(qwt$Q, na.rm = TRUE)
  q_max <- max(qwt$Q, na.rm = TRUE)
  g <- ggplot(data = qwt, aes(x = Q, y = W,
                              color = Legende,
                              shape = Legende
                              )
              ) +
    theme_bw() + theme(legend.position = 'bottom') +
    labs(title = paste('Wasserstand-Abfluss Kurven am Pegel:', pegel),
         caption = unique(qwt$case)) +
    xlab('Abfluss (m³/s)') + ylab('Wasserstand (m+NHN)') +
    geom_point(size = 1) +
    # stat_smooth(method = 'glm', formula = y ~ poly(x, 2)) +
    scale_x_continuous(
      breaks = pretty(q_min:q_max, 10, 10)
    )
  g
}
