get_pegel_column <- function(pegel = NULL, df = NULL, suffix = "",
                             all.lower = F, short = 0L){
  if (all.lower) pegel <- tolower(pegel)
  if (is.integer(short) && short > 1) pegel <- substr(pegel, 1, short)
  if (!is.data.frame(df)) stop('df must be a data.frame or data.table')
  pegel_col <- paste(pegel, suffix, sep = "")
  all_cols <- colnames(df)
  if (!pegel_col %in% all_cols){
    warning('no column with name: ', pegel_col, ' in df')
    rel <- NULL
  } else{
    if(is.data.table(df)){
      rel <- df[, get(pegel_col)]
    } else{
      rel <-  df[, pegel_col]
    }
  }
  return(rel)
}

#' Quick plot for long profile or pegel (column name)
#' @import ggplot2
#' @export
#' @param indt Input data.table or data.frame
#' @param pegel
#' @param sort to short longprofile by max value, not yet implemented
plot_wirkung <- function(indt = NULL,
                         pegel = "longprofile",
                         sort = TRUE){

  data_tb <- data.table(indt)
  lage_all <- colnames(data_tb)
  lage_all <- lage_all[-grep("ts|case", lage_all)]
  if (pegel == "longprofile"){
    data_max <- data_tb[, lapply(.SD, max, na.rm = TRUE), by = case]
    data_max_melt <- melt(data_max,
                          id.vars = c('case'),
                          variable.name = 'lage',
                          measure.vars = lage_all
    )
    g <- ggplot(data = data_max_melt,
                mapping = aes(x = lage, y = value, color = case)
    ) +
      theme(legend.position = 'bottom') +
      xlab('Lage') +
      ylab('Wasserstand (m+NHN)') +
      geom_point()
  } else{
    if ( !pegel %in% lage_all) stop('pegel: ', pegel, ' not found')
    g <- ggplot(data = data_tb,
                mapping = aes(x = ts,
                              y = get_pegel_column(pegel, data_tb),
                              color = case)
    ) + scale_x_datetime() +
      theme(legend.position = 'bottom') +
      xlab('Lage') +
      ylab('Wasserstand (m+NHN)') +
      geom_point()
  }
  return(g)
}
