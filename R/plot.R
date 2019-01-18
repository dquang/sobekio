#' Return data for one column in a data table with value stored in the column parameter
#' @export
#' @param column Name of the column
#' @param df a data.table or data.frame
#' @param all.lower Should column converted to lower case? Default = FALSE#
#' @param suffix Adding suffix to the column name
#' @param short Get only a given number of character, from left, of the column name
#' @return behave like column subsetting of data.table/data.frame
get_column_by_name <- function(column = NULL, df = NULL, suffix = "",
                             all.lower = F, short = 0L){
  if (all.lower) column <- tolower(column)
  if (is.integer(short) && short > 1) column <- substr(column, 1, short)
  if (!is.data.frame(df)) stop('df must be a data.frame or data.table')
  column_col <- paste(column, suffix, sep = "")
  all_cols <- colnames(df)
  if (!column_col %in% all_cols){
    warning('no column with name: ', column_col, ' in df')
    rel <- NULL
  } else{
    if(is.data.table(df)){
      rel <- df[, get(column_col)]
    } else{
      rel <-  df[, column_col]
    }
  }
  return(rel)
}

#' Quick plot for long profile or pos (column name)
#' @import ggplot2
#' @export
#' @param indt Input data.table or data.frame
#' @param pos "longprofile" or list of list of column to plot
#' @param x.lab x label
#' @param y.lab y label
#' @param title title
#' @param x.angle rotate angle of x.label
#' @param x.hjust hjust of x.label
#' @param size size of line/point
#' @param faced default FALSE. If TRUE, faced by case
#' @param sort to short longprofile by max value, not yet implemented
#' @return a ggplot2 graphic object
plot_wirkung <- function(indt = NULL,
                         pos = "longprofile",
                         x.lab = 'Lage',
                         y.lab = 'Wasserstand (m+NHN)',
                         title = '',
                         x.angle = 90L,
                         x.hjust = 1L,
                         size = 1L,
                         faced = FALSE,
                         sort = TRUE){

  data_tb <- data.table(indt)
  lage_all <- colnames(data_tb)
  lage_all <- lage_all[-grep("ts|case", lage_all)]
  if (pos[[1]] == "longprofile"){
    data_tb <- data_tb[, lapply(.SD, max, na.rm = TRUE), by = case]
    data_tb <- melt(data_tb,
                    id.vars = c('case'),
                    variable.name = 'lage',
                    measure.vars = lage_all
    )
    g <- ggplot(data = data_tb,
                mapping = aes(x = lage, y = value, color = case)
    ) + theme_bw() +
      theme(legend.position = 'bottom',
            axis.text.x = element_text(angle = x.angle, hjust = x.hjust)
      ) +
      xlab(x.lab) +
      ylab(y.lab) +
      ggtitle(title) +
      geom_point(size = size)
  } else{
    for (p in pos){
      if ( !p %in% lage_all) stop('column: ', p, ' not found in data table')
    }
    data_tb <- data_tb[, c('ts', pos, 'case'), with = FALSE]
    # print(data_tb)
    data_tb <- melt(data_tb,
                    id.vars = c('ts', 'case'),
                    variable.name = 'Lage'
    )
    # print(data_tb)
    g <- ggplot(data = data_tb,
                mapping = aes(x = ts,
                              # y = get_column_by_name(p, data_tb),
                              y = value,
                              color = case,
                              linetype = Lage)
    ) +
      scale_x_datetime() + theme_bw() +
      theme(legend.position = 'bottom',
            axis.text.x = element_text(angle = x.angle, hjust = x.hjust)
      ) +
      xlab(x.lab) +
      ylab(y.lab) +
      ggtitle(title) +
      geom_line(size = size)
  }
  if (faced) g <- g + facet_wrap(.~case, scales = 'free')
  return(g)
}
