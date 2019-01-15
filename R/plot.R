get_pegel_column <- function(pegel = NULL, df = NULL, suffix = "",
                             all.lower = TRUE, short = 3L){
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


#' Plot graphic for some locations for one or more cases
#' @param
#' @param
#' @export
plot_locs <- function(case.names = "",
                      sobek.project = "",
                      IDs = "",
                      type = c("node", "reach", "mea")
){

}
