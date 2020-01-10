#' Read monthly statistical value table from the DGJ page for one pegel
#'
#' @param pfile Path to pdf or .p file
#' @return a data.table
#' @export
dgj_mon_tbl <- function(pfile) {
  mon_names <- c('Jan', 'Feb', 'Mrz', 'Apr', 'Mai', 'Jun', 'Jul', 'Aug', 'Sep',
                 'Oct', 'Nov', 'Dez')
  chk_pdf <- isTRUE(grepl('pdf$', basename(pfile), ignore.case = TRUE))
  if (chk_pdf) {
    q_tbl <- tabulizer::extract_tables(pfile,
                                       pages = 1,
                                       guess = FALSE,
                                       area = list(c(376, 171, 424, 557)))
    q_tbl <- data.table(q_tbl[[1]])[, lapply(.SD, as.numeric)]
    q_tbl <- q_tbl[2:6,]
  } else {
    q_tbl <- fread(pfile, sep = '\n', header = FALSE)
    q_tbl[, org_ln := .I]
    tbl_start <- q_tbl[V1 == 'DGJ_Monatswerte_Aktuell', org_ln] + 2
    tbl_end <- tbl_start + 4
    q_tbl <- q_tbl[tbl_start:tbl_end]
    q_tbl[, dta := str_remove_all(V1, "\\(|\\)")]
    q_tbl[, dta := str_squish(dta)]
    q_tbl <- fread(text = q_tbl$dta, header = FALSE, sep = ' ')
    q_tbl <- q_tbl[, 3:14]
  }
  colnames(q_tbl) <- mon_names
  q_tbl$sta <- c('NQ', 'MNQ', 'MQ', 'MHQ', 'HQ')
  setcolorder(q_tbl, c('sta', mon_names))
  return(q_tbl)
}


#' Read table of extreme events from DGJ Table for one pegel
#'
#' @param pfile Path to pdf or .p file
#' @param as.is if TRUE return "raw" data
#' @return a data.table
#' @export
dgj_top_hq <- function(pfile, as.is = FALSE) {
  chk_pdf <- isTRUE(grepl('pdf$', basename(pfile), ignore.case = TRUE))
  if (chk_pdf) {
    q_tbl <- tabulizer::extract_tables(pfile,
                                       pages = 1,
                                       guess = FALSE,
                                       area = list(c(675.1, 210.5, 735.2, 343.6)))
    q_tbl <- data.table(q_tbl[[1]])
  } else {
    q_tbl <- fread(pfile, sep = '\n', header = FALSE)
    q_tbl[, org_ln := .I]
    tbl_start <- q_tbl[V1 == 'DGJ_Extremwerte_N', org_ln] + 1
    tbl_end <- tbl_start + 9
    q_tbl <- q_tbl[tbl_start:tbl_end]
    q_tbl[, dta := str_remove_all(V1, "\\(|\\)")]
    q_tbl[, dta := str_squish(dta)]
    q_tbl <- fread(text = q_tbl$dta, header = FALSE, sep = ' ')
  }
  if (!isTRUE(as.is)) {
    colnames(q_tbl) <- c('Q', 'Spende', 'W', 'Date')
    q_tbl[, Date := as.POSIXct(Date, tz = 'GMT', format = '%d.%m.%Y')]
    num_cols <-  c('Q', 'Spende', 'W')
    q_tbl[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  }
  return(q_tbl)
}


#' Read table of extreme events from DGJ Table for one pegel
#'
#' @inheritParams dgj_top_hq
#' @return a data.table
#' @export
dgj_top_nq <- function(pfile, as.is = FALSE) {
  chk_pdf <- isTRUE(grepl('pdf$', basename(pfile), ignore.case = TRUE))
  if (chk_pdf) {
    q_tbl <- tabulizer::extract_tables(pfile,
                                       pages = 1,
                                       guess = FALSE,
                                       area = list(c(676.7, 99.7, 735.3, 213.7)))
    q_tbl <- data.table(q_tbl[[1]])
  } else {
    q_tbl <- fread(pfile, sep = '\n', header = FALSE)
    q_tbl[, org_ln := .I]
    tbl_start <- q_tbl[V1 == 'DGJ_Jahreshauptwerte_Viel', org_ln] + 1
    tbl_end <- tbl_start + 9
    q_tbl <- q_tbl[tbl_start:tbl_end]
    q_tbl[, dta := str_remove_all(V1, "\\(|\\)")]
    q_tbl[, dta := str_squish(dta)]
    q_tbl <- fread(text = q_tbl$dta, header = FALSE, sep = ' ')
  }
  if (!isTRUE(as.is)) {
    colnames(q_tbl) <- c('Q', 'Spende', 'Date')
    q_tbl[, Date := as.POSIXct(Date, tz = 'GMT', format = '%d.%m.%Y')]
    num_cols <-  c('Q', 'Spende')
    q_tbl[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  }
  return(q_tbl)
}
