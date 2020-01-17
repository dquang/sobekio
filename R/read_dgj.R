#' Read monthly statistical value table from the DGJ page for one pegel
#'
#' @param pfile Path to pdf or .p file
#' @param type 'single' or 'multiple'. Type of Q-Table pdf, either all in one single page or multiple pages
#' @param as.is if TRUE return "raw" data
#' @return a data.table
#' @export
dgj_mon_tbl <- function(pfile, type = 'auto', as.is = FALSE) {
  type <- match.arg(type, choices = c('single', 'multiple', 'auto'))
  mon_names <- c('Jan', 'Feb', 'Mrz', 'Apr', 'Mai', 'Jun', 'Jul', 'Aug', 'Sep',
                 'Oct', 'Nov', 'Dez')
  chk_pdf <- isTRUE(grepl('pdf$', basename(pfile), ignore.case = TRUE))
  if (chk_pdf) {
    n_page <- tabulizer::get_n_pages(file = pfile)
    if (type == 'auto') type <- ifelse(n_page > 1, 'multipe', 'single')
    # working with single sheet table
    if (type == 'single') {
      e_tbl <- tabulizer::extract_tables(pfile,
                                         pages = 1,
                                         guess = FALSE,
                                         encoding = 'UTF-8',
                                         area = list(c(368, 154, 423, 553))
                                         )
      if (ncol(e_tbl[[1]]) > 12) e_tbl[[1]] <- e_tbl[[1]][, 2:13]
      if (nrow(e_tbl[[1]]) != 8) stop('unexpected output by reading pdf file')
      q_tbl <- data.table(e_tbl[[1]][3:7,])
      stat_period <- str_squish(paste(e_tbl[[1]][1,], collapse = ' '))
      stat_period <- stri_match_first_regex(stat_period, '(^[^ ]*) ')[, 2]
      stat_period <- str_replace(stat_period, '/', '-')
    } else {
      stat_period <- tabulizer::extract_text(
        file = pfile, pages = n_page, area = list(c(17, 459, 34, 573)))
      stat_period <- stri_match_first_regex(stat_period, '(\\d+-\\d+)')[, 2]
      stat_period <- str_replace(stat_period, '/', '-')
      e_tbl <- tabulizer::extract_tables(
        file = pfile, pages = n_page, npages = 1,
        guess = TRUE,
        method = "stream",
        encoding = 'UTF-8',
        columns = list(16),
        )
      if (nrow(e_tbl[[2]]) != 7) stop('unexpected output by reading pdf file')
      if (e_tbl[[1]][2, 2] == 'Dez') {
        mon_names <- c('Nov', 'Dez', 'Jan', 'Feb', 'Mrz', 'Apr', 'Mai',
                       'Jun', 'Jul', 'Aug', 'Sep', 'Oct')
      }
      q_tbl <- as.data.table(e_tbl[[2]][2:6,2:13])
    }

  } else {
    q_tbl <- fread(pfile, sep = '\n', header = FALSE)
    q_tbl[, org_ln := .I]
    stat_period_ln <- q_tbl[V1 == 'DGJ_Stammblock', org_ln] + 2
    # string template: (1925/2013)(1926/2014)(89)
    stat_period <- stri_match_first_regex(
      q_tbl[stat_period_ln, V1],
      '\\([^\\(]*\\(([^\\()]+)'
    )[, 2]
    stat_period <- str_replace(stat_period, '/', '-')
    tbl_start <- q_tbl[V1 == 'DGJ_Monatswerte_Aktuell', org_ln] + 2
    tbl_end <- tbl_start + 4
    q_tbl <- q_tbl[tbl_start:tbl_end]
    q_tbl[, dta := str_remove_all(V1, "\\(|\\)")]
    q_tbl[, dta := str_squish(dta)]
    q_tbl <- fread(text = q_tbl$dta, header = FALSE, sep = ' ', dec = ',')
    q_tbl <- q_tbl[, 3:14]
  }
  if (!as.is) {
    q_tbl <- q_tbl[, lapply(.SD, stri_replace_all_fixed,
                            replacement = '.', pattern = ',')]
    q_tbl <- q_tbl[, lapply(.SD, as.numeric)]
  }
  colnames(q_tbl) <- mon_names
  q_tbl$sta <- c('NQ', 'MNQ', 'MQ', 'MHQ', 'HQ')
  q_tbl$Pegel <- paste0('(', stat_period, ')')
  setcolorder(q_tbl, c('sta', mon_names, 'Pegel'))
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
