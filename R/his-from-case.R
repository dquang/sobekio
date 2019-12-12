#' Export data for nodes/reaches for multiple SOBEK Cases
#'
#' For all .HIS files subfolders.
#' @param case.list Path to the file containing SOBEK Cases to work with
#' @param sobek.project Path to Sobek Project folder
#' @param param The parameter in the .HIS file (waterlevel, discharge...)
#' @param verbose Default TRUE, to display some messages
#' @param get.max Default FALSE. If TRUE, the max values will return
#' @param get.abs.max Default FALSE. If TRUE, the values that have absolute max will return.
#' If get.abs.max and get.max both are TRUE, the absolute max will return
#' @param f.sep Seperator of the id.file, default = 'TAB'
#' @param f.header Logical. Has id.file header?
#' @param f.comment Comment character of the files. Default "#"
#' @param id.names Character vector for naming the IDs. Default NULL
#' @param case.desc For renaming case.list
#' @param id.type Type of ID, overwrite the ID_TYPE in ...
#' @param ... This accept only one parameter in syntax of ID_TYPE = ID_LIST.
#' ID_TYPE is one of wID, qID, mID, lID (latID), sID, pID, tID...
#' ID_LIST is a character vector.
#' For example mID = c('p_koeln', 'p_mainz')
#' \tabular{ll}{
#'   \strong{ID_TYPE} \tab \strong{DESCRIPTION} \cr
#'   mID          \tab Results at Measurements  \cr
#'   wID          \tab Results at Nodes         \cr
#'   qID          \tab Results at Reaches       \cr
#'   lID or latID \tab Results at Laterals      \cr
#'   sID          \tab Results at Structures    \cr
#'   pID          \tab Results for Pumpstations \cr
#'   tID          \tab Results for Triggers     \cr
#'   fmID         \tab Results from Flowmap     \cr
#'   fhID         \tab Results from Flowhis.his \cr
#'   moID         \tab Results from Morpmap.his \cr
#'   smID         \tab Results from Gsedmap.his \cr
#'   shID         \tab Results from Gsedhis.his \cr
#'   File name    \tab HIS file name without '.HIS'. Ex. reachvol = c('ID1', 'ID2')
#' }
#' @return a data.table
#' @export
his_from_case <- function(
  case.list, # path to list of cases to work with
  sobek.project, # path to Sobek Project folder
  param = 1, # index of the paramter to get data from .HIS file
  verbose = TRUE,
  get.max = FALSE, # instead of get the time series, get the max values only
  get.abs.max = FALSE,
  do.par = FALSE,
  f.comment = "#",
  f.header = FALSE,
  id.names = NULL,
  f.sep = "\t",
  case.desc = NULL,
  id.type = NULL,
  ...
  ) {
  if (isTRUE(get.abs.max)) get.max <- FALSE
  f_args <- as.list(match.call(expand.dots = FALSE))
  id_args <- list(...)
  if (length(id_args) != 1) {
    cat('"..." accepts only one parameter in form of id_type = id_list.\n')
    cat('for examples: mid = "p_worms"\n')
    stop('Wrong parameter input')
  }
  if (!is.null(id.type)) {
    id_type <- id.type
  } else {
    id_type <- names(f_args$...)
  }
  # processing case.list file
  if (is.character(case.list) && length(case.list) == 1 && file.exists(case.list)) {
    # reading case.list
    clist <- read.table(
      file = case.list,
      header = f.header,
      sep = f.sep,
      quote = "",
      comment.char = f.comment,
      stringsAsFactors = FALSE,
      blank.lines.skip = TRUE
    )
    clist <- data.table(clist)
    # first column in the clist is case_name
    colnames(clist)[1] <- "case_name"
  } else {
    if (is.vector(case.list)) {
      clist <- data.table(case_name = case.list)
    } else {
        stop("case.list must be a path to file or a character vector")
      }
  }
  if (!is.null(case.desc)) {
    stopifnot(length(case.desc) == length(case.list))
    clist$case_desc <- case.desc
  }
  if (!'case_desc' %in% colnames(clist)) {
    clist[, case_desc := case_name]
  } 
  id_list <- unlist(id_args, use.names = FALSE)
  if (length(id_list) == 1 && file.exists(id_list)) {
    id_tbl <- read.table(
      file = id_list,
      header = f.header,
      sep = f.sep,
      quote = "",
      comment.char = f.comment,
      stringsAsFactors = FALSE,
      blank.lines.skip = TRUE
    )
    id_list <- id_tbl[, 1]
    if (ncol(id_tbl) > 1) id.names <- id_tbl[, 2]
  }
  n_case <- length(case.list)
  # check SOBEK project
  sobek_cmt <- file_path(name = 'caselist.cmt', path = sobek.project)
  if (is.na(sobek_cmt)) {
    stop("Sobek Caselist.cmt does not exist! Check sobek.project Folder")
  }
  # reading SOBEK caselist.cmt
  sobek_clist <- data.table::fread(file = sobek_cmt,
                                  header = FALSE,
                                  sep = " ",
                                  quote = "'",
                                  strip.white = FALSE,
                                  encoding = 'Latin-1',
                                  stringsAsFactors = FALSE,
                                  blank.lines.skip = TRUE,
                                  colClasses = c('character', 'character'),
                                  col.names = c("case_number", "case_name")
                                  )
  sobek_clist <- sobek_clist[case_number != 0]
  clist <- merge(clist, sobek_clist, by = 'case_name', sort = FALSE,
                 all.x = TRUE)
  clist[tolower(case_name) == 'work', case_number := 'work']
  if (nrow(clist) == 0) stop('There is no matching case!')
  clist[is.na(case_number), not_found := .I]
  for (j in seq_along(clist[is.na(case_number), case_number])) {
    warning(paste("case: '", clist[not_found == j, case_name],
                "' is not in caselist.cmt'", sep = ''))
  }
  clist <- clist[!is.na(case_number), .SD, .SDcols = -c("not_found")]
  n_case <- nrow(clist)
  # check if clist contain a column for destination
  his_fname <- switch(
    toupper(id_type),
    MID = 'MEASSTAT.HIS',
    WID = 'CALCPNT.HIS',
    QID = 'REACHSEG.HIS',
    SID = 'STRUC.HIS',
    LID = 'QLAT.HIS',
    LATID = 'QLAT.HIS',
    PID = 'PUMP.HIS',
    TID = 'TRIGGERS.HIS',
    FMID = 'FLOWMAP.HIS',
    MOID = 'MORPMAP.HIS',
    FHID = 'FLOWHIS.HIS',
    SMID = 'GSEDMAP.HIS',
    SHID = 'GSEDHIS.HIS',
    paste0(id_type, '.HIS') # take id_type as file prefix
  )
  clist[, case_folder := file.path(sobek.project, case_number)]
  clist$his_file <- sapply(clist$case_folder, 
                           function(x) file_path(name = his_fname, path = x))
  if (isTRUE(do.par)) {
    # parallel computing here
    doParallel::registerDoParallel(parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    result <- foreach::foreach(i = 1:n_case, .combine = rbind) %dopar% {
      this_case_name <- clist$case_desc[[i]]
      this_case_hfile <- clist$his_file[[i]]
      his_data <- his_from_list(
        his.file = this_case_hfile, id.list = id_list, param = param
      )
      his_data$case <- this_case_name
      his_data
    }
    doParallel::stopImplicitCluster()
  } else{
    # if case.name found in the caselist.cmt
    result_list <- list(rep(NA, n_case))
    for (i in 1:n_case) {
      this_case_name <- clist$case_desc[[i]]
      this_case_hfile <- clist$his_file[[i]]
      his_data <- his_from_list(
        his.file = this_case_hfile, id.list = id_list, param = param
      )
      his_data$case <- this_case_name
      result_list[[i]] <- his_data
      rm(his_data)
    }
    result <- rbindlist(result_list)
    rm(result_list)
  }
  # get the max values if get.max == TRUE, only for one type of ID
  if (isTRUE(get.max)) {
      result <- result[, lapply(.SD, max,
      na.rm = TRUE),
      .SDcols = -c('ts'),
      by = case]
  }
  if (isTRUE(get.abs.max)) {
    result <- result[, 
                     lapply(.SD, function(x) {
                       x_abs <- abs(x)
                       i_max <- which.max(x_abs)
                       if (length(i_max) == 1) {
                         ret <- x[i_max]
                       } else{
                         warning('Could not found value that has absolute max. -Inf returned.')
                         ret <- -Inf
                       }
                       return(ret)
                       }
                       ),
                     .SDcols = -c('ts'),
                     by = case]
  }
  if (length(id.names) == length(id_list)) {
    colnames(result) <- c('ts', id.names, 'case')
  } else {
    if (!is.null(id.names)) warning('id.names is not same length as list of IDs, names were not set')
  }
    
  return(result)
}