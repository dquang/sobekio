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
#' @param id_name Default NULL. To set names for the IDs
#' @param f.sep Seperator of the id.file, default = 'TAB'
#' @param f.header Logical. Has id.file header?
#' @param f.comment Comment character of the files. Default "#"
#' @param id.names Character vector for naming the IDs. Default NULL
#' @param ... This accept only one parameter in syntax of ID_TYPE = ID_LIST.
#' ID_TYPE is one of wID, qID, mID, lID (latID), sID, pID, tID
#' ID_LIST is a character vector.
#' For example mID = c('p_koeln', 'p_mainz')
#' @return a data.table
#' @export
his_from_case <- function(
  case.list = NULL, # path to list of cases to work with
  sobek.project = NULL, # path to Sobek Project folder
  param = 1, # index of the paramter to get data from .HIS file
  verbose = TRUE,
  get.max = FALSE, # instead of get the time series, get the max values only
  get.abs.max = FALSE,
  do.par = FALSE,
  f.comment = "#",
  f.header = FALSE,
  id.names = NULL,
  f.sep = "\t",
  ...
  ) {
  if (isTRUE(get.abs.max)) get.max <- FALSE
  f_args <- as.list(match.call(expand.dots = FALSE))
  id_args <- list(...)
  # h_args <- as.list(match.call(expand.dots = TRUE))
  stopifnot(!is.null(case.list) & !is.null(sobek.project))
  id_types <- c('MID', 'WID', 'QID', 'LID', 'LATID', 'SID', 'PID', 'TID')
  id_type <- names(f_args$...)
  if (length(id_args) != 1 | !toupper(id_type) %in% id_types) {
    print('You may have typos in parameter names! Check following parameters:')
    print(names(f_args$...))
    stop("List of IDs must be given and ID_TYPE is one of: ",
         "c('mID', 'wID', 'qID', 'lID', 'latID', 'sID', 'pID', 'tID')"
    )
  }
  # processing case.list
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
      clist <- data.table(case_name = case.list,
                          stringAsFactors = FALSE)
    } else {
        stop("case.list must be a path to file or a character vector")
      }
    }
  id_list <- id_args[[id_type]]
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
  n_case = length(case.list)
  # n_id = length(id_list)
  # check SOBEK project
  sobek_cmt <- paste(sobek.project, "caselist.cmt", sep = "/")
  if (!file.exists(sobek_cmt)) {
    stop("Sobek Caselist.cmt does not exist! Check sobek.project Folder")
  }
  # reading SOBEK caselist.cmt
  sobek_clist <- data.table::fread(file = sobek_cmt,
                                  header = FALSE,
                                  sep = " ",
                                  quote = "'",
                                  stringsAsFactors = FALSE,
                                  blank.lines.skip = TRUE,
                                  colClasses = c('character', 'character'),
                                  col.names = c("case_number", "case_name")
                                  )
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
    TID = 'TRIGGERS.HIS'
  )
  clist[, his_file := paste(sobek.project,
                            case_number, his_fname, sep = "/")
        ]
  if (isTRUE(do.par)) {
    # parallel computing here
    # requireNamespace("doParallel", quietly = TRUE)
    # requireNamespace("foreach", quietly = TRUE)
    doParallel::registerDoParallel(parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    result <- foreach::foreach(i = 1:n_case, .combine = rbind) %dopar% {
      this_case_name <- clist$case_name[[i]]
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
    for (i in 1:n_case){
      this_case_name <- clist$case_name[[i]]
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
  if (isTRUE(get.max)){
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
                       if (length(i_max) == 1){
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


#' Export data for nodes/reaches for multiple SOBEK Cases
#'
#' For all .HIS files subfolders.
#' @param case.list Path to the file containing SOBEK Cases to work with
#' @param sobek.project Path to Sobek Project folder
#' @param param Index of the Paramter to get the data, default = 1
#' @param qID Path to file containing list of reach IDs for struct.his
#' @param wID Path to file containing list of node IDs for calcpnt.his
#' @param lID Path to file containing list of node IDs for lateral.his
#' @param sID Path to file containing list of node IDs for struct.his
#' @param mID Path to file containing list of node IDs for measstat.his
#' IDs list shoulde have the first column with IDs(col.name = "sobek_id")
#' Second column (col.name = "name") is for Names correspond to the IDs
#' @param f.sep Seperator of the id.file, default = 'TAB'
#' @param f.header Logical. Has id.file header?
#' @param f.comment Comment character of the files. Default "#"
#' @param copy.his Should .HIS files copied to the output folders?
#' @param out.folder Parent folder for the output.
#' @param verbose Should message be displayed?
#' @param get.max If TRUE, and there is only one type of ID, the max values will return
#' @return A list of data.table
#' @export
his_from_case_old <- function(
  case.list = "", # path to list of cases to work with
  sobek.project = "", # path to Sobek Project folder
  param = 1, # index of the paramter to get data from .HIS file
  qID = NULL, # path to list of sobek_ids for Discharge
  wID = NULL, # path to list of sobek_ids for Water level
  lID = NULL, # path to list of sobek_ids for Lateral
  sID = NULL, # path to list of sobek_ids for Structure
  mID = NULL, # path to list of sobek_ids for Measstation
  f.sep = "\t", # seperation of node list
  f.header = FALSE,
  f.comment = "#",
  copy.his = FALSE, # copy .HIS file to destination folder?
  out.folder = ".",
  verbose = TRUE,
  get.max = FALSE # instead of get the time series, get the max values only
) {
  wk_dir <- getwd()
  str_as_factor <- default.stringsAsFactors()
  options("stringsAsFactors" = FALSE)
  if (!dir.exists(out.folder)) {
    dir.create(out.folder, recursive = TRUE, showWarnings = FALSE)
  }
  # check SOBEK project
  sobek_cmt <- paste(sobek.project, "caselist.cmt", sep = "/")
  if (!file.exists(sobek_cmt)) {
    stop("Sobek Caselist.cmt does not exist! Check sobek.project Folder")
  }
  case.list <- unlist(case.list)
  if (is.character(case.list) && length(case.list) == 1 && file.exists(case.list)){
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
  } else {
    if (is.vector(case.list)){
      clist <- data.table(matrix(case.list,
                                 nrow = length(case.list),
                                 ncol = 1),
                          stringAsFactors = FALSE)
    } else {
      if (is.data.frame(case.list)){
        clist <- data.table(case.list)
      } else {
        stop("case.list must be a path to file, a list, or a data.frame")
      }
    }
  }
  # first column in the clist is case_name
  colnames(clist)[1] <- "case_name"
  if (ncol(clist) >= 2){
    if(!"case_folder" %in% colnames(clist)) colnames(clist)[2] <- "case_folder"
  }
  col_case_folder <- ifelse("case_folder" %in% colnames(clist),
                            TRUE, FALSE
  )
  # reading SOBEK caselist.cmt
  sobek_clist <- data.table::fread(file = sobek_cmt,
                                   header = FALSE,
                                   sep = " ",
                                   quote = "'",
                                   stringsAsFactors = FALSE,
                                   blank.lines.skip = TRUE,
                                   col.names = c("case_number", "case_name")
  )
  sobek_clist[, case_name := gsub('"', '', case_name, fixed = TRUE)]
  clist$case_number <- lapply(clist$case_name,
                              FUN = .get_case_number, case.list = sobek_clist)
  clist[tolower(case_name) == 'work', case_number := 'work']
  # check if clist contain a column for destination
  if ("case_folder" %in% colnames(clist)){
    clist$case_dest_folder <- lapply(clist$case_folder,
                                     FUN = function(x, pth = out.folder) {
                                       paste(pth, x, sep = "/")
                                     }
    )
    clist$case_sobek_folder <- lapply(clist$case_number,
                                      FUN = function(x, pth = sobek.project) {
                                        paste(pth, x, sep = "/")
                                      }
    )
  } else {
    clist$case_sobek_folder <- lapply(clist$case_number,
                                      FUN = function(x, pth = sobek.project){
                                        paste(pth, x, sep = "/")
                                      }
    )
    clist$case_dest_folder <- lapply(clist$case_number,
                                     FUN = function(x, pth = out.folder){
                                       paste(pth, x, sep = "/")
                                     }
    )
  }
  # copy .HIS file to destination folders
  if (copy.his) {
    for (i in clist$case_number) {
      if (!is.na(i)) {
        if (!dir.exists(clist$case_folder[clist$case_number == i])){
          dir.create(clist$case_folder[clist$case_number == i],
                     recursive = TRUE)
        }
        src_folder <- clist$case_sobek_folder[clist$case_number == i]
        dest_folder <- paste(clist$case_folder[clist$case_number == i], "/",
                             sep = ""
        )
        if (!is.null(wID)) {
          file.copy(
            from = paste(src_folder, "CALCPNT.HIS", sep = "/"),
            to = dest_folder
          )
        }
        if (!is.null(qID)) {
          file.copy(
            from = paste(src_folder, "REACHSEG.HIS", sep = "/"),
            to = dest_folder
          )
        }
        if (!is.null(lID)) {
          file.copy(
            from = paste(src_folder, "QLAT.HIS", sep = "/"),
            to = dest_folder
          )
        }
        if (!is.null(sID)) {
          file.copy(
            from = paste(src_folder, "STRUC.HIS", sep = "/"),
            to = dest_folder
          )
        }
        if (!is.null(mID)) {
          file.copy(
            from = paste(src_folder, "MEASSTAT.HIS", sep = "/"),
            to = dest_folder
          )
        }
      }
    }
  }
  wID_res <- list()
  qID_res <- list()
  lID_res <- list()
  sID_res <- list()
  mID_res <- list()
  result <- list()
  # j <- 1
  for (i in clist$case_number) {
    if (verbose) {
      print(paste(
        "Working with Case:",
        clist$case_name[clist$case_number == i]
      ))
    }
    # if case.name found in the caselist.cmt
    if (!is.na(i)) {
      his_folder <- ifelse(copy.his,
                           clist$case_dest_folder[clist$case_number == i],
                           clist$case_sobek_folder[clist$case_number == i]
      )
      if (!is.null(wID)) {
        if(length(wID)==1 && file.exists(wID)){
          tmp <- his_from_file(
            his.file = paste(his_folder, "CALCPNT.HIS", sep = "/"),
            id.file = wID[[1]],
            param = param
          )
          tmp$case <- clist$case_name[clist$case_number == i]
          wID_res[[i]] <- tmp
        } else {
          if (is.vector(wID)){
            tmp <- his_from_list(
              his.file = paste(his_folder, "CALCPNT.HIS", sep = "/"),
              id.list = unlist(wID),
              param = param
            )
            tmp$case <- clist$case_name[clist$case_number == i]
            wID_res[[i]] <- tmp
          }
        }
      }

      # get data for Discharge
      if (!is.null(qID)) {
        if(length(qID)==1 && file.exists(qID)){
          tmp <- his_from_file(
            his.file = paste(his_folder, "REACHSEG.HIS", sep = "/"),
            id.file = qID[[1]],
            param = param
          )
          tmp$case <- clist$case_name[clist$case_number == i]
          qID_res[[i]] <- tmp
        } else {
          if (is.vector(qID)){
            tmp <- his_from_list(
              his.file = paste(his_folder, "REACHSEG.HIS", sep = "/"),
              id.list = unlist(qID),
              param = param
            )
            tmp$case <- clist$case_name[clist$case_number == i]
            qID_res[[i]] <- tmp
          }
        }
      } # end of if (!is.na(i))

      # get data for Laterals
      if (!is.null(lID)) {
        if(length(lID)==1 && file.exists(lID)){
          tmp <- his_from_file(
            his.file = paste(his_folder, "QLAT.HIS", sep = "/"),
            id.file = lID[[1]],
            param = param
          )
          tmp$case <- clist$case_name[clist$case_number == i]
          lID_res[[i]] <- tmp
        } else {
          if (is.vector(lID)){
            tmp <- his_from_list(
              his.file = paste(his_folder, "QLAT.HIS", sep = "/"),
              id.list = unlist(lID),
              param = param
            )
            tmp$case <- clist$case_name[clist$case_number == i]
            lID_res[[i]] <- tmp
          }
        }
      }

      # get data for Structure
      if (!is.null(sID)) {
        if(length(sID)==1 && file.exists(sID)){
          tmp <- his_from_file(
            his.file = paste(his_folder, "STRUC.HIS", sep = "/"),
            id.file = sID[[1]],
            param = param
          )
          tmp$case <- clist$case_name[clist$case_number == i]
          sID_res[[i]] <- tmp
        } else {
          if (is.vector(sID)){
            tmp <- his_from_list(
              his.file = paste(his_folder, "STRUC.HIS", sep = "/"),
              id.list = unlist(sID),
              param = param
            )
            tmp$case <- clist$case_name[clist$case_number == i]
            sID_res[[i]] <- tmp
          }
        }
      }

      # get data for Measstation
      if (!is.null(mID)) {
        if(length(mID)==1 && file.exists(mID)){
          tmp <- his_from_file(
            his.file = paste(his_folder, "MEASSTAT.HIS", sep = "/"),
            id.file = mID[[1]],
            param = param
          )
          tmp$case <- clist$case_name[clist$case_number == i]
          mID_res[[i]] <- tmp
        } else {
          if (is.vector(mID)){
            tmp <- his_from_list(
              his.file = paste(his_folder, "MEASSTAT.HIS", sep = "/"),
              id.list = unlist(mID),
              param = param
            )
            tmp$case <- clist$case_name[clist$case_number == i]
            mID_res[[i]] <- tmp
          }
        }
      }
    }
  }
  # rm(tmp)

  if (length(wID_res) > 0) {
    result$waterlevel <- data.table::rbindlist(wID_res)
    # colnames(result$waterlevel) <-
    rm(wID_res)
  }
  if (length(qID_res) > 0) {
    result$discharge <- data.table::rbindlist(qID_res)
    rm(qID_res)
  }
  if (length(lID_res) > 0) {
    result$lateral <- data.table::rbindlist(lID_res)
    rm(lID_res)
  }
  if (length(sID_res) > 0) {
    result$structure <- data.table::rbindlist(sID_res)
    rm(sID_res)
  }
  if (length(mID_res) > 0) {
    result$measstation <- data.table::rbindlist(mID_res)
    rm(mID_res)
  }
  options("stringsAsFactors" = str_as_factor)
  setwd(wk_dir)
  if (length(result) == 1) {
    result <- result[[1]]
    # get the max values if get.max == TRUE, only for one type of ID
    if (isTRUE(get.max)){
      result <- result[, lapply(.SD, max, na.rm = TRUE), by = case]
    }
  }
  return(result)
}



