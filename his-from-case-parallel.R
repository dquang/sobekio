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
his_from_case <- function(
  case.list = "", # list of cases to work with
  sobek.project = "", # path to Sobek Project folder
  param = 1, # index of the paramter to get data from .HIS file
  verbose = TRUE,
  get.max = FALSE, # instead of get the time series, get the max values only
  ...
  ) {
  f_args <- match.call(expand.dots = FALSE)
  
  id_types <- c('wID', 'qID', 'lID', 'sID', 'mID', 'latID')
  id_type <- id_types[sapply(
    id_types,
    function(name, x) hasName(x = x, name = name), # for reversing hasName function
    f_args$...)
    ]
  his_fname <- switch(
    id_type,
    wID = 'calcpnt.his',
    qID = 'reachseg.his',
    lID = 'qlat.his',
    latID = 'qlat.his',
    sID = 'struc.his',
    mID = 'measstat.his'
  )
  
  if (length(eval(f_args$...[[id_type]])) * length(case.list) > 100){
    print('Load his_from_case_parallel')
    result <- his_from_case_parallel(
      case.list = case.list,
      sobek.project = sobek.project,
      param = param,
      verbose = verbose,
      get.max = get.max,
      ...
    )
  } else{
    print('Load his_from_case_serial')
    result <- his_from_case_serial(
      case.list = case.list,
      sobek.project = sobek.project,
      param = param,
      verbose = verbose,
      get.max = get.max,
      ...
    )
  }
}



his_from_case_serial <- function(
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
            from = paste(src_folder, "calcpnt.his", sep = "/"),
            to = dest_folder
          )
        }
        if (!is.null(qID)) {
          file.copy(
            from = paste(src_folder, "reachseg.his", sep = "/"),
            to = dest_folder
          )
        }
        if (!is.null(lID)) {
          file.copy(
            from = paste(src_folder, "lateral.his", sep = "/"),
            to = dest_folder
          )
        }
        if (!is.null(sID)) {
          file.copy(
            from = paste(src_folder, "struc.his", sep = "/"),
            to = dest_folder
          )
        }
        if (!is.null(mID)) {
          file.copy(
            from = paste(src_folder, "measstat.his", sep = "/"),
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
            his.file = paste(his_folder, "calcpnt.his", sep = "/"),
            id.file = wID[[1]],
            param = param
          )
          tmp$case <- clist$case_name[clist$case_number == i]
          wID_res[[i]] <- tmp
        } else {
          if (is.vector(wID)){
            tmp <- his_from_list(
              his.file = paste(his_folder, "calcpnt.his", sep = "/"),
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
            his.file = paste(his_folder, "reachseg.his", sep = "/"),
            id.file = qID[[1]],
            param = param
          )
          tmp$case <- clist$case_name[clist$case_number == i]
          qID_res[[i]] <- tmp
        } else {
          if (is.vector(qID)){
            tmp <- his_from_list(
              his.file = paste(his_folder, "reachseg.his", sep = "/"),
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
            his.file = paste(his_folder, "qlat.his", sep = "/"),
            id.file = lID[[1]],
            param = param
          )
          tmp$case <- clist$case_name[clist$case_number == i]
          lID_res[[i]] <- tmp
        } else {
          if (is.vector(lID)){
            tmp <- his_from_list(
              his.file = paste(his_folder, "qlat.his", sep = "/"),
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
            his.file = paste(his_folder, "struc.his", sep = "/"),
            id.file = sID[[1]],
            param = param
          )
          tmp$case <- clist$case_name[clist$case_number == i]
          sID_res[[i]] <- tmp
        } else {
          if (is.vector(sID)){
            tmp <- his_from_list(
              his.file = paste(his_folder, "struc.his", sep = "/"),
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
            his.file = paste(his_folder, "measstat.his", sep = "/"),
            id.file = mID[[1]],
            param = param
          )
          tmp$case <- clist$case_name[clist$case_number == i]
          mID_res[[i]] <- tmp
        } else {
          if (is.vector(mID)){
            tmp <- his_from_list(
              his.file = paste(his_folder, "measstat.his", sep = "/"),
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


his_from_case_parallel <- function(
  case.list = "", # list of cases to work with
  sobek.project = "", # path to Sobek Project folder
  param = 1, # index of the paramter to get data from .HIS file
  verbose = TRUE,
  get.max = FALSE, # instead of get the time series, get the max values only
  ...
) {
  on.exit(stopImplicitCluster())
  f_args <- as.list(match.call(expand.dots = FALSE))
  check_args <- TRUE %in% grepl('mID|qID|sID|wID|lID|latID', 
                                  names(f_args$...))
  if (length(f_args$...) > 2) stop("there are too many extra parameters")
  if (check_args == FALSE){
    stop('One of mID|qID|sID|wID|lID|latID parameter has to be given')
  } else{
    if (length(f_args$...) == 2) {
      if (!hasName(f_args$..., 'id_name')) cat(
        'Too many parameters. It should be, i.e:
      \nmID = c("p_koeln", "p_mainz"),\nid_name = c("Köln", "Mainz")'
      )
      # check if id and id_name has same length
      stopifnot(length(eval(f_args$...[[1]])) == 
                  length(eval(f_args$...[[2]]))
      )
    }
  }
  # check SOBEK project
  sobek_cmt <- paste(sobek.project, "caselist.cmt", sep = "/")
  if (!file.exists(sobek_cmt)) {
    stop("Sobek Caselist.cmt does not exist! Check sobek.project Folder")
  }
  case.list <- unlist(case.list)
  ncase <- length(case.list)
  clist <- data.table(case_name = rep("", ncase), 
                      # case_number = rep(_NA_Character, ncase),
                      his_file = rep("", ncase)
                      )
  clist$case_name <- unlist(case.list)
  # first column in the clist is case_name
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
  # sobek_clist[, case_name := gsub('"', '', case_name, fixed = TRUE)]
  clist <- merge(clist, sobek_clist, by = 'case_name', all.x = TRUE, 
                 sort = FALSE)
  clist[tolower(case_name) == 'work', case_number := 'work']
  id_types <- c('wID', 'qID', 'lID', 'sID', 'mID', 'latID')
  id_type <- id_types[sapply(
    id_types,
    function(name, x) hasName(x = x, name = name), # for reversing hasName function
    f_args$...)
    ]
  his_fname <- switch(
    id_type,
    wID = 'calcpnt.his',
    qID = 'reachseg.his',
    lID = 'qlat.his',
    latID = 'qlat.his',
    sID = 'struc.his',
    mID = 'measstat.his'
  )
  clist[, his_file := paste(sobek.project, case_number, his_fname, sep = "\\")]
  id_list <- eval(f_args$...[[id_type]])
  registerDoParallel(detectCores() - 1)
  result <- foreach(i_case = 1:ncase, .combine = rbind) %dopar% {
    # print(eval(f_args$...[[id_type]]))
    i <- clist$case_number[i_case]
    # id_list <- eval(f_args$...[[id_type]])
    his_file <- clist$his_file[i_case]
    # if case.name found in the caselist.cmt
    if (!is.na(i)) {
      tmp <- his_from_list(
            id.list = id_list,
            his.file = his_file,
            param = param
        )
      tmp$case <- clist$case_name[i_case]
    }
    tmp # return result to the foreach
  }
  if (hasName(f_args$..., "id_name")) {
    colnames(result) <- c('ts', eval(f_args$...$id_name), 'case')
  }
  # get the max values if get.max == TRUE, only for one type of ID
  if (isTRUE(get.max)){
    result <- result[, lapply(.SD, max, na.rm = TRUE), by = case]
  }
  return(result)
}
