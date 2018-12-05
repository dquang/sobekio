################################################################################
#' Export data for nodes/reaches for one SOBEK Case
#'
#' For all .HIS files subfolders.
#' @param case.name Name of the SOBEK case to work with\cr
#' @param qID Path to file containing list of reach IDs for struct.his\cr
#' @param wID IDs for calcpnt.his\cr
#' @param lID IDs for lateral.his\cr
#' @param sID IDs for struct.his\cr
#' @param mID IDs for measstat.his\cr
#' @param f.sep Seperator of the id.file, default = '\\t'
#' @param f.header Logical. Has id.file header?
#' @param param Index/Name of the Paramter to get the data, default = 1
#' @param verbose Should message be displayed?
#' @return A list of data.table
#' @export
his_single_case <- function(
  case.name = NULL, # Name of the Sobek case
  sobek.project = NULL, # path to Sobek Project folder
  qID = NULL, # path to list of sobek_ids for Discharge
  wID = NULL, # path to list of sobek_ids for Water level
  lID = NULL, # path to list of sobek_ids for Lateral
  sID = NULL, # path to list of sobek_ids for Structure
  mID = NULL, # path to list of sobek_ids for Measstation
  f.sep = "\t", # seperation of node list
  f.header = FALSE,
  param = 1, # index of the paramter to get data from .HIS file
  # out.folder = ".",
  verbose = FALSE) {
  str_as_factor <- default.stringsAsFactors()
  options("stringsAsFactors" = FALSE)
  if (is.null(case.name) || class(as.character(case.name)) != "character"){
    stop("case.name must be given and coercible to charaters")
  }
  if (!is.vector(case.name) || length(case.name) > 1) stop("case.name must be a string")
  # if (!dir.exists(out.folder)) {
  # 	dir.create(out.folder, recursive = FALSE, showWarnings = FALSE)
  # }
  # setwd(out.folder)
  # check SOBEK project
  sobek_cmt <- paste(sobek.project, "caselist.cmt", sep = "/")
  if (!file.exists(sobek_cmt)) {
    stop("Sobek Caselist.cmt does not exist!")
  }
  # reading SOBEK caselist.cmt
  sobek_clist <- data.table::fread(
    file = sobek_cmt,
    header = FALSE,
    sep = " ",
    quote = "'",
    stringsAsFactors = FALSE,
    blank.lines.skip = TRUE,
    col.names = c("case_number", "case_name")
  )
  case_number <- .get_case_number(case.name, sobek_clist)
  his_folder <- paste(sobek.project, case_number, sep = "/")
  # extracting data
  # wID_res <- list()
  # qID_res <- list()
  # lID_res <- list()
  # sID_res <- list()
  # mID_res <- list()
  result  <- list()
  if (verbose) print(paste("Working with Case:", case_name))
  if (!is.na(case_number)) {
    if (!is.null(wID)) {
      if(length(wID)==1 && file.exists(wID)){
        wID_res <- his_from_file(
          his.file = paste(his_folder, "calcpnt.his", sep = "/"),
          id.file = wID[[1]],
          param = param
        )
        wID_res$case <- case.name
        result$waterlevel <- wID_res
      } else {
        if (is.vector(wID)){
          wID_res <- his_from_list(
            his.file = paste(his_folder, "calcpnt.his", sep = "/"),
            id.list = unlist(wID),
            param = param
          )
          wID_res$case <- case.name
          result$waterlevel <- wID_res
        }
      }
    }

    # get data for Discharge
    if (!is.null(qID)) {
      if(length(qID)==1 && file.exists(qID)){
        qID_res <- his_from_file(
          his.file = paste(his_folder, "reachseg.his", sep = "/"),
          id.file = qID[[1]],
          param = param
        )
        qID_res$case <- case.name
        result$discharge <- qID_res
      } else {
        if (is.vector(qID)){
          qID_res <- his_from_list(
            his.file = paste(his_folder, "reachseg.his", sep = "/"),
            id.list = unlist(qID),
            param = param
          )
          qID_res$case <- case.name
          result$discharge <- qID_res
        }
      }
    }

      # get data for Laterals
      if (!is.null(lID)) {
        if(length(lID)==1 && file.exists(lID)){
          lID_res <- his_from_file(
            his.file = paste(his_folder, "lateral.his", sep = "/"),
            id.file = lID[[1]],
            param = param
          )
          lID_res$case <- case.name
          result$lateral <- lID_res
        } else {
          if (is.vector(lID)){
            lID_res <- his_from_list(
              his.file = paste(his_folder, "lateral.his", sep = "/"),
              id.list = unlist(lID),
              param = param
            )
            lID_res$case <- case.name
            result$lateral <- lID_res
          }
        }
      }

      # get data for Structure
      if (!is.null(sID)) {
        if(length(sID)==1 && file.exists(sID)){
          sID_res <- his_from_file(
            his.file = paste(his_folder, "struc.his", sep = "/"),
            id.file = sID[[1]],
            param = param
          )
          sID_res$case <- case.name
          result$structure <- sID_res
        } else {
          if (is.vector(sID)){
            sID_res <- his_from_list(
              his.file = paste(his_folder, "struc.his", sep = "/"),
              id.list = unlist(sID),
              param = param
            )
            sID_res$case <- case.name
            result$structure <- sID_res
          }
        }
      }

      # get data for Measstation
      if (!is.null(mID)) {
        if(length(mID)==1 && file.exists(mID)){
          mID_res <- his_from_file(
            his.file = paste(his_folder, "measstat.his", sep = "/"),
            id.file = mID[[1]],
            param = param
          )
          mID_res$case <- case.name
          result$measstation <- mID_res
        } else {
          if (is.vector(mID)){
            mID_res <- his_from_list(
              his.file = paste(his_folder, "measstat.his", sep = "/"),
              id.list = unlist(mID),
              param = param
            )
            mID_res$case <- case.name
            result$measstation <- mID_res
          }
        }
      }
    }
  options("stringsAsFactors" = str_as_factor)
  return(result)
}
