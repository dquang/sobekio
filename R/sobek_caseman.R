#' Sobek Simulation for single case
#'
#' @param case.name Name of the Case
#' @param sobek.project Path to Sobek Project Folder
#' @param sobek.path Path to Sobek Program Folder (ex. d:/so21302)
#' @param fix.data Path to the fixed data
#' @param begin Starting time of simulation. If given, the start time in settings.dat will be temporally replaced by this value. This is useful for testing simulations.
#' @param end Ending time of simulation
#' @param overwrite Should simulation result overwrite back to Case folder?
#' @param clear.temp Should temporary folder be cleared after the simulation?
#' @export
#' @return a list of simulation summary and changed files
sobek_sim <- function(case.name,
                      sobek.project,
                      sobek.path,
                      fix.data = NULL,
                      begin = NULL,
                      end = NULL,
                      overwrite = TRUE,
                      clear.temp = TRUE
) {
  case.name <- unlist(case.name)
  stopifnot(length(case.name) == 1)
  sobek.path <- normalizePath(sobek.path)
  if (!is.null(fix.data)) fix.data <- normalizePath(fix.data)
  sobek.project <- normalizePath(sobek.project)
  c_folder <- get_case_folder(case.list = case.name, sobek.project = sobek.project)
  stopifnot(!is.na(c_folder))
  c_number <- basename(c_folder)
  wkd <- getwd()
  on.exit(setwd(wkd))
# prepare WORK and CMTWORK folders ----------------------------------------
  tmp_folder <- tempdir(check = TRUE)
  tmp_folder <- tempfile(pattern = "SoSIM_", tmp_folder)
  if (!dir.exists(tmp_folder)) dir.create(tmp_folder)
  wk_folder_del <- tmp_folder
  on.exit(unlink(wk_folder_del, recursive = TRUE))
  wk_folder <- paste0(tmp_folder, "\\WORK")
  cmt_folder <- paste0(tmp_folder, "\\CMTWORK")
  # copy files to WORK folder
  init_sbk_work(case.folder = c_folder, tmp.folder = tmp_folder)
  # back up original settings.dat and casedesc.cmt
  org_set <- tempfile(tmpdir = tmp_folder)
  org_desc <- tempfile(tmpdir = tmp_folder)
  file.copy(from = paste0(tmp_folder, "\\WORK\\settings.dat"),
            to = org_set, overwrite = TRUE)
  file.copy(from = paste0(tmp_folder, "\\WORK\\casedesc.cmt"),
            to = org_desc, overwrite = TRUE)
  init_sbk_cmt(case.folder = c_folder, tmp.folder = tmp_folder,
               sobek.path = sobek.path, fix.data = fix.data,
               type = "simulate")
  setwd(cmt_folder)
  # change simulation BEGIN and END in settings.dat
  change_settings(begin = begin, end = end,
                  dat = file.path(tmp_folder, "WORK\\settings.dat")
                  )
# simulate ----------------------------------------------------------------
  cmd <- paste0(sobek.path, "\\programs\\simulate.exe simulate.ini")
  if (interactive()) {
    cat("Waiting for Sobek Simulation.exe. DO NOT terminate R or run any other commands...\n")
    cat("If you need to do something else with R, please open another session\n")
  }
  else{
    cat(
      "Running simulation for case:\n",
      case.name,
      "\nPlease DO NOT CLOSE this windows until the simulation has been done.\n"
    )
  }
  system(command = cmd, wait = TRUE)
# checking result and saving changes--------------------------------------------
  ret <- XML::xmlToList("simulate_log.xml")[[2]]
  changes <- NA
  sim_success <- isTRUE(tolower(ret[["Summary"]][["Succesfull"]]) == "true")
  if (sim_success) {
    # update casedesc.cmt after simulation
    # have to do this because the complexity of path to fixed files
    desc_cmt <- fread(file = org_desc,
                   header = FALSE, sep = "\n", encoding = "UTF-8")
    c_status <- desc_cmt[grep("^[A-Z0-9]{1, } \\d{1,}", V1)]
    c_status[, c("V2", "V3") := tstrsplit(V1, " ")]
    c_status[!grepl("map|view", V2, ignore.case = TRUE), V3 := 1]
    c_status[grepl("map|view", V2, ignore.case = TRUE), V3 := 2]
    c_status[, V1 := paste(V2, V3)]
    desc_cmt[grep("^[A-Z0-9]{1, } \\d{1,}", V1), V1 := c_status$V1]
    fwrite(
      desc_cmt,
      file = file.path(wk_folder, "casedesc.cmt"),
      sep = "\n", col.names = FALSE, row.names = FALSE, quote = FALSE
    )
    setwd(tmp_folder)
    # restore settings.dat
    file.copy(from = org_set, overwrite = TRUE,
              to = paste0(tmp_folder, "\\WORK\\settings.dat"),
              )
    unlink(org_set)
    unlink(org_desc)
    setwd(tmp_folder)
    if (overwrite) {
      # get case folder again, try to avoid if SOBEK GUI changed things in between
      c_folder <- get_case_folder(case.name, sobek.project)
      if (is.na(c_folder))
        stop("Case folder is not found any more for case: ", case.name)
      changed_wk <- save_changed_files(orig = c_folder, work = wk_folder)
      changed_others <- save_changed_files(
        orig = sobek.project, work = tmp_folder, except.dirs = "work|cmtwork")
      changed_files <- c(changed_wk, changed_others)
    }
    cat("done.\n")
  }
  else {
    cat("Simulation was not successful\n")
    par_err_chk <- isTRUE(ret[["ErrorSourceMsg"]] == "parsen")
    if (par_err_chk) {
      parsen_msg <- fread(file = "parsen.msg",
                          blank.lines.skip = FALSE,
                          sep = "\n", header = FALSE)
      # cleaning before return
      setwd(wkd)
      parsen_tmp <- tempfile(tempdir())
      file.copy(from = "parsen.msg", to = parsen_tmp, overwrite = TRUE)
      par_err <- parsen_msg[grepl('Error', V1, ignore.case = TRUE)]
      cat(par_err$V1, "\n")
      cat("Check file: ", parsen_tmp, " for more details.\n")
      if (!interactive()) {
        system(
          paste("notepad.exe", parsen_tmp),
          wait = FALSE,
          invisible = FALSE
        )
      }
      ret <- parsen_msg
    }
    else{
      setwd(wkd)
      if (!interactive()) cat(ret, "\n")
    }
    setwd(wkd)
  }
  setwd(wkd)
  invisible(list(summary = ret, changes = changed_files))
}


#' Parallel simulating for many cases
#'
#' This function divides a list of cases n threads and simulate them parallely
#'
#' @param case.list List of cases
#' @param sobek.project Path to sobek project
#' @param sobek.path Path to sobek installation folder
#' @param n Number of threads (Default 2)
#' @param wait Number of seconds to wait before starting simulation on other cores
#' This parameter is to avoid sobek to start too quick and cause error
#' @export
par_sim_cases <- function(case.list, sobek.project, sobek.path,
                          n = 2, wait = 2) {
  n_cores <- parallel::detectCores()
  if (n > n_cores) {
    cat('There are only ', n_cores, ' cores available')
    cat('Do not set n more than this.')
    stop('n > number of cores')
  }
  case.list <- unlist(case.list)
  cmd_header <- data.table(cmds = c(
    'library(sobekio)',
    paste0("sobek.project <- '", sobek.project, "'"),
    paste0("sobek.path <- '", sobek.path, "'")
  ))
  cmd_footer <- data.table(
    cmds = c(
      "cat('Please check if all cases were simulated. Then press Enter to exit...\\n')",
      "invisible(scan('stdin', character(), nlines = 1, n = 1, quiet = TRUE))"
    )
  )
  n_cases <- length(case.list)
  if (n_cases < n) n <- n_cases
  n_cases_per_thread <- ceiling(n_cases / n) # number of cases in one file
  for (i in seq.int(n)) {
    case_begin <- 1 + (i - 1) * n_cases_per_thread
    case_end <- i * n_cases_per_thread
    if (case_end > n_cases) case_end <- n_cases
    cases_i <- case.list[case_begin:case_end]
    files_i <- tempfile(pattern = 'r_sim', fileext = '.R')
    cmd_i <- paste0("sobek_sim(case.name = '", cases_i, "', ",
                    'sobek.project = sobek.project, sobek.path = sobek.path)'
    )
    fwrite(cmd_header, file = files_i, col.names = FALSE, sep = '\n')
    fwrite(list(cmd_i), file = files_i, col.names = FALSE, sep = '\n',
           append = TRUE)
    fwrite(cmd_footer, file = files_i, col.names = FALSE, sep = '\n',
           append = TRUE)
    r_script <- file.path(R.home(), 'bin/Rscript.exe')
    cmd <- paste0(r_script, ' --vanilla "', files_i, '"')
    system(command = cmd, wait = FALSE, invisible = FALSE)
    if (i > 1) {
      cat('Wait for some seconds before starting next R session')
      Sys.sleep(wait)
    }
  }
}


#' View Sobek Results
#'
#' @inheritParams sobek_sim
#' @param fix.data Path to fixed data of the project. This parameter is useful when the project folder is moved but path to the fixed data in the casedesc.cmt was not updated. However, it was not tested. It is generally better not to use fixed data for a sobek project, otherwise it will cause many problems.
#' @export
sobek_view <- function(case.name, sobek.project, sobek.path, fix.data = NULL) {
  sobek.path <- gsub("/", "\\\\", sobek.path)
  sobek.project <- gsub("/", "\\\\", sobek.project)
  c_folder <- get_case_folder(case.list = case.name, sobek.project = sobek.project)
  stopifnot(!is.na(c_folder))
  c_number <- basename(c_folder)
  if (is.null(fix.data)) {
    fix.data <- paste0(sobek.project, "\\fixed")
    if (!file.exists(fix.data))
      fix.data <- paste0(sobek.path, "\\fixed")
  } else {
    fix.data <- normalizePath(fix.data)
  }
  tmp_folder <- tempdir(check = TRUE)
  tmp_folder <- tempfile(pattern = "Sbk_", tmpdir = tmp_folder)
  dir.create(tmp_folder)
  wkd <- getwd()
  on.exit(setwd(wkd))
  on.exit(unlink(tmp_folder, recursive = TRUE))
  cmt_folder <- file.path(tmp_folder, "CMTWORK", fsep = "\\")
  init_sbk_work(case.folder = c_folder, tmp.folder = tmp_folder)
  init_sbk_cmt(case.folder = c_folder, tmp.folder = tmp_folder,
               sobek.path = sobek.path,
               fix.data = fix.data, type = "view")
  # Viewing Result ----------------------------------------------------------
  setwd(cmt_folder)
  cmd <- paste0(sobek.path, "\\programs\\prepmapp.exe prepmapp.ini")
  cat(
    "Viewing result for case:\n",
    case.name,
    "\nPlease wait until the Viewer has been done.\n"
  )
  system(cmd, wait = TRUE, invisible = FALSE)
  setwd(wkd)
}


#' Edit Sobek case
#'
#' @inheritParams sobek_view
#' @export
sobek_edit <- function(case.name, sobek.project, sobek.path, fix.data = NULL) {
  sobek.path <- gsub("/", "\\\\", sobek.path)
  sobek.project <- gsub("/", "\\\\", sobek.project)
  c_folder <- get_case_folder(case.list = case.name, sobek.project = sobek.project)
  stopifnot(!is.na(c_folder))
  c_number <- basename(c_folder)
  if (is.null(fix.data)) {
    fix.data <- paste0(sobek.project, "\\fixed")
    if (!file.exists(fix.data))
      fix.data <- paste0(sobek.path, "\\fixed")
  } else {
    fix.data <- normalizePath(fix.data)
  }
  tmp_folder <- tempdir(check = TRUE)
  tmp_folder <- tempfile(pattern = "Sbk_", tmpdir = tmp_folder)
  dir.create(tmp_folder)
  wkd <- getwd()
  on.exit(setwd(wkd))
  on.exit(unlink(tmp_folder, recursive = TRUE))
  cmt_folder <- file.path(tmp_folder, "CMTWORK", fsep = "\\")
  wk_folder <- file.path(tmp_folder, "WORK", fsep = "\\")
  init_sbk_work(case.folder = c_folder, tmp.folder = tmp_folder)
  org_desc <- tempfile(tmpdir = tmp_folder, pattern = "tmp_cdesc_")
  file.copy(from = paste0(tmp_folder, "\\WORK\\casedesc.cmt"),
            to = org_desc, overwrite = TRUE)
  init_sbk_cmt(case.folder = c_folder, tmp.folder = tmp_folder,
               sobek.path = sobek.path,
               fix.data = fix.data, type = "edit")
  # Call schemat.exe for editing------------------------------------------------
  setwd(cmt_folder)
  cmd <- paste0(sobek.path, "\\programs\\schemat.exe schemat.ini")
  if (interactive()) {
    cat(
      "Editing case:\n",
      case.name,
      "\nPlease wait until the editing has been done.\n"
    )
  } else {
    cat(
      "Editing case:\n",
      case.name,
      "\nPlease DO NOT CLOSE this windows until the editing has been done.\n"
    )
  }
  system(cmd, wait = TRUE, invisible = FALSE)
  setwd(wkd)
  # save changed files
  # save work folder to case folder
  changed_wk <- save_changed_files(orig = c_folder, work = wk_folder)
  changed_others <- save_changed_files(
    orig = sobek.project, work = tmp_folder,
    except.dirs = "work|cmtwork", except.files = basename(org_desc))
  changed_files <- c(changed_wk, changed_others)
  changed_files <- changed_files[!grepl("casedesc.cmt$", changed_files,
                                       ignore.case = TRUE)]
  # save casedesc.cmt
  desc_cmt <- fread(file = org_desc,
                    header = FALSE, sep = "\n", encoding = "UTF-8")
  if (length(changed_files) > 0) {
    # if there was something changed, we change the case status
    c_status <- desc_cmt[grep("^[A-Z0-9]{1, } \\d{1,}", V1)]
    c_status[, c("V2", "V3") := tstrsplit(V1, " ")]
    c_status[!grepl("map|view", V2, ignore.case = TRUE), V3 := 1]
    c_status[grepl("map|view", V2, ignore.case = TRUE), V3 := 4]
    c_status[, V1 := paste(V2, V3)]
    desc_cmt[grep("^[A-Z0-9]{1, } \\d{1,}", V1), V1 := c_status$V1]
    changed_files <- c(changed_files, paste0(c_folder, "\\casedesc.cmt"))
  }
  fwrite(
    desc_cmt,
    file = paste0(c_folder, "\\casedesc.cmt"),
    sep = "\n", col.names = FALSE, row.names = FALSE, quote = FALSE
  )
  unlink(org_desc)
  invisible(changed_files)
}
