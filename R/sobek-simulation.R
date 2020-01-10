#' Run Sobek Simulation for single case
#' @param case.name Name of the Case
#' @param sobek.project Path to Sobek Project Folder
#' @param sobek.path Path to Sobek Program Folder (ex. d:/so21302)
#' @param overwrite Should simulation result overwrite back to Case folder?
#' @param clear.temp Should temp directory to be clear?
#' @export
sobek_sim <- function(case.name = NULL,
                      sobek.project = NULL,
                      sobek.path = NULL,
                      overwrite = TRUE,
                      clear.temp = TRUE
                      ){
  sobek.path <- gsub("/", "\\\\", sobek.path)
  sobek.project <- gsub("/", "\\\\", sobek.project)
  sobek_cmt <- file_path(name = 'caselist.cmt', path = sobek.project)
  clist <- fread(file = sobek_cmt,
                 header = FALSE,
                 sep = " ",
                 quote = "'",
                 col.names = c("case_number", "case_name")
                 )
  clist[, case_name := gsub('"', '', case_name, fixed = TRUE)]
  setkey(clist, case_name)
  if (grepl("^\\d{1,}$", case.name)) {
    c_number <- case.name
  } else{
    c_number <- clist[case.name, case_number]
    stopifnot(!is.na(c_number))
  }
  c_folder <- paste(sobek.project, c_number, sep = "/")
  wkd <- getwd()
  on.exit(setwd(wkd))
  # copy relative file to working folder
  setwd(sobek.path)
  tmp_folder <- format(Sys.time(), format = "%d%m%Y_%H%M%S")
  tmp_folder <- paste(tmp_folder, floor(runif(1, 1, 10000)), sep = '_')
  # 03.06.2019 09:58 Change tmp_folder to absolut path,
  # for copying all data back (also restart files)
  tmp_folder <- paste(sobek.path, tmp_folder, sep = "\\")
  wk_folder_del <- tmp_folder
  dir.create(tmp_folder)
  # on.exit(unlink(wk_folder_del, recursive = TRUE))
  # on.exit(unlink(paste(sobek.path, wk_folder, sep ="/"), recursive = TRUE))
  prj_files <- dir(sobek.project, full.names = TRUE)
  prj_files <- prj_files[!grepl("/[0-9]{1,}$", prj_files)]
  prj_files <- prj_files[!grepl("/WORK|/CMTWORK|/NEWSTART|\\.his$",
                                prj_files, ignore.case = TRUE)]
  # prj_files <- prj_files[!grepl("/CMTWORK", prj_files, ignore.case = TRUE)]
  file.copy(from = prj_files,
            overwrite = TRUE,
            to = tmp_folder,
            recursive = TRUE)
  # copy case folder to work folder
  c_folder_ohne_his <- dir(c_folder, full.names = TRUE)
  # removing old .HIS files to save HDD space
  c_folder_ohne_his <- c_folder_ohne_his[!grepl(pattern = '\\.his$',
                                               x = c_folder_ohne_his,
                                               ignore.case = TRUE
                                               )
                                         ]
  c_folder_in_tmp <- paste(tmp_folder, c_number, sep = "\\")
  if (!dir.exists(c_folder_in_tmp)) dir.create(c_folder_in_tmp)
  file.copy(from = c_folder_ohne_his,
            overwrite = TRUE,
            to = c_folder_in_tmp,
            recursive = TRUE
            )
  wk_folder <- paste(tmp_folder, "WORK", sep = "\\")
  if (!dir.exists(wk_folder)) dir.create(wk_folder)
  file.copy(from = dir(c_folder, full.names = TRUE,
                       recursive = TRUE,
                       all.files = TRUE,
                       include.dirs = TRUE,
                       no.. = TRUE
                       ),
            overwrite = TRUE,
            to = wk_folder)
  sim_ini_f <- paste(system.file(package = 'sobekio'),
                     'simulate/simulate.ini', sep = '/')
  sim_ini <- fread(sim_ini_f, sep = "\t", header = FALSE)
  sim_ini[, V1 := gsub('_PROGRAM_DIR_', sobek.path, V1, fixed = TRUE)]
  setwd(wk_folder)
  fwrite(sim_ini, file = 'simulate.ini', col.names = FALSE,
         row.names = FALSE, quote = FALSE)
  cmd <- paste("cmd.exe /c ", sobek.path, "/programs/simulate.exe simulate.ini", sep = "")
  if (interactive()) {
    cat("Waiting for Sobek Simulation.exe. DO NOT terminate R or run any other commands...\n")
    cat("If you need to do something else with R, please open another session\n")
  } else{
    cat(
      "Running simulation for case:\n",
      clist[case_number == c_number, case_name],
       "\nPlease DO NOT CLOSE this windows until the simulation has been done.\n"
      )
  }
  system(command = cmd, wait = TRUE)
  so_res <- XML::xmlToList("simulate_log.xml")[[2]]
  if (tolower(so_res[["Summary"]][["Succesfull"]]) == "false") {
    print("Simulation was not successful")
    if (so_res[["Summary"]][["ErrorSourceMsg"]] == "parsen") {
      parsen_msg <- fread(file = paste(wk_folder, "parsen.msg", sep = "\\"),
                          blank.lines.skip = FALSE,
                          sep = "\n", header = FALSE)
      # cleaning before return
      setwd(wkd)
      parsen_tmp <- paste(tempdir(),"\\parsen.msg", sep = "")
      file.copy(from = paste(wk_folder, "parsen.msg", sep = "\\"),
                to = parsen_tmp, overwrite = TRUE)
      unlink(wk_folder_del, recursive = TRUE)
      if (!interactive()) {
        print(parsen_msg[grepl('Error', V1, ignore.case = TRUE)])
        system(
          paste("notepad.exe", parsen_tmp),
          wait = FALSE,
          invisible = FALSE
        )
      }
      return(parsen_msg)
    } else{
      setwd(wkd)
      unlink(wk_folder_del, recursive = TRUE)
      if (!interactive()) print(so_res[["Summary"]])
      return(so_res[["Summary"]])
    }
    setwd(wkd)
    unlink(wk_folder_del, recursive = TRUE)
  } else {
      # update casedesc.cmt after simulation
      cdesc <- data.table::fread(file = "casedesc.cmt", header = FALSE, sep = "\n"
                                 )
      cdesc[V1 %like% "^PLUVIUS1"] <- "PLUVIUS1 1"
      cdesc[V1 %like% "^MAPPER1"] <- "MAPPER1 2"
      cdesc[V1 %like% "^VIEW1"] <- "VIEW1 2"
      cdesc[V1 %like% "^VIEW2"] <- "VIEW2 2"
      cdesc[V1 %like% "\\NETTER.DLF",
            V1 := gsub("\\#\\NETTER.DLF",
                     paste("\\", c_number,"\\NETTER.DLF", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\ODS2XLS.DLF",
            V1 := gsub("\\#\\ODS2XLS.DLF",
                     paste("\\", c_number,"\\ODS2XLS.DLF", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\TABLES.LST",
            V1 := gsub("\\#\\TABLES.LST",
                     paste("\\", c_number,"\\TABLES.LST", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\PARSEN.DMP",
            V1 := gsub("\\#\\PARSEN.DMP",
                     paste("\\", c_number,"\\PARSEN.DMP", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\PARSEN.MSG",
            V1 := gsub("\\#\\PARSEN.MSG",
                     paste("\\", c_number,"\\PARSEN.MSG", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\SOBEK.LOG",
            V1 := gsub("\\#\\SOBEK.LOG",
                     paste("\\", c_number,"\\SOBEK.LOG", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\STRUC.HIS",
            V1 := gsub("\\#\\STRUC.HIS",
                     paste("\\", c_number,"\\STRUC.HIS", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\STRUC.HIA",
            V1 := gsub("\\#\\STRUC.HIA",
                     paste("\\", c_number,"\\STRUC.HIA", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\STRUCDIM.HIS",
            V1 := gsub("\\#\\STRUCDIM.HIS",
                     paste("\\", c_number,"\\STRUCDIM.HIS", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\STRUCDIM.HIA",
            V1 := gsub("\\#\\STRUCDIM.HIA",
                     paste("\\", c_number,"\\STRUCDIM.HIA", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\REACHSEG.HIS",
            V1 := gsub("\\#\\REACHSEG.HIS",
                     paste("\\", c_number,"\\REACHSEG.HIS", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\REACHSEG.HIA",
            V1 := gsub("\\#\\REACHSEG.HIA",
                     paste("\\", c_number,"\\REACHSEG.HIA", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\REACHDIM.HIS",
            V1 := gsub("\\#\\REACHDIM.HIS",
                     paste("\\", c_number,"\\REACHDIM.HIS", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\REACHDIM.HIA",
            V1 := gsub("\\#\\REACHDIM.HIA",
                     paste("\\", c_number,"\\REACHDIM.HIA", sep = ""),
                     V1, fixed = TRUE)
            ]
      fwrite(cdesc, file = "casedesc.cmt", col.names = FALSE, row.names = FALSE,
             quote = FALSE)
      if (overwrite) {
        # copy work folder back to case folder
        files_list <- list.files(".", recursive = TRUE, all.files = TRUE,
                                 no.. = TRUE)
        file.copy(from = files_list,
                  to = c_folder,
                  recursive = TRUE,
                  overwrite = TRUE)
        # copy restart folder back to restart folder
        if (dir.exists("../restart")) {
          files_list <- list.files("../restart",
                                   recursive = TRUE,
                                   all.files = TRUE,
                                   full.names = TRUE,
                                   no.. = TRUE)
          file.copy(from = files_list,
                    to = paste(sobek.project, 'restart', sep = "/") ,
                    recursive = TRUE,
                    overwrite = TRUE)
        }
        cat("done.\n")
      }
      # cleaning
      setwd(sobek.path)
      if (clear.temp) unlink(wk_folder_del, recursive = TRUE)
      setwd(wkd)
  }
}


#' Open Sobek case for editing
#'
#' @param case.name Name of the Case
#' @param sobek.project Path to Sobek Project Folder
#' @param sobek.path Path to Sobek Program Folder (ex. d:/so21302)
#' @param clear.temp Should temp directory to be clear?
#' @param external If TRUE, R will use a CMD session to start NETTER,
#' and the current R session is available to user immediately.
#' @export
sobek_edit <- function(case.name = NULL,
                      sobek.project = NULL,
                      sobek.path = NULL,
                      clear.temp = TRUE,
                      external = TRUE){
  sobek.path <- gsub("/", "\\\\", sobek.path)
  sobek.project <- gsub("/", "\\\\", sobek.project)
  sobek_cmt <- file_path(name = 'caselist.cmt', path = sobek.project)
  clist <- fread(file = sobek_cmt,
                 header = FALSE,
                 sep = " ",
                 quote = "'",
                 col.names = c("case_number", "case_name")
  )
  clist[, case_name := gsub('"', '', case_name, fixed = TRUE)]
  setkey(clist, case_name)
  if (grepl("^\\d{1,}$", case.name)) {
    c_number <- case.name
  } else{
    c_number <- clist[case.name, case_number]
    stopifnot(!is.na(c_number))
  }
  c_folder <- paste(sobek.project, c_number, sep = "\\")
  wkd <- getwd()
  on.exit(setwd(wkd))
  # copy relative file to working folder
  setwd(sobek.path)
  tmp_folder <- format(Sys.time(), format = "%d%m%Y_%H%M%S")
  tmp_folder <- paste(tmp_folder, floor(runif(1, 1, 10000)), sep = '_')
  tmp_folder <- file.path(sobek.path, tmp_folder)
  wk_folder_del <- tmp_folder
  dir.create(tmp_folder)
  prj_files <- dir(sobek.project, full.names = TRUE)
  prj_files <- prj_files[!grepl("[\\/][0-9]{1,}", prj_files,
                                ignore.case = TRUE)]
  # for editing, the folder NEWSTART and RESTART are not needed
  prj_files <- prj_files[!grepl("/WORK|/CMTWORK|NEWSTART|RESTART",
                                prj_files, ignore.case = TRUE)]
  file.copy(from = prj_files,
            to = tmp_folder,
            overwrite = TRUE,
            recursive = TRUE)
  cmt_folder <- file.path(tmp_folder, "CMTWORK")
  wk_folder <- file.path(tmp_folder, "WORK")
  if (!dir.exists(wk_folder)) dir.create(wk_folder)
  if (!dir.exists(cmt_folder)) dir.create(cmt_folder)
  # copy case folder to work folder
  c_folder_ohne_his <- dir(c_folder, full.names = TRUE)
  c_folder_ohne_his <- c_folder_ohne_his[!grepl(pattern = '\\.his$',
                                                x = c_folder_ohne_his,
                                                ignore.case = TRUE)]
  file.copy(from = c_folder_ohne_his,  overwrite = TRUE,  to = wk_folder)
  cmt_files <- list.files(paste(system.file(package = 'sobekio'),
                                'network_edit', sep = '/'),
                          full.names = TRUE,
                          pattern = "\\.[:alnum:]*",
                          no.. = TRUE)
  # FIXME: find and copy "fixed" folder
  for (i in cmt_files) {
    f1 <- fread(file = i, sep = "\n", quote = "", header = FALSE)
    f1[, V1 := gsub("_WORK_DIR_", wk_folder, V1, fixed = TRUE)]
    f1[, V1 := gsub("_PROGRAM_DIR_", sobek.path, V1, fixed = TRUE)]
    f2 <- paste(cmt_folder, basename(i), sep = "\\")
    fwrite(x = f1, file = f2, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
  setwd(cmt_folder)
  if (!external) {
    #<FIXME: copy back only changed files!>
    cmd <- paste("cmd.exe /c ",
                 sobek.path,
                 "\\programs\\netter.exe ntrpluv.ini ",
                 '..\\WORK\\NETWORK.NTW',
                 sep = "")
    if (interactive()) {
      cat("Waiting for NETTER. DO NOT terminate R or run any other commands...\n")
      cat("If you need to do something else with R, please open another session\n")
    } else {
      cat("You are editing case:\n",
          clist[case_number == c_number, case_name],
          "\nPlease DO NOT CLOSE this windows manually.")
    }
    system(command = cmd, wait = TRUE)
    # removing temp. data
    setwd(tmp_folder)
    cat('Copy files back to case folder...\n')
    file.copy(from = list.files("WORK",
                                all.files = TRUE,
                                recursive = TRUE,
                                full.names = TRUE),
              to = c_folder, overwrite = TRUE)
    file.copy(from = list.files('FIXED',
                                all.files = TRUE,
                                recursive = TRUE,
                                full.names = TRUE),
              to = paste(sobek.project, 'FIXED', sep = "\\"),
              overwrite = TRUE)
    if (clear.temp) {
      cat('Clearing temporary files...')
      unlink(wk_folder_del, recursive = TRUE)
    }
  } else {
    cmd_f <- paste(sobek.path, '\\', basename(tmp_folder),
                   '.cmd', sep = '')
    cmd0 <- paste('cd ', cmt_folder)
    cmd1 <- paste("call ",
                  sobek.path,
                 "\\programs\\netter.exe ",
                 cmt_folder,
                 '\\ntrpluv.ini ',
                 wk_folder,
                 '\\NETWORK.NTW',
                 sep = "")
    # copy work folder back to the case folder, only newer files will be copied
    cmd11 <- paste("call xcopy /C /Y /E /H /R",
                  wk_folder,
                  # '\\*.* ',
                  c_folder,
                  # '\\',
                  sep = " ")
    # copy fixed folder back to the fixed folder, only newer files will be copied
    cmd12 <- paste("call xcopy /C /Y /E /H /R",
                   tmp_folder,
                   '\\FIXED ',
                   sobek.project,
                   '\\FIXED',
                   sep = "")
    cmd2 <- paste('cd ', sobek.path)
    cmd3 <- 'echo the temporary folder was not deleted!'
    cmd4 <- paste('del ', cmd_f)
    # check the nchar(tmp_folder) to make sure not to delete anything incorrectly
    if (nchar(tmp_folder) > 0 && clear.temp) {
      cmd3 <- paste('rmdir ', tmp_folder, "\\",
                    ' /s /q', sep = '')
    }
    cmd.file <- data.table(
      V1 = list(
        '@echo off',
        cmd0,
        paste('echo You are viewing case:',
              clist[case_number == c_number, case_name]),
        'echo DO NOT close this windows until you have finished with NETTER',
        cmd1,
        'echo Copying edited data back to project and clearing. Please wait...',
        cmd11, cmd12, cmd2, cmd3, cmd4,
        'call cmd /c "echo Done."'#,
        # 'PAUSE' # does not work
        )
                           )
    fwrite(cmd.file, file = cmd_f,
           col.names = FALSE, quote = FALSE)
    cmd <- paste('cmd.exe /c',  cmd_f)
    system(command = cmd, wait = FALSE, invisible = FALSE)
    print('If something went wrong with CMD, try the function again with option \'external = FALSE\'')
  }
}


#' Run Sobek Simulation for single case
#' @param case.name Name of the Case
#' @param sobek.project Path to Sobek Project Folder
#' @param sobek.path Path to Sobek Program Folder (ex. d:/so21302)
#' @param clear.temp Should temp directory to be clear?
#' @param external If TRUE, R will use a CMD session to start NETTER,
#' and the current R session is available to user immediately.
#' @export
sobek_view <- function(case.name = NULL,
                       sobek.project = NULL,
                       sobek.path = NULL,
                       clear.temp = TRUE,
                       external = TRUE){
  sobek.path <- gsub("/", "\\\\", sobek.path)
  sobek.project <- gsub("/", "\\\\", sobek.project)
  sobek_cmt <- file_path(name = 'caselist.cmt', path = sobek.project)
  clist <- fread(file = sobek_cmt,
                 header = FALSE,
                 sep = " ",
                 quote = "'",
                 col.names = c("case_number", "case_name")
  )
  clist[, case_name := gsub('"', '', case_name, fixed = TRUE)]
  setkey(clist, case_name)
  if (grepl("^\\d{1,}$", case.name)) {
    c_number <- case.name
  } else{
    c_number <- clist[case.name, case_number]
    stopifnot(!is.na(c_number))
  }
  c_folder <- paste(sobek.project, c_number, sep = "\\")
  wkd <- getwd()
  on.exit(setwd(wkd))
  # copy relative file to working folder
  setwd(sobek.path)
  tmp_folder <- format(Sys.time(), format = "%d%m%Y_%H%M%S")
  tmp_folder <- paste(tmp_folder, floor(runif(1, 1, 10000)), sep = '_')
  dir.create(tmp_folder)
  cmt_folder <- paste(sobek.path, tmp_folder, 'CMTWORK', sep = "\\")
  work_folder <- paste(sobek.path, tmp_folder, 'WORK', sep = "\\")
  dir.create(cmt_folder)
  dir.create(work_folder)
  file.copy(paste(c_folder, 'settings.dat', sep = "\\"), work_folder,
            overwrite = TRUE)
  file.copy(paste(c_folder, 'casedesc.cmt', sep = "\\"),
            cmt_folder,
            overwrite = TRUE)
  # copy case folder to work folder
  wk_folder_del <- paste(sobek.path, tmp_folder, sep = "\\")
  cmt_files <- list.files(paste(system.file(package = 'sobekio'),
                                'result_view/CMTWORK', sep = '/'),
                          full.names = TRUE,
                          no.. = TRUE)
  work_files <- list.files(paste(system.file(package = 'sobekio'),
                                 'result_view/WORK', sep = '/'),
                           full.names = TRUE,
                           no.. = TRUE)
  for (i in cmt_files) {
    f1 <- fread(file = i, sep = "\n", quote = "", header = FALSE)
    f1[, V1 := gsub("_CASE_DIR_", c_folder, V1, fixed = TRUE)]
    f1[, V1 := gsub("_PROGRAM_DIR_", sobek.path,  V1, fixed = TRUE)]
    f1[, V1 := gsub("_PROJECT_DIR_", sobek.project, V1, fixed = TRUE)]
    f2 <- paste(cmt_folder, basename(i), sep = "\\")
    fwrite(x = f1, file = f2, quote = FALSE, row.names = FALSE,
           col.names = FALSE)
  }
  for (i in work_files) {
    f1 <- fread(file = i, sep = "\n", quote = "", header = FALSE)
    f1[, V1 := gsub("_CASE_DIR_", c_folder, V1, fixed = TRUE)]
    f1[, V1 := gsub("_PROGRAM_DIR_", sobek.path,  V1, fixed = TRUE)]
    f1[, V1 := gsub("_PROJECT_DIR_", sobek.project, V1, fixed = TRUE)]
    f2 <- paste(work_folder, basename(i), sep = "\\")
    fwrite(x = f1, file = f2, quote = FALSE, row.names = FALSE,
           col.names = FALSE)
  }
  if (!external) {
    setwd(cmt_folder)
    cmd <- paste("cmd.exe /c ",
                 sobek.path,
                 "\\programs\\netter.exe ",
                 cmt_folder, '\\ntrpluvr.ini ',
                 cmt_folder, '\\netter1.ntc ',
                 sep = "")
    if (interactive()) {
      print("Waiting for NETTER. DO NOT terminate R or run any other commands...")
      print("If you need to do something else with R, please open another session")
    } else{
      cat(
        "You are viewing results of the case case:\n",
        clist[case_number == c_number, case_name],
        "\nPlease DO NOT CLOSE this windows manually."
      )
    }
    system(command = cmd, wait = TRUE)
    # removing temp. data
    setwd(wkd)
    cat('cleaning temporary data...')
    if (clear.temp) {
      unlink(wk_folder_del, recursive = TRUE)
    }
  } else {
    cmd0 <- paste('cd ', cmt_folder)
    cmd1 <- paste("call ",
                  sobek.path,
                  "\\programs\\netter.exe ",
                  cmt_folder, '\\ntrpluvr.ini ',
                  cmt_folder, '\\netter1.ntc ',
                  sep = "")
    cmd2 <- paste('cd ', sobek.path)
    cmd3 <- 'echo the temporary folder was not deleted!'
    cmd4 <- paste('del ', sobek.path, '\\', tmp_folder, '.cmd', sep = '')
    # check the nchar(tmp_folder) to make sure not to delete anything incorrectly
    if (nchar(tmp_folder) > 2 && clear.temp) {
      cmd3 <- paste('rmdir ', sobek.path, "\\", tmp_folder,
                    ' /s /q', sep = '')
    }
    cmd.file <- data.table(
      V1 = list(
        '@echo off',
        cmd0,
        paste('echo You are viewing case:',
              clist[case_number == c_number, case_name]),
        'echo DO NOT close this windows until you have finished with NETTER',
        cmd1,
        'echo Clearing...Please wait.',
        cmd2, cmd3, cmd4,
        'pause') # does not work
    )
    fwrite(cmd.file, file = paste(sobek.path, '\\', tmp_folder, '.cmd', sep = ''),
           col.names = FALSE, quote = FALSE)
    cmd <- paste('cmd.exe /c ',  sobek.path, '\\', tmp_folder, '.cmd', sep = '')
    system(command = cmd, wait = FALSE, invisible = FALSE)
    print('If something went wrong with CMD, try the function again with option \'external = FALSE\'')
  }
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
      "invisible(scan('stdin', character(), nlines = 1, quiet = TRUE))"
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
