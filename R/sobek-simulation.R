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
  clist <- fread(file = paste(sobek.project, "caselist.cmt", sep = "/"),
                 header = FALSE,
                 sep = " ",
                 quote = "'",
                 col.names = c("case_number", "case_name")
                 )
  c_number <- .get_case_number(case.name, clist)
  c_folder <- paste(sobek.project, c_number, sep = "/")
  wkd <- getwd()
  # copy relative file to working folder
  setwd(sobek.path)
  wk_folder <- format(Sys.time(), format = "%d%m%Y_%H%M%S")
  dir.create(wk_folder)
  prj_files <- dir(sobek.project, full.names = TRUE)
  prj_files <- prj_files[!grepl("/[0-9]{1,}$", prj_files)]
  file.copy(from = prj_files,
            to = wk_folder,
            recursive = TRUE)
  # copy case folder to work folder
  file.copy(from = c_folder,
            to = wk_folder,
            recursive = TRUE
            )
  wk_folder_del <- wk_folder
  wk_folder <- paste(wk_folder, "work", sep = "/")
  if(!dir.exists(wk_folder)) dir.create(wk_folder)
  file.copy(from = dir(c_folder, full.names = TRUE,
                       recursive = TRUE,
                       all.files = TRUE,
                       include.dirs = TRUE,
                       no.. = TRUE
                       ),
            to = wk_folder)
  file.copy(from = paste(sobek.path, "programs/simulate.ini", sep = "/"),
            to = wk_folder)
  # fwrite(list())
  setwd(wk_folder)
  file.rename(from = "status.cas",
            to = "@STATUS.CAS")
  # file.remove("status.cas")
  cmd <- paste("cmd.exe /c ", sobek.path, "/programs/simulate.exe simulate.ini", sep = "")
  print("Waiting for Sobek Simulation.exe. DO NOT terminate R or run any other commands...")
  print("If you need to do something else with R, please open another session")
  system(command = cmd, wait = TRUE)
  so_res <- XML::xmlToList("simulate_log.xml")[[2]]
  if (so_res[["Summary"]][["Succesfull"]] == "false"){
    print("Simulation was not successful")
    if (so_res[["Summary"]][["ErrorSourceMsg"]]=="parsen"){
      parsen_msg <- fread(file = "parsen.msg",
                          sep = "\n", header = F)
      parsen_msg <- parsen_msg[grep("Error", V1),]
      # cleaning before return
      setwd(wkd)
      unlink(wk_folder_del, recursive = TRUE)
      return(parsen_msg)
    } else{
      print("Error message: ")
      print(so_res[["Summary"]][["ErrorSourceMsg"]])
    }
    setwd(wkd)
    unlink(wk_folder_del, recursive = TRUE)
  } else {
      file.rename(from = "@status.cas",
                  to = "STATUS.CAS")

      # have to write: checking simulation status and display errors if any

      # update casedesc.cmt after simulation
      cdesc <- data.table::fread(file = "casedesc.cmt", header = F, sep = "\n"
                                 )
      cdesc[V1 %like% "^PLUVIUS1"] <- "PLUVIUS1 1"
      cdesc[V1 %like% "^MAPPER1"] <- "MAPPER1 2"
      cdesc[V1 %like% "^VIEW1"] <- "VIEW1 2"
      cdesc[V1 %like% "^VIEW2"] <- "VIEW2 2"
      cdesc[V1 %like% "\\NETTER.DLF",
            V1:=gsub("\\#\\NETTER.DLF",
                     paste("\\", c_number,"\\NETTER.DLF", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\ODS2XLS.DLF",
            V1:=gsub("\\#\\ODS2XLS.DLF",
                     paste("\\", c_number,"\\ODS2XLS.DLF", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\TABLES.LST",
            V1:=gsub("\\#\\TABLES.LST",
                     paste("\\", c_number,"\\TABLES.LST", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\PARSEN.DMP",
            V1:=gsub("\\#\\PARSEN.DMP",
                     paste("\\", c_number,"\\PARSEN.DMP", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\PARSEN.MSG",
            V1:=gsub("\\#\\PARSEN.MSG",
                     paste("\\", c_number,"\\PARSEN.MSG", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\SOBEK.LOG",
            V1:=gsub("\\#\\SOBEK.LOG",
                     paste("\\", c_number,"\\SOBEK.LOG", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\STRUC.HIS",
            V1:=gsub("\\#\\STRUC.HIS",
                     paste("\\", c_number,"\\STRUC.HIS", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\STRUC.HIA",
            V1:=gsub("\\#\\STRUC.HIA",
                     paste("\\", c_number,"\\STRUC.HIA", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\STRUCDIM.HIS",
            V1:=gsub("\\#\\STRUCDIM.HIS",
                     paste("\\", c_number,"\\STRUCDIM.HIS", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\STRUCDIM.HIA",
            V1:=gsub("\\#\\STRUCDIM.HIA",
                     paste("\\", c_number,"\\STRUCDIM.HIA", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\REACHSEG.HIS",
            V1:=gsub("\\#\\REACHSEG.HIS",
                     paste("\\", c_number,"\\REACHSEG.HIS", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\REACHSEG.HIA",
            V1:=gsub("\\#\\REACHSEG.HIA",
                     paste("\\", c_number,"\\REACHSEG.HIA", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\REACHDIM.HIS",
            V1:=gsub("\\#\\REACHDIM.HIS",
                     paste("\\", c_number,"\\REACHDIM.HIS", sep = ""),
                     V1, fixed = TRUE)
            ]
      cdesc[V1 %like% "\\REACHDIM.HIA",
            V1:=gsub("\\#\\REACHDIM.HIA",
                     paste("\\", c_number,"\\REACHDIM.HIA", sep = ""),
                     V1, fixed = TRUE)
            ]
      fwrite(cdesc, file = "casedesc.cmt", col.names = F, row.names = F)
      # ask_cp <- readline("copy result back to the case folder? (y/n): ")

      if (overwrite){
        files_list <- list.files(".", pattern = "*.*", all.files = TRUE,
                                 no.. = TRUE)
        file.copy(from = files_list,
                  to = c_folder,
                  recursive = F,
                  overwrite = TRUE)
        print("done.")
      }
      # cleaning
      setwd(sobek.path)
      if (clear.temp) unlink(wk_folder_del, recursive = TRUE)
      setwd(wkd)
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
  clist <- fread(file = paste(sobek.project, "caselist.cmt", sep = "\\"),
                 header = FALSE,
                 sep = " ",
                 quote = "'",
                 col.names = c("case_number", "case_name")
  )
  c_number <- .get_case_number(case.name, clist)
  c_folder <- paste(sobek.project, c_number, sep = "\\")
  wkd <- getwd()
  on.exit(setwd(wkd))
  # copy relative file to working folder
  setwd(sobek.path)
  tmp_folder <- format(Sys.time(), format = "%d%m%Y_%H%M%S")
  dir.create(tmp_folder)
  prj_files <- dir(sobek.project, full.names = TRUE)
  prj_files <- prj_files[!grepl("[\\/][0-9]{1,}$", prj_files)]
  file.copy(from = prj_files,
            to = tmp_folder,
            recursive = TRUE)
  # copy case folder to work folder
  file.copy(from = c_folder,
            to = tmp_folder,
            recursive = TRUE
  )
  wk_folder_del <- paste(sobek.path, tmp_folder, sep = "\\")
  cmt_folder <- paste(sobek.path, tmp_folder, "CMTWORK", sep = "\\")
  wk_folder <- paste(sobek.path, tmp_folder, "WORK", sep = "\\")
  if (!dir.exists(wk_folder)) dir.create(wk_folder)
  if (!dir.exists(wk_folder)) dir.create(cmt_folder)
  cmt_files <- list.files(paste(system.file(package = 'sobekio'),
                                'network_view', sep = '/'),
                          full.names = TRUE,
                          pattern = "\\.[:alnum:]*",
                          no.. = TRUE)
  for (i in cmt_files){
    # print(i)

    f1 <- fread(file = i, sep = "\n", quote = "", header = F)
    f1[, V1 := gsub("_replace_this_", sobek.path, V1,
                 fixed = TRUE)]
    f2 <- paste(cmt_folder, basename(i), sep = "\\")
    fwrite(x = f1, file = f2, quote = F, row.names = F, col.names = F)
  }
  file.copy(from = paste(c_folder, "casedesc.cmt", sep = "\\"),
            to = cmt_folder)
  file.copy(from = paste(sobek.path, "programs\\simulate.ini", sep = "\\"),
            to = wk_folder)
  file.copy(from = dir(c_folder, full.names = TRUE,
                       recursive = TRUE,
                       all.files = TRUE,
                       include.dirs = TRUE,
                       no.. = TRUE
  ),
  to = wk_folder)
  file.copy(from = paste(sobek.path, "programs\\simulate.ini", sep = "\\"),
            to = wk_folder)
  setwd(cmt_folder)
  if (!external){
    cmd <- paste("cmd.exe /c ",
                 sobek.path.c,
                 "\\programs\\netter.exe ntrpluv.ini ",
                 '..\\WORK\\NETWORK.NTW',
                 sep = "")
    print("Waiting for Sobek Simulation.exe. DO NOT terminate R or run any other commands...")
    print("If you need to do something else with R, please open another session")
    # print(paste('Please using NETTER to open the network file in folder: ',
    #       wk_folder, 'for Viewing'))
    system(command = cmd, wait = TRUE)
    # removing temp. data
    setwd(wkd)
    if (clear.temp) {
      unlink(wk_folder_del, recursive = TRUE)
    }
  } else {
    cmd0 <- paste('cd ', cmt_folder)
    cmd1 <- paste("call ",
                  sobek.path,
                 "\\programs\\netter.exe ",
                 cmt_folder,
                 '\\ntrpluv.ini ',
                 wk_folder,
                 '\\NETWORK.NTW',
                 sep = "")
    cmd2 <- paste('cd ', sobek.path)
    cmd3 <- 'echo the temporary folder was not deleted!'
    cmd4 <- paste('del ', sobek.path, '\\', tmp_folder, '.cmd', sep = '')
    # check the nchar(tmp_folder) to make sure not to delete anything incorrectly
    if (nchar(tmp_folder) > 0 && clear.temp) {
      cmd3 <- paste('rmdir ', sobek.path, "\\", tmp_folder,
                    ' /s /q', sep = '')
    }
    cmd.file <- data.table(
      V1=list(
        '@echo off',
        cmd0,
        'echo DO NOT close this windows until you have finished with NETTER',
        cmd1, cmd2, cmd3, cmd4,
        'pause') # does not work
                           )
    fwrite(cmd.file, file = paste(sobek.path, '\\', tmp_folder, '.cmd', sep = ''),
           col.names = F, quote = F)
    cmd <- paste('cmd.exe /c ',  sobek.path, '\\', tmp_folder, '.cmd', sep = '')
    system(command = cmd, wait = FALSE, invisible = FALSE)
    print('If something went wrong with CMD, try the function again with option \'external = FALSE\'')
  }
}


#' #' Run Sobek Simulation for single case
#' #' @param case.name Name of the Case
#' #' @param sobek.project Path to Sobek Project Folder
#' #' @param sobek.path Path to Sobek Program Folder (ex. d:/so21302)
#' #' @param clear.temp Should temp directory to be clear?
#' #' @param external If TRUE, R will use a CMD session to start NETTER,
#' #' and the current R session is available to user immediately.
#' #' @export
#' sobek_view_result <- function(case.name = NULL,
#'                        sobek.project = NULL,
#'                        sobek.path = NULL,
#'                        clear.temp = TRUE,
#'                        external = TRUE){
#'   sobek.path <- gsub("/", "\\\\", sobek.path)
#'   sobek.project <- gsub("/", "\\\\", sobek.project)
#'   clist <- fread(file = paste(sobek.project, "caselist.cmt", sep = "\\"),
#'                  header = FALSE,
#'                  sep = " ",
#'                  quote = "'",
#'                  col.names = c("case_number", "case_name")
#'   )
#'   c_number <- .get_case_number(case.name, clist)
#'   c_folder <- paste(sobek.project, c_number, sep = "\\")
#'   wkd <- getwd()
#'   on.exit(setwd(wkd))
#'   # copy relative file to working folder
#'   setwd(sobek.path)
#'   tmp_folder <- format(Sys.time(), format = "%d%m%Y_%H%M%S")
#'   dir.create(tmp_folder)
#'   prj_files <- dir(sobek.project, full.names = TRUE)
#'   prj_files <- prj_files[!grepl("[\\/][0-9]{1,}$", prj_files)]
#'   file.copy(from = prj_files,
#'             to = tmp_folder,
#'             recursive = TRUE)
#'   # copy case folder to work folder
#'   file.copy(from = c_folder,
#'             to = tmp_folder,
#'             recursive = TRUE
#'   )
#'   wk_folder_del <- paste(sobek.path, tmp_folder, sep = "\\")
#'   cmt_folder <- paste(sobek.path, tmp_folder, "CMTWORK", sep = "\\")
#'   wk_folder <- paste(sobek.path, tmp_folder, "WORK", sep = "\\")
#'   if (!dir.exists(wk_folder)) dir.create(wk_folder)
#'   if (!dir.exists(wk_folder)) dir.create(cmt_folder)
#'   cmt_files <- list.files(paste(system.file(package = 'sobekio'),
#'                                 'result_view', sep = '/'),
#'                           full.names = TRUE,
#'                           pattern = "\\.[:alnum:]*",
#'                           no.. = TRUE)
#'   for (i in cmt_files){
#'     f1 <- fread(file = i,
#'                 sep = "\n",
#'                 quote = "",
#'                 header = F,
#'                 col.names = c('V1'))
#'     f1[, V1 := gsub("_replace_this_", sobek.path, V1,
#'                     fixed = TRUE)]
#'
#'     f1 <- fread(file = i, sep = "\n", quote = "", header = F)
#'     f1[, V1 := gsub("_case_folder_", c_folder, V1,
#'                     fixed = TRUE)]
#'     f1[, V1 := gsub("_work_folder_", wk_folder, V1,
#'                     fixed = TRUE)]
#'     f1[, V1 := gsub("_so_folder_", sobek.path, V1,
#'                     fixed = TRUE)]
#'     f2 <- paste(cmt_folder, basename(i), sep = "\\")
#'     fwrite(x = f1, file = f2, quote = F, row.names = F, col.names = F)
#'   }
#'   file.copy(from = paste(c_folder, "casedesc.cmt", sep = "\\"),
#'             to = cmt_folder)
#'   file.copy(from = paste(sobek.path, "programs\\simulate.ini", sep = "\\"),
#'             to = wk_folder)
#'   file.copy(from = dir(c_folder, full.names = TRUE,
#'                        recursive = TRUE,
#'                        all.files = TRUE,
#'                        include.dirs = TRUE,
#'                        no.. = TRUE
#'   ),
#'   to = wk_folder)
#'   file.copy(from = paste(sobek.path, "programs\\simulate.ini", sep = "\\"),
#'             to = wk_folder)
#'   setwd(cmt_folder)
#'   if (!external){
#'     cmd <- paste("cmd.exe /c ",
#'                  sobek.path.c,
#'                  "\\programs\\netter.exe ntrpluvr.ini ",
#'                  '..\\WORK\\NETWORK.NTW',
#'                  sep = "")
#'     print("Waiting for Sobek Simulation.exe. DO NOT terminate R or run any other commands...")
#'     print("If you need to do something else with R, please open another session")
#'     # print(paste('Please using NETTER to open the network file in folder: ',
#'     #       wk_folder, 'for Viewing'))
#'     system(command = cmd, wait = TRUE)
#'     # removing temp. data
#'     setwd(wkd)
#'     if (clear.temp) {
#'       unlink(wk_folder_del, recursive = TRUE)
#'     }
#'   } else {
#'     cmd0 <- paste('cd ', cmt_folder)
#'     cmd1 <- paste("call ",
#'                   sobek.path,
#'                   "\\programs\\netter.exe ",
#'                   cmt_folder,
#'                   '\\ntrpluvr.ini ',
#'                   wk_folder,
#'                   '\\NETWORK.NTW',
#'                   sep = "")
#'     cmd2 <- paste('cd ', sobek.path)
#'     cmd3 <- 'echo the temporary folder was not deleted!'
#'     cmd4 <- paste('del ', sobek.path, '\\', tmp_folder, '.cmd', sep = '')
#'     # check the nchar(tmp_folder) to make sure not to delete anything incorrectly
#'     if (nchar(tmp_folder) > 0 && clear.temp) {
#'       cmd3 <- paste('rmdir ', sobek.path, "\\", tmp_folder,
#'                     ' /s /q', sep = '')
#'     }
#'     cmd.file <- data.table(
#'       V1=list(
#'         '@echo off',
#'         cmd0,
#'         'echo DO NOT close this windows until you have finished with NETTER',
#'         cmd1, cmd2, cmd3, cmd4,
#'         'pause') # does not work
#'     )
#'     fwrite(cmd.file, file = paste(sobek.path, '\\', tmp_folder, '.cmd', sep = ''),
#'            col.names = F, quote = F)
#'     cmd <- paste('cmd.exe /c ',  sobek.path, '\\', tmp_folder, '.cmd', sep = '')
#'     system(command = cmd, wait = FALSE, invisible = FALSE)
#'     print('If something went wrong with CMD, try the function again with option \'external = FALSE\'')
#'   }
#' }
