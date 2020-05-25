#' Run Sobek Simulation for single case
#' @param case.name Name of the Case
#' @param sobek.project Path to Sobek Project Folder
#' @param sobek.path Path to Sobek Program Folder (ex. d:/so21302)
#' @param overwrite Should simulation result overwrite back to Case folder?
#' @param clear.temp Should temp directory to be clear?
#' @export
sobek_sim_new <- function(case.name = NULL,
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
