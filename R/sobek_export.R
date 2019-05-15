#' Export list of cases from a sobek project to another location
#' @param case.list Path to file with case list
#' @param sobek.project Path to current sobek project
#' @param dest Destination folder
#' @param copy.his Should .HIS file be copied? Default TRUE
#' @param his.only Copy only HIS/HIA files for saving time?
#' @export
sobek_export <- function(case.list,
                         sobek.project,
                         dest,
                         copy.his = TRUE,
                         his.only = FALSE
                         ){
  if (!dir.exists(dest)) dir.create(dest)
  if (is.character(case.list[[1]]) & file.exists(case.list[[1]])){
    case.list <-  data.table(read.table(case.list,
                                        header = FALSE, sep ="\n",
                                        encoding = "windows-1252",
                                        stringsAsFactors = FALSE
    )
    )
    case.list <- case.list$V1
  }
  sobek_clist <- data.table(read.table(
    paste(sobek.project, "caselist.cmt", sep = "/"),
    header = FALSE, col.names = c('case_number', 'case_name'),
    sep = " ", quote = "'"), stringsAsFactors = FALSE
    )
  sobek_clist[, case_name := gsub('"', '', case_name, fixed = TRUE)]
  all_files <- dir(path = sobek.project,
                   pattern = "^[^0-9]{1,}$",
                   full.names = TRUE, no.. = TRUE)
  file.copy(from = all_files, to = dest, recursive = TRUE)
  for (i in case.list) {
    print(paste('copying case: ', i, "..."))
    from_folder <- dirname(get_file_path(
      case.name = i,
      sobek.project = sobek.project,
      type = 'bnd.dat'
    ))
    if (isTRUE(his.only)) {
      # export only HIS, HIA for reading result
      case_folder <- basename(from_folder)
      dest_folder <- paste(dest, case_folder, sep = "/")
      dir.create(dest_folder)
      case_files <- dir(path = from_folder,
                        full.names = TRUE,
                        no.. = TRUE)
      case_files <- case_files[grepl("\\.his$|\\.hia$", case_files,
                                     # fixed = TRUE,
                                     ignore.case = TRUE)]
      file.copy(case_files, dest_folder)
    } else{
      if (!isTRUE(copy.his)) {
        # do not copy .HIS file, saving disk space and time
        case_folder <- basename(from_folder)
        dest_folder <- paste(dest, case_folder, sep = "/")
        dir.create(dest_folder)
        case_files <- dir(path = from_folder,
                          full.names = TRUE,
                          no.. = TRUE)
        case_files <- case_files[!grepl("\\.his$", case_files,
                                        # fixed = TRUE,
                                        ignore.case = TRUE)]
        file.copy(case_files, dest_folder)
      } else{
        # copy all files - full export
        file.copy(from = from_folder,
                  to = dest,
                  recursive = TRUE)
      }
    }
  }
  # update register.cmt
  sobek_reg <- fread(file = paste(sobek.project, "register.cmt", sep = "\\"),
                     header = FALSE, sep = "\n", fill = FALSE,
                     stringsAsFactors = FALSE
                     )
  case_folders <- sapply(case.list, .get_case_number, sobek_clist)

  for (i in case_folders){
    sobek_reg <- sobek_reg[!grepl(paste("\\\\", i, "\\\\", sep = ""),
                                  V1)]
  }
  sobek_reg[1,1] <- as.character(length(sobek_reg$V1) - 1)
  case_cmt <- data.table(case_folders, case.list)
  print('updating register...')
  case_cmt[, case.list := paste("'", case.list, "'", sep = "")]
  write.table(case_cmt,
         file = paste(dest, "caselist.cmt", sep = "\\"),
         quote = FALSE,
         col.names = FALSE,
         row.names = FALSE,
         fileEncoding = "windows-1252",
         sep = " ")
  fwrite(sobek_reg,
         file = paste(dest, "register.cmt", sep = "\\"),
         col.names = FALSE,
         quote = FALSE
         )
}
