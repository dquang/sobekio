#' Export list of cases from a sobek project to another location
#' @param case.list Path to file with case list
#' @param sobek.project Path to current sobek project
#' @param dest Destination folder
#' @export
sobek_export <- function(case.list, sobek.project, dest){
  if (!dir.exists(dest)) dir.create(dest)
  case.list <-  data.table(read.table(case.list,
                        header = FALSE, sep ="\n",
                        encoding = "windows-1252",
                        stringsAsFactors = FALSE
                        )
                        )
  sobek_clist <- data.table(read.table(
    paste(sobek.project, "caselist.cmt", sep = "/"),
    header = FALSE, col.names = c('case_number', 'case_name'),
    sep = " ", quote = "'"), stringsAsFactors = FALSE
    )
  sobek_clist[, case_name := gsub('"', '', case_name, fixed = TRUE)]
  all_files <- dir(path = sobek.project,
                   pattern = "^[^0-9]{1,}$",
                   full.names = T, no.. = T)
  file.copy(from = all_files, to = dest, recursive = T)
  for (i in case.list$V1){
    print(paste('copying case: ', i, "..."))
    from_folder <- dirname(get_file_path(case.name = i,
                                         sobek.project = sobek.project,
                                         type = 'bnd.dat'))
    file.copy(from = from_folder, to = dest, recursive = T)
    }
  # update register.cmt
  sobek_reg <- fread(file = paste(sobek.project, "register.cmt", sep = "\\"),
                     header = FALSE, sep = "\n", fill = FALSE,
                     stringsAsFactors = FALSE
                     )
  case_folders <- sapply(case.list$V1, .get_case_number, sobek_clist)

  for (i in case_folders){
    sobek_reg <- sobek_reg[!grepl(paste("\\\\", i, "\\\\", sep = ""),
                                  V1)]
  }
  sobek_reg[1,1] <- as.character(length(sobek_reg$V1) - 1)
  case_cmt <- data.table(case_folders, case.list)
  print('updating register...')
  write.table(case_cmt,
         file = paste(dest, "caselist.cmt", sep = "\\"),
         quote = TRUE,
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
