#' Get the Data table for an element with specific ID from a .DAT files
#' @param dat.file Path to .DAT file
#' @param s.id ID of the element to get data
#' @return a data.table
#' @export
get_data_from_id <- function(dat.file = NULL,
                             s.id = NULL){
  # check input
  if (!file.exists(dat.file)) stop(dat.file, " file does not exist!")

  lat_dat <- fread(file = dat.file,
                   sep = "\n",
                   header = FALSE,
                   data.table = TRUE,
                   quote = ""
                   )
  id_tbl <- lat_dat[V1 %like% " id '"]
  s.id_check <- TRUE %in% grepl(paste(" id '", s.id, "'", sep = ""),
                                lat_dat$V1
                                )
  if (!s.id_check)  stop("ID: '", s.id, "' not found.")
  id_line <- lat_dat[V1 %like% " id '", which = TRUE]
  id_line_shift <- shift(id_line, n = 1L,
                         type = "lead",
                         fill = length(lat_dat$V1) + 1
                         ) - 1

  # position of the s.id in the lat_dat file
  id_pos <- lat_dat[V1 %like% paste(" id '", s.id, "'", sep = ""), which = TRUE]
  id_data_begin <- id_pos + 2
  # index of the s.id in the id_tbl
  id_idx <- which(id_line == id_pos)

  # data table of the s.id with header & footer
  id_data <- lat_dat[id_pos:id_line_shift[id_idx], ]
  # search last "tble..." line, to avoid commenting line at the end
  id_data_tble <- id_data[grepl("^tble", V1), which = TRUE]
  # position of last data line
  id_data_end <- id_pos + id_data_tble - 1
  # pure data of the s.id
  id_data <- id_data[3:(id_data_tble -1), ]
  # split the first column to three new columns
  id_data[, c("V2", "V3", "V4"):= tstrsplit(V1, split = " ", fix = TRUE)]

  return(id_data)
}


################################################################################
#' Set Value of an element in .DAT file to constant, and remove the time serie table
#' @param dat.file Path to .DAT file
#' @param s.id ID of the element to get data
#' @param value The constant value to set
#' @param output Path to write output file. Use the same path as dat.file to overwrite
#' @export
set_const_id <- function(dat.file = NULL,
                         s.id = NULL,
                         value = 0,
                         output = NULL
                         ){
  # check input
  if (!file.exists(dat.file)) stop(dat.file, " file does not exist!")
  out.file <- ifelse(is.null(output),
                     paste(dat.file, ".mod", sep = ""),
                     output)

  lat_dat <- fread(file = fdat,
                   sep = "\n",
                   header = FALSE,
                   data.table = TRUE,
                   quote = ""
                   )
  data_length <- length(lat_dat$V1)
  id_tbl <- lat_dat[V1 %like% " id '"]
  s.id_check <- TRUE %in% grepl(paste(" id '", s.id, "'", sep = ""),
                                lat_dat$V1
  )
  if (!s.id_check)  stop("ID: '", s.id, "' not found.")
  id_line <- lat_dat[V1 %like% " id '", which = TRUE]
  id_line_shift <- shift(id_line, n = 1L,
                         type = "lead",
                         fill = data_length + 1
                         ) - 1

  # position of the s.id in the lat_dat file
  id_pos <- lat_dat[V1 %like% paste(" id '", s.id,  "'", sep = ""), which = TRUE]
  # index of the s.id in the id_tbl
  id_idx <- which(id_line == id_pos)
  # data table of the s.id with header & footer
  id_data <- lat_dat[id_pos:id_line_shift[id_idx], ]
  # search last "tble..." line, to avoid commenting line at the end
  id_data_tble <- id_data[grepl("^tble", V1), which = TRUE]
  # position of the line tble for the s.id in the lat_dat
  id_pos_end <- id_pos + id_data_tble - 1
  s.id_line <- id_tbl$V1[[id_idx]]
  s.id_line <- sub("dc lt 1 0",
                   paste("dc lt 0", value, sep = " "),
                   s.id_line)
  s.id_line <- sub("PDIN 0 0 '' pdin",
                   tolower(substring(s.id_line, 1, 4)),
                   s.id_line)
  s.id_line <- list(
                 paste("* data table of", s.id, "was remove"),
                 paste("* value of", s.id, "is now constant =", value),
                 s.id_line
                 )
  fwrite(lat_dat[-(id_pos:id_pos_end)],
         file = out.file,
         quote = FALSE,
         col.names = FALSE,
         sep = " ")
  fwrite(data.table(s.id_line),
         file = out.file,
         append = TRUE,
         quote = FALSE,
         col.names = FALSE
         )

  return(TRUE)
}


################################################################################
#' Set Value of an element in .DAT file to constant, and remove the time serie table
#' @param dat.file Path to .DAT file
#' @param s.id ID of the element to get data
#' @param tble data.frame with in format "ts|value"
#' @param output Path to write output file. Use the same path as dat.file to overwrite
#' @export
change_tble <- function(dat.file = NULL,
                         s.id = NULL,
                         tble = NULL,
                         output = NULL
){
  # check input
  if (!file.exists(dat.file)) stop(dat.file, " file does not exist!")
  # out.file <- ifelse(is.null(output),
  #                    paste(dat.file, ".mod", sep = ""),
  #                    output)

  lat_dat <- fread(file = dat.file,
                   sep = "\n",
                   header = FALSE,
                   data.table = TRUE,
                   quote = ""
  )
  data_length <- length(lat_dat$V1)
  id_tbl <- lat_dat[V1 %like% " id '"]
  s.id_check <- TRUE %in% grepl(paste(" id '", s.id, "'", sep = ""),
                                lat_dat$V1
  )
  if (!s.id_check)  stop("ID: '", s.id, "' not found.")
  id_line <- lat_dat[V1 %like% " id '", which = TRUE]
  id_line_shift <- shift(id_line, n = 1L,
                         type = "lead",
                         fill = data_length + 1
  ) - 1

  # position of the s.id in the lat_dat file
  id_pos <- lat_dat[V1 %like% paste(" id '", s.id,  "'", sep = ""), which = TRUE]
  # index of the s.id in the id_tbl
  id_idx <- which(id_line == id_pos)
  # data table of the s.id with header & footer
  id_data <- lat_dat[id_pos:id_line_shift[id_idx], ]
  # search last "tble..." line, to avoid commenting line at the end
  id_data_tble <- id_data[grepl("^tble", V1), which = TRUE]
  # position of the line tble for the s.id in the lat_dat
  id_pos_end <- id_pos + id_data_tble - 1
  s.id_line <- id_tbl$V1[[id_idx]]
  # s.id_line <- sub("dc lt 1 0",
  #                  paste("dc lt 0", value, sep = " "),
  #                  s.id_line)
  # s.id_line <- sub("PDIN 0 0 '' pdin",
  #                  tolower(substring(s.id_line, 1, 4)),
  #                  s.id_line)
  # s.id_line <- list(
  #   paste("* data table of", s.id, "was remove"),
  #   paste("* value of", s.id, "is now constant =", value),
  #   s.id_line
  # )
  if (is.null(output)) output <- paste(substr(dat.file,
                                              start = 1,
                                              stop = nchar(dat.file) - 4),
                                       ".mod", sep = ""
                                       )
  fwrite(lat_dat[-(id_pos:id_pos_end)],
         file = output,
         quote = FALSE,
         col.names = FALSE,
         sep = " ")
  fwrite(data.table(list(s.id_line, "TBLE")),
         file = output,
         append = TRUE,
         quote = FALSE,
         col.names = FALSE
         )
  fwrite(tble,
         file = output,
         append = TRUE,
         quote = FALSE,
         col.names = FALSE
         )
  # get the ending line based on id type
  tble_footer <- id_data[grepl("^tble", V1)]
  fwrite(data.table(list(tble_footer)),
         file = output,
         append = TRUE,
         quote = FALSE,
         col.names = FALSE
         )

  return(TRUE)
}

