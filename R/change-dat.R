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
  # id_tbl <- lat_dat[V1 %like% " id '"]
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
  # id_data_begin <- id_pos + 2
  # index of the s.id in the id_tbl
  id_idx <- which(id_line == id_pos)
  # data table of the s.id with header & footer
  id_data <- lat_dat[id_pos:id_line_shift[id_idx], ]
  # search last "tble..." line, to avoid commenting line at the end
  id_data_tble <- id_data[grepl("^tble", V1), which = TRUE]
  # position of last data line
  # id_data_end <- id_pos + id_data_tble - 1
  # pure data of the s.id
  id_data <- id_data[3:(id_data_tble -1), ]
  # split the first column to three new columns
  id_data[, c("V2", "V3", "V4"):= tstrsplit(V1, split = " ", fix = TRUE)]

  return(id_data)
}


#' Set Value of an element in .DAT file to constant, and remove the time serie table
#' @param dat.file Path to .DAT file
#' @param s.id ID of the element to get data
#' @param value const value to set, default = 0
#' @param output Path to write output file. Use the same path as dat.file to overwrite
#' @export
set_q_const <- function(dat.file = NULL,
                        s.id = NULL,
                        value = 0,
                        output = NULL
){
  # check input
  outDec <- getOption("OutDec")
  options(OutDec = ".")
  on.exit(options(OutDec = outDec))
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
  if (length(id_data_tble) == 0) id_data_tble = 0
  # position of the line tble for the s.id in the lat_dat
  id_pos_end <- id_pos + id_data_tble - 1
  s.id_line <- id_tbl$V1[[id_idx]]
  if (is.character(value)) value <- sub(",", ".", value, fixed = TRUE)
  s.id_line_new <- sub("ty 1 q_ .*",
                       paste("ty 1 q_ dw 0  ", value, " 0 ",
                             tolower(substr(s.id_line, 1, 4)),
                             sep = ""),
                       s.id_line)
  if (s.id_line_new == s.id_line) warning('value does not change ', dat.file)
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
  fwrite(list(s.id_line_new),
         file = output,
         append = TRUE,
         quote = FALSE,
         col.names = FALSE
  )
  return(TRUE)
}


#' change table value of one id in the DAT file
#' @param dat.file Path to .DAT file
#' @param s.id ID of the element to get data
#' @param tble a data.frame/data.table of one column with PRN format
#' If tble is given as data.frame of two columns, the first one is time serie and the second one is the value, it will be converted to PRN format.
#' @param output Path to write output file. Use the same path as dat.file to overwrite
#' @export
change_tble <- function(dat.file = NULL,
                        output = dat.file,
                        s.id = NULL,
                        tble = NULL,
                        comments = NULL
){

  outDec <- getOption("OutDec")
  options(OutDec = ".")
  on.exit(options(OutDec = outDec))
  # check input
  if (!is.data.frame(tble)) stop('tble must be a data.frame/data.table')
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
  if (length(id_data_tble) == 0) id_data_tble = 0
  # position of the line tble for the s.id in the lat_dat
  id_pos_end <- id_pos + id_data_tble - 1
  s.id_line <- id_tbl$V1[[id_idx]]
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
  if (!is.null(comments)){
    fwrite(list(paste("*", comments)),
           file = output,
           append = TRUE,
           quote = FALSE,
           col.names = FALSE
    )
  }
  fwrite(data.table(list(s.id_line, "TBLE")),
         file = output,
         append = TRUE,
         quote = FALSE,
         col.names = FALSE
  )
  # checking tble input
  prn_pattern <- "[\"\'][0-9]{4}/[0-9]{2}/[0-9]{2};[0-9]{2}:[0-9]{2}:[0-9]{2}[\"\'] [0-9]{1,}\\.?[0-9]{1,} <"
  tble_sample <- sample(1:length(tble[, 1]), 5, replace = T)
  tble_check <- FALSE %in% grepl(prn_pattern, tble[tble_sample, 1])
  if (tble_check){
    if (!is.data.table(tble)) tble <- as.data.table(tble)
    if (ncol(tble) != 2) stop('tble should have 2 columns')
    colnames(tble) <- c('ts', 'value')
    ts_class_check <- FALSE %in% grepl("POSIX", tble[1, 1])
    if (ts_class_check){
      tble[, ts := as.POSIXct(ts, tz = 'GMT',
                              tryFormats = c(
                                "%Y-%m-%d %H:%M:%S",
                                "%Y/%m/%d %H:%M:%S",
                                "%d.%m.%Y %H:%M:%S",
                                "%Y-%m-%d %H:%M",
                                "%Y/%m/%d %H:%M",
                                "%d.%m.%Y %H:%M",
                                "%Y-%m-%d",
                                "%Y/%m/%d",
                                "%d.%m.%Y")
                              )
           ]
    }
    tble[ ,prn := paste("'",
                        format(ts,
                               format = '%Y/%m/%d;%H:%M:%S', tz = 'GMT'),
                        "' ",
                        value,
                        " <",
                        sep = ""
                        )
    ]
    fwrite(tble[, c("prn")],
           file = output,
           append = TRUE,
           quote = FALSE,
           col.names = FALSE
    )
  } else{
    fwrite(tble[, 1],
           file = output,
           append = TRUE,
           quote = FALSE,
           col.names = FALSE
    )
  }
  # get the ending line based on id type
  tble_footer <- list(paste("tble", tolower(substr(id_data[1,1], 1, 4)),
                            sep = " "))
  fwrite(data.table(tble_footer),
         file = output,
         append = TRUE,
         quote = FALSE,
         col.names = FALSE
  )
  return(TRUE)
}
