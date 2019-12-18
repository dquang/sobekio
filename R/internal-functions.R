# Convert parameter name to parameter index
param_name_2_id <- function(param.name, param.df) {
  # using exact matching to prevent potential problem caused by special characters
  param_short_check <- grep(tolower(param.name), tolower(param.df$param_short),
                      fixed = TRUE)
  if (length(param_short_check) == 1) {
    # stop('parameter with name: ', param.name, ' is ambiguous or not found')
    rel <- param_short_check[[1]]
  } else {
    rel <- NA_integer_
    param_long_check <- grep(tolower(param.name),
                             tolower(param.df$param_long),
                             fixed = TRUE)
    if (length(param_long_check) == 1) {
      rel <- param_long_check[[1]]
      } else{
        warning('parameter with name: ', param.name, 
                ' is ambiguous or not found')
        rel <- NA_integer_
        }
    }
  return(rel)
}


# Get data matrix of the .HIS file
# @param his.file Path to .HIS file, string
# @return a numeric matrix with ncol = total_loc*total_param, nrow = total_tstep
his_df <- function(his.file) {
  con <- file(his.file, open = "rb", encoding = "native.enc")
  seek(con, 160)
  param_nr <- readBin(con, "int", size = 4)
  total_loc <- readBin(con, "int", size = 4)
  data_bytes <- file.size(his.file) -
    (160 + # for the .HIS information ("title")
       2 * 4 + # for total parameters & total locations
       20 * param_nr + # for the paramter names
       total_loc * (4 + 20)) # location table
  # int(4) for time, double(4) for data
  total_tstep <- data_bytes / (4 + 4 * param_nr * total_loc)
  data_mtx <- matrix(nrow = total_tstep, ncol = param_nr * total_loc)
  # searching the start time (t0) and time step (dt) in his_title
  seek(con, where = 168 + 20 * param_nr + 24 * total_loc)
  # this reading can be done with chunk = max 1000.
  for (i in 1:total_tstep) {
    seek(con, where = 4, origin = "current")
    data_mtx[i, ] <- readBin(con, what = "double", size = 4,
                                               n = param_nr * total_loc
                                               )
  }
  close(con)
  return(data_mtx)
}


#' Get all time steps in the .HIS file
#' 
#' @param his.file Path to .HIS file, string
#' @return a single column data.frame of timeserie (POSXCt)
his_time_df <- function(his.file) {
  if (!file.exists(his.file)) {
    stop(paste("HIS file:", his.file, "does not exit!"))
  }
  con <- file(his.file, open = "rb", encoding = "native.enc")
  txt_title <- stri_conv(readBin(con, 'raw', n = 160), from = 'windows-1252')
  # get the total bytes of the his file
  his_fsize <- file.size(his.file)
  # get number of parameters and number of locations
  seek(con, 160)
  param_nr <- readBin(con, "int", size = 4)
  total_loc <- readBin(con, "int", size = 4)
  # get the number of time steps
  data_bytes <- his_fsize -
    (160 + # for the .HIS information ("title")
       2 * 4 + # for total parameters & total locations
       20 * param_nr + # for the paramter names
       total_loc * (4 + 20)) # location table

  # int(4) for time, double(4) for data
  total_tstep <- data_bytes / (4 + 4 * param_nr * total_loc)
  # searching the start time (t0) and time step (dt) in his_title
  t0_pattern <- "\\d{4}.\\d{2}.\\d{2} \\d{2}:\\d{2}:\\d{2}"
  dt_pattern <- "scu= *(\\d{1,})s"
  his_t0 <- stri_match_first_regex(txt_title, t0_pattern)[1]
  his_t0 <- as.POSIXct(his_t0, format = "%Y.%m.%d %H:%M:%S", tz = "GMT")
  his_dt <- as.integer(stri_match_first_regex(txt_title, dt_pattern)[2])
  his_time <- vector(mode = 'numeric', length = total_tstep)
  seek(con, where = 168 + 20 * param_nr + 24 * total_loc)
  for (i in 1:total_tstep) {
    his_time[i] <- readBin(con, "integer", size = 4) * his_dt
    seek(con, where = 4 * param_nr * total_loc, origin = "current")
  }
  close(con)
  his_time <- as.POSIXct(his_time, origin = his_t0, tz = "GMT")
  his_time <- data.table(ts = his_time)

  return(his_time)
}


#' Get parameter table from .HIS & .HIA files
#' 
#' @param his.file Path to the .HIS file
#' @return a data.table with two column: location & sobek.id
his_parameter <- function(his.file = "") {
  con <- file(his.file, open = "rb", encoding = "native.enc")
  # move file reading cursor to byte 160, where title ends
  # just to make sure correct reading
  seek(con, where = 160, origin = "start")
  # read total number of parameters
  param_nr <- readBin(con, what = "int", n = 1, size = 4)
  param_id <- vector(mode = "integer", length = param_nr)
  param_name <- vector(mode = "character", length = param_nr)
  seek(con, where = 168, origin = "start")
  # get parameter table
  for (i in 1:param_nr) {
    param_id[i] <- i
    param_name[i] <- stri_trim_both(stri_conv(readBin(con, what = "raw", n = 20),
                               from = 'windows-1252'))
    seek(con, where = 168 + 20 * i, origin = "start")
  }
  close(con)
  his.params <- data.table(cbind(param_id, str_trim(param_name)))
  colnames(his.params) <- c("param_id", "param_short")
  # try to read .hia
  hia_file <- paste(substr(his.file, start = 1, stop = nchar(his.file) - 4),
                    ".HIA", sep = "")
  if (file.exists(hia_file)) {
    hia_dt <- fread(
      file = hia_file,
      sep = "\n",
      header = FALSE,
      col.names = "V1",
      na.strings = "",
      data.table = TRUE,
      strip.white = FALSE,
      encoding = 'Latin-1',
      blank.lines.skip = TRUE,
      quote = ""
    )
    # remove blank lines
    hia_dt <- na.omit(hia_dt)
    # check if there is a Long Parameters Section
    hia_check <- TRUE %in% grepl("^\\[Long Parameters]", hia_dt$V1)
    # check if Long Parameters is the last section, and empty?
    if (hia_check) {
      long_loc_pos <- hia_dt[V1 == "[Long Parameters]", which = TRUE]
      if (long_loc_pos > length(hia_dt$V1)) hia_check <- FALSE
    }
    # check if Long Parameters is an empty section in between
    if (hia_check) {
      # get the first character of the next line after the "[Long Parameters]
      first_char <- substr(hia_dt$V1[long_loc_pos + 1], 1, 1)
      if (first_char == "[") hia_check <- FALSE
    }
    # finally get Long Parameters if till here hia_check is TRUE
    if (hia_check) {
      hia_sbegin <- grep("^\\[", hia_dt$V1) + 1
      hia_send <- shift(hia_sbegin, type = "lead",
                                    fill = length(hia_dt$V1) + 2) - 2
      pos_long_loc <- grep("^\\[Long Parameters]", hia_dt$V1)
      i_long_loc <- which(hia_sbegin == pos_long_loc + 1)
      if (length(i_long_loc) > 0) {
        long_loc <- hia_dt[hia_sbegin[i_long_loc]:hia_send[i_long_loc], ]
        long_loc[, c("param_id", "param_long") := tstrsplit(V1, "=", fixed = TRUE)]
        long_loc[, V1 := NULL]
        his.params <- merge(his.params, long_loc, all.x = TRUE,
                          by = "param_id",
                          sort = FALSE)
        his.params[which(is.na(param_long)), param_long := param_short]
      }
    }
  }
  # make sure his.params always has 3 columns
  if (!"param_long" %in% colnames(his.params)) {
    his.params[, param_long := '']
  }
  # correcting the 'water level' instead of 'waterlevel' in measstat.his
  his.params[, param_short := sub('water level|w.level',
                                  'Waterlevel',
                                  ignore.case = TRUE,
                                  param_short
                                  )
             ]
  his.params[, param_long := sub('water level|w.level',
                                  'Waterlevel',
                                 param_long,
                                 ignore.case = TRUE
  )
  ]
  return(his.params)
}


#' Information for his_from_case
id_type_tbl <- data.table(
  ID_TYPE = c(
    'mID', # Results at Measurements
    'wID', # Results at Nodes
    'qID', # Results at Reaches
    'lID or latID', # Results at Laterals
    'sID', # Results at Structures
    'pID', # Results for Pumpstations
    'tID', # Results for Triggers
    'fmID', # Results from Flowmap
    'fhID', # Results from Flowhis.his
    'moID', # Results from Morpmap.his
    'smID', # Results from Gsedmap.his
    'shID',  # Results from Gsedhis.his
    'File name'
  ),
  DESCRIPTION = c(
    'Results at Measurements',
    'Results at Nodes',
    'Results at Reaches',
    'Results at Laterals',
    'Results at Structures',
    'Results for Pumpstations',
    'Results for Triggers',
    'Results from Flowmap',
    'Results from Flowhis.his',
    'Results from Morpmap.his',
    'Results from Gsedmap.his',
    'Results from Gsedhis.his',
    'HIS file name without \'.HIS\'. Ex. reachvol = c(\'ID1\', \'ID2\')'
  )
) 


#' Finding correct file path
#' 
#' This function find the case-sensitive file path to a file
#' 
#' @param name Name of the file
#' @param path Path to parent folder
#' @param f_tbl Table of file name and path
#' @return A character string or NA_character_ if not found
file_path <- function(name = NULL, path = NULL, f_tbl = NULL) {
  if (is.null(f_tbl)) {
    f_list <- list.files(path = path, all.files = TRUE, 
                         include.dirs = FALSE,
                         full.names = TRUE, recursive = FALSE,
                         no.. = TRUE)
    f_name_list <- toupper(list.files(path = path, all.files = TRUE, 
                              include.dirs = FALSE,
                              full.names = FALSE, recursive = FALSE,
                              no.. = TRUE))
    f_tbl <- data.table(cs_path = f_list, f_name_upper = f_name_list)
  }
  ret <- f_tbl[f_name_upper == toupper(name), cs_path]
  if (length(ret) != 1) ret <- NA_character_
  return(ret)
}