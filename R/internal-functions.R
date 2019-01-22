#' Convert Sobek ID to location index
#' @param sobek.id Node/Reach ID
#' @param his.locs Location table
#' @import data.table
.id2loc <- function(id, his.locs) {
  # using exact matching to prevent potential problem caused by special characters
  location <- his.locs[sobek.id == id, location]
  if (length(location) == 0){
    location <- his.locs[long.id == id, location]
  }
  if (length(location) == 0){
    location <- NA_integer_
  }
  if (is.na(location[[1]])) warning("sobek.id: '", id,
                               "' does not found in the location table")
  return(as.integer(location[[1]]))
}


# Convert parameter name to parameter index
.id2param <- function(param.name, param.df) {
  # using exact matching to prevent potential problem caused by special characters
  param_short_check <- grep(tolower(param.name), tolower(param.df$param_short),
                      fixed = TRUE)
  if (length(param_short_check) == 1){
    # stop('parameter with name: ', param.name, ' is ambiguous or not found')
    rel <- param_short_check[[1]]
  } else {
    rel <- NA_integer_
    param_long_check <- grep(tolower(param.name),
                             tolower(param.df$param_long),
                             fixed = TRUE)
    if (length(param_long_check) == 1){
      rel <- param_long_check[[1]]
      } else{
        warning('parameter with name: ', param.name, ' is ambiguous or not found')
        rel <- NA_integer_
        }
    }
  return(rel)
}


# Get column index of parameter (param) at the location (loc)
# in the data matrix
.loc2col <- function(loc, param, n_param){
  colnr <- param + n_param*(loc - 1)
  return(colnr)
}


# Get case number from case name
# .get_case_number <- function(case.name, case.list) {
#   if (typeof(case.list) != "list") stop("case.list must be a list")
#   if (case.name %in% case.list$case_name){
#     x <- case.list$case_number[case.list$case_name == case.name]
#     return(x)
#   } else{
#     warning("case ", case.name, " not found in the case list")
#     return(NA_integer_)
#   }
# }

.get_case_number <- function(case.name, case.list) {
  setkey(case.list, case_name)
  if (typeof(case.list) != "list") stop("case.list must be a list")
  x  <-  as.integer(case.list[case.name, case_number])
  if (is.na(x)) warning("case ", case.name, " not found in the case list")
  return(x)
}


# Get data matrix of the .HIS file
# @param his.file Path to .HIS file, string
# @return a numeric matrix with ncol = total_loc*total_param, nrow = total_tstep
.his_df <- function(his.file) {
  con <- file(his.file, open = "rb", encoding = "native.enc")
  # his_fsize <- file.size(his.file)
  seek(con, 160)
  param_nr <- readBin(con, "int", size = 4, endian = "little")
  total_loc <- readBin(con, "int", size = 4, endian = "little")
  data_bytes <- file.size(his.file) -
    (160 + # for the .HIS information ("title")
       2 * 4 + # for total parameters & total locations
       20 * param_nr + # for the paramter names
       total_loc * (4 + 20)) # location table
  # int(4) for time, double(4) for data
  total_tstep <- data_bytes / (4 + 4 * param_nr * total_loc)
  his_lines <- matrix(nrow = total_tstep, ncol = param_nr * total_loc)
  # searching the start time (t0) and time step (dt) in his_title
  seek(con, where = 168 + 20 * param_nr + 24 * total_loc)
  # this reading can be done with chunk = max 1000.
  for (i in 1:total_tstep) {
    # his_lines[i, 1] <- readBin(con, what = "int", size = 4,
    #                            n = 1L,
    #                            endian = "little")
    seek(con, where = 4, origin = "current")
    his_lines[i, ] <- readBin(con, what = "double", size = 4,
                                               n = param_nr * total_loc,
                                               endian = "little"
                                               )
  }
  close(con)
  return(his_lines)
}


# Get all time steps in the .HIS file with GMT as time zone
# @param his.file Path to .HIS file, string
# @return a single column data.frame of timeserie (POSXCt)
.his_time_df <- function(his.file) {
  # output: one column matrix, nrows = total time steps

  if (!file.exists(his.file)) {
    stop(paste("HIS file:", his.file, "does not exit!"))
  }

  con <- file(his.file, open = "rb", encoding = "native.enc")
  his_title <- vector(mode = "character", length = 4)
  txt_title <- readBin(con, "character", size = 160, endian = "little")
  for (i in 1:4) his_title[i] <- substr(txt_title,
                                        start = 40*(i - 1) + 1, stop = 40*i)

  # get the total bytes of the his file
  his_fsize <- file.size(his.file)

  # get number of parameters and number of locations
  seek(con, 160)
  param_nr <- readBin(con, "int", size = 4, endian = "little")
  total_loc <- readBin(con, "int", size = 4, endian = "little")
  # get the number of time steps
  data_bytes <- file.size(his.file) -
    (160 + # for the .HIS information ("title")
       2 * 4 + # for total parameters & total locations
       20 * param_nr + # for the paramter names
       total_loc * (4 + 20)) # location table

  # int(4) for time, double(4) for data
  total_tstep <- data_bytes / (4 + 4 * param_nr * total_loc)
  # searching the start time (t0) and time step (dt) in his_title
  t0_pattern <- "[0-9]{4}.[0-9]{2}.[0-9]{2}[[:space:]][0-9]{2}:[0-9]{2}:[0-9]{2}"
  dt_pattern <- "scu=[[:space:]]{1,}([0-9]{1,})(s)"
  his_t0 <- regmatches(his_title[4], regexpr(t0_pattern, his_title[4]))
  his_t0 <- as.POSIXct(his_t0, format = "%Y.%m.%d %H:%M:%S", tz = "GMT")
  his_dt <- as.integer(gsub(
    dt_pattern,
    "\\1",
    regmatches(
      his_title[4],
      gregexpr(dt_pattern, his_title[4])
    )[[1]]
  ))
  his_time <- matrix(nrow = total_tstep, ncol = 1)
  seek(con, where = 168 + 20 * param_nr + 24 * total_loc)
  for (i in 1:total_tstep) {
    his_time[i, 1] <- readBin(con, "integer", size = 4, endian = "little") * his_dt
    seek(con, where = 4 * param_nr * total_loc, origin = "current")
  }
  close(con)
  his_time <- as.POSIXct(his_time, origin = his_t0, tz = "GMT")
  colnames(his_time) <- c("ts")

  return(his_time)
}


#' Get parameter table from .HIS & .HIA files
#' @param his.file Path to the .HIS file
#' @return a data.table with two column: location & sobek.id
.his_parameter <- function(his.file = "") {
  str_as_factor <- default.stringsAsFactors()
  options("stringsAsFactors" = FALSE)
  con <- file(his.file, open = "rb", encoding = "native.enc")
  his_title <- vector(mode = "character", length = 4)
  txt_title <- readBin(con, "character", size = 160, endian = "little")
  for (i in 1:4) his_title[i] <- substr(txt_title,
                                        start = 40*(i - 1) + 1, stop = 40*i)
  # move file reading cursor to byte 160, where title ends
  # just to make sure correct reading
  seek(con, where = 160, origin = "start")
  # read total number of parameters
  param_nr <- readBin(con, what = "int", n = 1, size = 4, endian = "little")
  # read total number of locations
  # total_loc <- readBin(con, what = "int", n = 1, size = 4, endian = "little")
  param_id <- vector(mode = "integer", length = param_nr)
  param_name <- vector(mode = "character", length = param_nr)
  seek(con, where = 168, origin = "start")
  # get parameter table
  for (i in 1:param_nr){
    param_id[i] <- i
    param_name[i] <- substr(readBin(con,
                                  what = "character", size = 20,
                                  endian = "little"
    ),
    start = 1,
    stop = 20 # SOBEK output max. 20 chars names
    )
    seek(con, where = 168 + 20 * i, origin = "start")
  }
  close(con)
  his.params <- data.table(cbind(param_id, trimws(param_name)))
  colnames(his.params) <- c("param_id", "param_short")
  # try to read .hia
  hia_file <- paste(substr(his.file, start = 1, stop = nchar(his.file) - 4),
                    ".hia", sep = "")
  if (file.exists(hia_file)){
    hia_dt <- data.table::fread(file = hia_file,
                                sep = "\n",
                                header = F,
                                col.names = "V1",
                                na.strings = "",
                                data.table = TRUE,
                                # encoding = "UTF-8",
                                blank.lines.skip = TRUE,
                                quote = "")
    # remove blank lines
    hia_dt <- na.omit(hia_dt)
    # check if there is a Long Parameters Section
    hia_check <- TRUE %in% grepl("^\\[Long Parameters]", hia_dt$V1)
    # check if Long Parameters is the last section, and empty?
    if (hia_check){
      long_loc_pos <- hia_dt[V1 == "[Long Parameters]", which = TRUE]
      if (long_loc_pos > length(hia_dt$V1)) hia_check <- FALSE
    }
    # check if Long Parameters is an empty section in between
    if (hia_check){
      # get the first character of the next line after the "[Long Parameters]
      first_char <- substr(hia_dt$V1[long_loc_pos + 1], 1, 1)
      if (first_char == "[") hia_check <- FALSE
    }
    # finally get Long Parameters if till here hia_check is TRUE
    if (hia_check){
      hia_sbegin <- grep("^\\[", hia_dt$V1) + 1
      hia_send <- data.table::shift(hia_sbegin, type = "lead",
                                    fill = length(hia_dt$V1) + 2) - 2
      pos_long_loc <- grep("^\\[Long Parameters]", hia_dt$V1)
      i_long_loc <- which(hia_sbegin == pos_long_loc + 1)
      if (length(i_long_loc) > 0){
        long_loc <- hia_dt[hia_sbegin[i_long_loc]:hia_send[i_long_loc], ]
        long_loc[, c("param_id", "param_long") := data.table::tstrsplit(V1, "=",
                                                                     fixed = TRUE)]
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
  options("stringsAsFactors" = str_as_factor)
  return(his.params)
}


# Get data of given locations and parameter
# @param his.file Path to .HIS file, string
# @param param Index of the parameter, integer
# @return a data.table
.his_from_locs <- function(his.file, locs, param = 1L) {
  # get whole data matrix
  hisdf <- .his_df(his.file)
  # timedf <- .his_time_df(his.file)
  con <- file(his.file, 'rb', encoding = 'native.enc')
  seek(con, 160)
  param_nr = readBin(con, 'int', n = 1, size = 4)
  close(con)
  # creating a mask for the matrix, to get only columns for the param
  cols_mask <- sapply(locs, .loc2col, param, param_nr)
  return(hisdf[, unlist(cols_mask)])
}
