#' Read location table from .HIS file
#' Locations are SOBEK's internal index of the node/reach IDs
#' sobek.ids are IDs of nodes/reaches in the River Network
#' sobek.id are automatically truncated to max. length of 20 characters by SOBEK
#' @param his.file Path to the .HIS file
#' @return a data.table with two column: location & sobek.id
#' @export
#' @import data.table
his_location <- function(his.file = "") {
  str_as_factor <- default.stringsAsFactors()
  options("stringsAsFactors" = FALSE)
  # this fucntion take input is a HIS file (path to)
  # out put is a data frame with two column (location & sobek.id)
  if (!file.exists(his.file)) {
    stop(paste("HIS file:", his.file, "does not exit!"))
  }
  con <- file(his.file, open = "rb", encoding = "native.enc")
  # check .HIS file simple way
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
  total_loc <- readBin(con, what = "int", n = 1, size = 4, endian = "little")
  # initialize location -id and -name vectors
  loc_id <- vector(mode = "integer", length = total_loc)
  loc_name <- vector(mode = "character", length = total_loc)
  # 160 for the title, 4 + 4 for param_nr,total_loc, and 20*param_nr for params
  # yes I think that it is more readable without spaces around "*"
  seek(con, where = 168 + 20 * param_nr, origin = "start")
  # get locations table
  for (i in 1:total_loc){
    loc_id[i] <- as.integer(i)
    seek(con, 4, "current")
    loc_name[i] <- substr(readBin(con,
    															what = "character", size = 20,
    															endian = "little"
    															),
    											start = 1,
    											stop = 20 # SOBEK output max. 20 chars names
    											)
    seek(con, where = 168 + 20 * param_nr + 24 * i, origin = "start")
  }
  close(con)
  his.locs <- data.table(cbind(as.integer(loc_id), trimws(loc_name)))
  colnames(his.locs) <- c("location", "sobek.id")
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
    # check if there is a Long Location Section
    hia_check <- TRUE %in% grepl("^\\[Long Locations]", hia_dt$V1)
    # check if Long Locations is the last section, and empty?
    if (hia_check){
      long_loc_pos <- which(hia_dt$V1 == "[Long Locations]")
      if (long_loc_pos > length(hia_dt$V1)) hia_check <- FALSE
    }
    # check if Long Locations is an empty section in between
    if (hia_check){
      # get the first character of the next line after the "[Long Locations]
      first_char <- substr(hia_dt$V1[long_loc_pos + 1], 1, 1)
      if (first_char == "[") hia_check <- FALSE
    }
    # finally get Long Locations if till here hia_check is TRUE
    if (hia_check){
      hia_sbegin <- grep("^\\[", hia_dt$V1) + 1
      hia_send <- data.table::shift(hia_sbegin, type = "lead",
                                    fill = length(hia_dt$V1) + 2) - 2
      pos_long_loc <- grep("^\\[Long Locations]", hia_dt$V1)
      i_long_loc <- which(hia_sbegin == pos_long_loc + 1)
      if (length(i_long_loc) > 0){
        long_loc <- hia_dt[hia_sbegin[i_long_loc]:hia_send[i_long_loc], ]
        long_loc[, c("location", "long.id") := data.table::tstrsplit(V1, "=",
                                                                     fixed = TRUE)]
        long_loc[, V1 := NULL]
        his.locs <- merge(his.locs, long_loc, all.x = TRUE,
                          by = "location",
                          sort = FALSE)
        # his.locs[which(is.na(long.id)), long.id := sobek.id]
      }
    }
  }
  options("stringsAsFactors" = str_as_factor)
  if (!'long.id' %in% colnames(his.locs)) his.locs[, long.id:='']
  return(his.locs)
}


#' Read basic information of the .HIS file
#' @param his.file Path to the .HIS file
#' @return A list of basic information, includes:
#' \itemize{
#' \item Name of the Sobek Modell Case: case_name
#' \item Number of parameters: param_nr
#' \item Names of Parameters: param_names
#' \item Total locations (ID): total_loc
#' \item Total time steps: total_tstep
#' \item Start time ot the timeserie: his_t0
#' \item Time step of the timeserie: his_dt (sec.)
#' }
#' @export
his_info <- function(his.file = "") {
  str_as_factor <- default.stringsAsFactors()
  options("stringsAsFactors" = FALSE)
  if (!file.exists(his.file)) {
    stop(paste("HIS file:", his.file, "does not exit!"))
  }
  con <- file(his.file, open = "rb", encoding = "native.enc")
  # check .HIS file simple way
  # first 160 bytes are characters for the title part
  # check .HIS file simple way
  his_title <- vector(mode = "character", length = 4)
  txt_title <- readBin(con, "character", size = 160, endian = "little")
  for (i in 1:4) his_title[i] <- substr(txt_title,
                                        start = 40*(i - 1) + 1, stop = 40*i)
  # bytes 160-167 are for 2 int
  seek(con, where = 160, origin = "start")
  param_nr <- readBin(con, what = "int", size = 4, endian = "little")
  total_loc <- readBin(con, what = "int", size = 4, endian = "little")
  seek(con, where = 168, origin = "start")
  # read parameter names
  params_str <- readBin(con, what = "character", size = 20*param_nr,
                        endian = "little")
  # each parameter name is stored in a fixed string having length = 20
  param_names <- vector(mode = "character", length = param_nr)
  # removing padding strings at the end
  for (i in 1:param_nr) param_names[i] <- substr(params_str,
                                                 start = 20*(i - 1) + 1,
                                                 stop = 20*i)
  close(con)
  # The rest is for numerical data with the following structure
  data_bytes <- file.size(his.file) -
    (160 + # for the .HIS information ("title")
      2 * 4 + # for total parameters & total locations
      20 * param_nr + # for the paramter names
      total_loc * (4 + 20)) # location table
  # int(4) for time, double(4) for data
  total_tstep <- data_bytes / (4 + 4 * param_nr * total_loc)
  # searching the case name
  case_name <- substr(his_title[3], start = 8, stop = nchar(his_title[3]))
  # searching the start time (t0) and time step (dt) in title
  t0_pattern <- "[0-9]{4}.[0-9]{2}.[0-9]{2}[[:space:]][0-9]{2}:[0-9]{2}:[0-9]{2}"
  dt_pattern <- "scu=[[:space:]]{1,}([0-9]{1,})s"
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
  result <- list(
    case_name = case_name,
    param_nr = param_nr,
    param_names = param_names,
    total_loc = total_loc,
    total_tstep = total_tstep,
    his_t0 = his_t0,
    his_dt = his_dt
  )
  options("stringsAsFactors" = str_as_factor)
  return(result)
}


#' Export data for nodes/reaches using IDs from a list
#' @param his.file Path to the .HIS file, string
#' @param id.list List of Sobek IDs
#' @param param Index or Name of the Paramter to get the data, default = 1.
#' @return A data.table
#' @export
his_from_list <- function(
  his.file = NULL, # path to .HIS file
  id.list = NULL, # list of node ids to get data
  param = 1L) {
  str_as_factor <- default.stringsAsFactors()
  options("stringsAsFactors" = FALSE)
  if (!file.exists(his.file)) {
    stop("HIS file: ", his.file, " does not exist!")
  }
  con <- file(his.file, 'rb')
  seek(con, 160)
  param_nr <- readBin(con, 'int', n = 1L, size = 4)
  close(con)
  if(!is.vector(id.list)) stop("id must be a vector")
  if (!is.numeric(param)) {
    pardf <- .his_parameter(his.file)
    par <- .id2param(param, pardf)
    if (is.na(par)) {
      print('List of Parameters in the .HIS file')
      print(pardf)
      stop('Parameter: ', param, ' not found')
    }
  } else {
    par <- as.integer(param)
    if (!par %in% seq.int(1, param_nr, 1)){
      stop('Parameter: ', param, ' out of range(1, ', param_nr,')')
    }
  }
  locdf <- his_location(his.file)
  locs <- sapply(id.list, .id2loc, locdf)
  hisdf <- .his_from_locs(his.file = his.file, locs = locs, param = par)
  tsdf <- .his_time_df(his.file = his.file)
  df_out <- data.table(tsdf, hisdf, stringsAsFactors = FALSE)
  options("stringsAsFactors" = str_as_factor)
  colnames(df_out) <- c('ts', id.list)
  return(df_out)
}


#' Export data for nodes/reaches using IDs from a file
#' @param his.file Path to the .HIS file
#' @param id.file Path to file contain list of IDs \n.
#' ID file should have one-two columns, first for the sobek ID and the second (optional) for names that will be column names in the output data.table.
#' @param param Index/Name of the Paramter to get the data, default = 1
#' @param f.header The id.file contain header?, boolean, default = FALSE
#' @param f.sep Seperator of the id.file, default = 'TAB'
#' @param f.comment Commment character of the file, default = '#'
#' @return A data.table
#' @export
his_from_file <- function(
                          his.file, # path to .HIS file
                          id.file, # path to list of sobek.ids file
                          param = 1L,
                          f.header = FALSE, # does node list contain header
                          f.sep = "\t", # seperation of node list
                          f.comment = '#'
                          ) {
  str_as_factor <- default.stringsAsFactors()
  options("stringsAsFactors" = FALSE)
  if (!file.exists(his.file) || !file.exists(id.file)) {
    stop(paste("HIS file: ", his.file,
               " and/or id.file: ", id.file,
               " does not exit!"))
  }
  id.list <- read.table(
    file = id.file,
    header = f.header,
    sep = f.sep,
    quote = "",
    comment.char = f.comment,
    stringsAsFactors = FALSE,
    blank.lines.skip = TRUE
  )
  df_out <- his_from_list(his.file = his.file,
                          id.list = id.list[, 1],
                          param = param)

  if (ncol(id.list) > 1){
    colnames(df_out) <- c("ts", id.list[, 2])
  }
  options("stringsAsFactors" = str_as_factor)
  return(df_out)
}


#' Convert his file to CSV, for one parameter
#' @param his.file Path to his file
#' @param output Path to output file
#' @param param Index or Name of parameter to get value
#' @param ... Other parameters passing to data.table::fwrite function
#' @export
his_2_csv <- function(his.file = NULL,
                      output = NULL,
                      param = 1L,
                      ...){
  rloc <- his_location(his.file)
  # ifelse does not work, weird!
  rtble <- his_from_list(his.file = his.file,
                         id.list = rloc$sobek.id,
                         param = param)
  if(!dir.exists(dirname(output))) dir.create(dirname(output), recursive = TRUE)
  data.table::fwrite(rtble, file = output, ...)
}
