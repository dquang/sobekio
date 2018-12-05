#' read a .HIS file and return a data.frame of location and sobek.id
#' Locations are SOBEK's internal index of the node/reach IDs
#' sobek.ids are IDs of nodes/reaches in the River Network
#' sobek.id are automatically truncated to max. length of 20 characters by SOBEK
#' @param his.file Path to the .HIS file
#' @return a data.table with two column: location & sobek.id
#' example his_loc("c:/sobek21302/rhein.lit/110/reachseg.his")
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
      long_loc_pos <- hia_dt[V1 == "[Long Locations]", which = TRUE]
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
        # data.table::setkey(long_loc, location)
        # data.table::setkey(his.locs, location)
        his.locs <- merge(his.locs, long_loc, all.x = TRUE,
                          by = "location",
                          sort = FALSE)
        his.locs[which(is.na(long.id)), long.id := sobek.id]
      }
    }
  }
  options("stringsAsFactors" = str_as_factor)
  return(his.locs)
}


################################################################################
#' get basic information of the .HIS file
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


################################################################################
#' Export data for nodes/reaches using IDs from a file
#' @param his.file Path to the .HIS file, string
#' @param id.file Path to file contain list of IDs \n
#' IDs list shoulde have the first column with IDs(col.name = "sobek.id")\n
#' Second column (col.name = "name") is for Names correspond to the IDs
#' @param f.header If the id.file contain header?, boolean, default = FALSE
#' @param f.sep Seperator of the id.file, default = '\t'
#' @param param Index of the Paramter to get the data, default = 1
#' @return A data.frame
#' @export
his_from_file <- function(
                          his.file, # path to .HIS file
                          id.file, # path to list of sobek.ids file
                          f.header = FALSE, # does node list contain header
                          f.sep = "\t", # seperation of node list
                          param = 1L) {
  str_as_factor <- default.stringsAsFactors()
  options("stringsAsFactors" = FALSE)
  if (!file.exists(his.file) || !file.exists(id.file)) {
    stop(paste("HIS file: ", his.file,
               " and/or id.file: ", id.file,
               " does not exit!"))
  }
  id.list <- data.table::fread(file = id.file,
                                 header = f.header,
                                 sep = f.sep,
                                 stringsAsFactors = FALSE,
                                 quote = ""
                                 )
  df_out <- his_from_list(his.file = his.file,
                          id.list = id.list$V1,
                          param = param)

  if (ncol(id.list) == 1){
    colnames(df_out) <- c("ts", id.list$V1)
  } else {
    colnames(df_out) <- c("ts", id.list$V2)
  }
  options("stringsAsFactors" = str_as_factor)
  return(df_out)
}

################################################################################
#' Export data for nodes/reaches using IDs from a file
#' @param his.file Path to the .HIS file, string
#' @param id.file Path to file contain list of IDs \n
#' IDs list shoulde have the first column with IDs(col.name = "sobek.id")\n
#' Second column (col.name = "name") is for Names correspond to the IDs
#' @param f.header If the id.file contain header?, boolean, default = FALSE
#' @param f.sep Seperator of the id.file, default = '\t'
#' @param param Index of the Paramter to get the data, default = 1
#' @return A data.frame
#' @export
his_from_file_1 <- function(
  his.file, # path to .HIS file
  id.file, # path to list of sobek.ids file
  f.header = FALSE, # does node list contain header
  f.sep = "\t", # seperation of node list
  param = 1L) {
  str_as_factor <- default.stringsAsFactors()
  options("stringsAsFactors" = FALSE)
  if (!file.exists(his.file) || !file.exists(id.file)) {
    stop(paste("HIS file: ", his.file,
               " and/or id.file: ", id.file,
               " does not exit!"))
  }
  con <- file(his.file, open = "rb", encoding = "native.enc")
  his_title <- readBin(con, "character", size = 160, endian = "little")
  seek(con, where = 160, origin = "start")
  param_nr <- readBin(con, "int", size = 4, endian = "little")
  total_loc <- readBin(con, "int", size = 4, endian = "little")
  data_bytes <- file.size(his.file) -
    (160 + # for the .HIS information ("title")
       2 * 4 + # for total parameters & total locations
       20 * param_nr + # for the paramter names
       total_loc * (4 + 20)) # location table
  # int(4) for time, double(4) for data
  total_tstep <- data_bytes / (4 + 4 * param_nr * total_loc)
  if (length(his_title) == 0) {
    close(con)
    stop(paste("HIS file:", his.file, "has wrong format"))
  }
  if (!grepl("SOBEK[[:space:]]{1,}", his_title)) {
    close(con)
    stop(paste("HIS file:", his.file, "has wrong format"))
  }
  close(con)
  id.list <- data.table::fread(file = id.file,
                               header = f.header,
                               sep = f.sep,
                               stringsAsFactors = FALSE,
                               quote = ""
  )
  hia_file <- paste(substr(his.file, start = 1, stop = nchar(his.file) - 4),
                    ".hia", sep = "")
  if (!file.exists(hia_file)){
    # if hia_file does not exist, we have to trim the node.id
    # get only the first 20 characters because SOBEK fixed format
    id.list[, 1] <- substring(id.list[, 1], first = 1, last = 20)
    warning("Sobek IDs from the input list were cut to length of 20",
            " because no .hia file found.")
  }
  # get location table
  locs <- his_location(his.file)

  id.list$sobek_loc <- lapply(id.list$V1,
                              FUN = .id2loc,
                              his.locs = locs
  )
  his_data <- .his_df(his.file = his.file,
                      param = param
  )

  # +1 for the date.time column
  df_out <- data.frame(matrix(ncol = length(id.list$sobek_loc),
                              nrow = total_tstep
  ),
  stringsAsFactors = FALSE
  )
  for (i in 1:length(id.list[["sobek_loc", exact = TRUE]])) {
    # id.list$sobek_loc[[i-1]] for the correct row in the id.list
    # after that, +1 for the correct column in the his_data
    # because the first column is the "ts"
    col_id <- id.list[["sobek_loc", exact = TRUE]][[i]]
    df_out[, i] <- his_data[, col_id]
  }
  rm(his_data)
  his_ts <- .his_time_df(his.file)

  df_out <- data.table::data.table(his_ts, df_out,
                       stringsAsFactors = FALSE)

  if (ncol(id.list)== 2){
    colnames(df_out) <- c("ts", id.list$V1)
  } else {
    colnames(df_out) <- c("ts", id.list$V2)
  }
  options("stringsAsFactors" = str_as_factor)
  return(df_out)
}


################################################################################
#' Export data for nodes/reaches using IDs from a list
#' @param his.file Path to the .HIS file, string
#' @param id.list List of IDs to search for data
#' IDs list shoulde have the first column with IDs(col.name = "sobek.id")\cr
#' Second column (col.name = "name") is for Names correspond to the IDs\cr
#' @param param Index of the Paramter to get the data, default = 1\cr
#' Name of the parameter can be given instead of index
#' @return A data.frame
#' @export
his_from_list_2 <- function(
                          his.file = NULL, # path to .HIS file
                          id.list = NULL, # list of node ids to get data
                          param = 1L) {
  str_as_factor <- default.stringsAsFactors()
  options("stringsAsFactors" = FALSE)
  if (!file.exists(his.file)) {
    stop(paste("HIS file:", his.file, "does not exist!"))
  }
  if(!is.vector("id.list")) stop("id.list must be a character vector")
  con <- file(his.file, open = "rb", encoding = "native.enc")
  seek(con, where = 160, origin = "start")
  param_nr <- readBin(con, what = "int", size = 4, endian = "little")
  total_loc <- readBin(con, what = "int", size = 4, endian = "little")
  close(con)
  # The rest is for numerical data with the following structure
  data_bytes <- file.size(his.file) -
    (160 + # for the .HIS information ("title")
       2 * 4 + # for total parameters & total locations
       20 * param_nr + # for the paramter names
       total_loc * (4 + 20)) # location table
  # int(4) for time, double(4) for data
  total_tstep <- data_bytes / (4 + 4 * param_nr * total_loc)
  hia_file <- paste(substr(his.file, start = 1, stop = nchar(his.file) - 4),
                    ".hia", sep = "")
  if (!file.exists(hia_file)){
    # if hia_file does not exist, we have to trim the node.id
    # get only the first 20 characters because SOBEK fixed format
    id.list <- lapply(id.list, FUN = substring, first = 1, last = 20)
    warning("Sobek IDs from the input list were cut to length of 20",
            " because no .hia file found.")
  }
  # get location table
  locs <- his_location(his.file)
  sobek_loc <- lapply(id.list,
                      FUN = .id2loc,
                      his.locs = locs
                      )
  hisdata <- .his_df(his.file = his.file,
                     param = param
                     )
  df_out <- data.frame(matrix(ncol = length(id.list),
                              nrow = total_tstep
                              ),
                       stringsAsFactors = FALSE
                       )
  colnames(df_out) <- id.list
  for (i in 1:length(sobek_loc)) {
    col_id <- sobek_loc[[i]]
    df_out[, i] <- hisdata[, col_id]
  }
  rm(hisdata)
  ts <- .his_time_df(his.file)
  df_out <- data.frame(ts, df_out,
                       stringsAsFactors = FALSE)
  options("stringsAsFactors" = str_as_factor)
  return(df_out)
}


################################################################################
#' Export data for nodes/reaches using IDs from a list
#' @param his.file Path to the .HIS file, string
#' @param id an ID to search for data
#' IDs list shoulde have the first column with IDs(col.name = "sobek.id")\cr
#' Second column (col.name = "name") is for Names correspond to the IDs\cr
#' @param param Index of the Paramter to get the data, default = 1\cr
#' Name of the parameter can be given instead of index
#' @return A data.frame
#' @export
his_single_id <- function(
  his.file = NULL, # path to .HIS file
  id = NULL, # list of node ids to get data
  param = 1L) {
  str_as_factor <- default.stringsAsFactors()
  options("stringsAsFactors" = FALSE)
  if (!file.exists(his.file)) {
    stop(paste("HIS file:", his.file, "does not exist!"))
  }
  if(!is.character(id)) stop("id must be a string")

  hisdata <- .his_id_df(his.file = his.file,
                        id = id[[1]],
                        param = param
                        )
  ts <- .his_time_df(his.file)
  df_out <- data.table::data.table(ts, hisdata,
                       stringsAsFactors = FALSE)
  colnames(df_out) <- c("ts", id[[1]])
  options("stringsAsFactors" = str_as_factor)
  return(df_out)
}


################################################################################
#' Export data for nodes/reaches using IDs from a list
#' @param his.file Path to the .HIS file, string
#' @param id.list List of IDs to search for data
#' IDs list shoulde have the first column with IDs(col.name = "sobek.id")
#' Second column (col.name = "name") is for Names correspond to the IDs
#' @param param Index of the Paramter to get the data, default = 1.
#' Name of the parameter can be given instead of index
#' @param nmax Maximum length of list to switch the method of getting data.
#' @return A data.table
#' @export
his_from_list <- function(
  his.file = NULL, # path to .HIS file
  id.list = NULL, # list of node ids to get data
  param = 1L,
  nmax = 70L) {
  str_as_factor <- default.stringsAsFactors()
  options("stringsAsFactors" = FALSE)
  if (!file.exists(his.file)) {
    stop(paste("HIS file:", his.file, "does not exist!"))
  }
  if(!is.vector(id.list)) stop("id must be a vector")
  con <- file(his.file, open = "rb", encoding = "native.enc")
  seek(con, where = 160, origin = "start")
  param_nr <- readBin(con, what = "int", size = 4, endian = "little")
  total_loc <- readBin(con, what = "int", size = 4, endian = "little")
  close(con)
  # The rest is for numerical data with the following structure
  data_bytes <- file.size(his.file) -
    (160 + # for the .HIS information ("title")
       2 * 4 + # for total parameters & total locations
       20 * param_nr + # for the paramter names
       total_loc * (4 + 20)) # location table
  # int(4) for time, double(4) for data
  total_tstep <- data_bytes / (4 + 4 * param_nr * total_loc)
  hia_file <- paste(substr(his.file, start = 1, stop = nchar(his.file) - 4),
                    ".hia", sep = "")
  if (!file.exists(hia_file)){
    # if hia_file does not exist, we have to trim the node.id
    # get only the first 20 characters because SOBEK fixed format
    id.list <- lapply(id.list, FUN = substring, first = 1, last = 20)
    warning("Sobek IDs from the input list were cut to length of 20",
            " because no .hia file found.")
  }
  # get location table
  # locs <- his_location(his.file)
  # sobek_loc <- lapply(id.list,
                      # FUN = .id2loc,
                      # his.locs = locs
                      # )
  if (length(id.list) <= nmax){
    # print("from here")
    hisdata <- .his_id_list_df(his.file = his.file,
                          id.list = id.list,
                          param = param
                          )
    df_out <- data.table(matrix(ncol = length(id.list),
                                nrow = total_tstep
                                ),
                         stringsAsFactors = FALSE
                         )
    ts <- .his_time_df(his.file)
    df_out <- data.table::data.table(ts, hisdata,
                                     stringsAsFactors = FALSE)
  } else {
    locs <- his_location(his.file)
    sobek_loc <- lapply(id.list,
                        FUN = .id2loc,
                        his.locs = locs
                        )
    # print("from there")
    hisdata <- .his_df(his.file = his.file,
                       param = param
                       )
    df_out <- data.frame(matrix(ncol = length(id.list),
                                nrow = total_tstep
                                ),
                         stringsAsFactors = FALSE
                         )
    # colnames(df_out) <- id.list
    for (i in 1:length(sobek_loc)) {
      col_id <- sobek_loc[[i]]
      df_out[, i] <- hisdata[, col_id]
    }
    ts <- .his_time_df(his.file)
    df_out <- data.frame(ts, df_out,
                         stringsAsFactors = FALSE)
  }
  colnames(df_out) <- c("ts", unlist(id.list))
  options("stringsAsFactors" = str_as_factor)
  return(data.table(df_out))
  # return(df_out)
}

#' Convert his file to csv
#' @param his.file Path to his file
#' @param output Path to output file
#' @param param Index or Name of parameter to get value
#' @export
his_2_csv <- function(his.file = NULL,
                      output = NULL,
                      param = 1L){
  rloc <- his_location(his.file)
  if(ncol(rloc) == 3) {
    id_list <- rloc$long.id
  } else{
    id_list <- rloc$sobek.id
  }
  # ifelse does not work, weird!
  rtble <- his_from_list(his.file = his.file,
                         id.list = id_list,
                         param = param)
  if(!dir.exists(dirname(output))) dir.create(dirname(output), recursive = TRUE)
  data.table::fwrite(rtble, file = output)
}
