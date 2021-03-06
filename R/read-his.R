#' Get location table from .HIS file
#'
#' Locations are SOBEK's internal index of the node/reach IDs
#' sobek.id are IDs of nodes/reaches in the River Network
#' sobek.id are automatically truncated to max. length of 20 characters by SOBEK
#'
#' @param his.file Path to the .HIS file
#' @return a data.table with two column: location & sobek.id
#' @export
#' @import data.table
his_location <- function(his.file) {
  # this fucntion take input is a HIS file (path to)
  # out put is a data frame with two column (location & sobek.id)
  if (!file.exists(his.file)) {
    stop(paste("HIS file:", his.file, "does not exit!"))
  }
  con <- file(his.file, open = "rb", encoding = "native.enc")
  # move file reading cursor to byte 160, where title ends
  # just to make sure correct reading
  seek(con, where = 160, origin = "start")
  # read total number of parameters
  param_nr <- readBin(con, what = "int", n = 1, size = 4)
  # read total number of locations
  total_loc <- readBin(con, what = "int", n = 1, size = 4)
  # initialize location -id and -name vectors
  loc_id <- vector(mode = "integer", length = total_loc)
  loc_id <- seq.int(from = 1, to = total_loc, by = 1)
  loc_name <- vector(mode = "character", length = total_loc)
  # 160 for the title, 4 + 4 for param_nr,total_loc, and 20*param_nr for params
  seek(con, where = 168 + 20 * param_nr, origin = "start")
  # get locations table
  for (i in 1:total_loc) {
    seek(con, 4, "current")
    loc_name[i] <- stri_conv(readBin(con, what = "raw", n = 20),
                             from = 'windows-1252')
    seek(con, where = 168 + 20 * param_nr + 24 * i, origin = "start")
  }
  close(con)
  his_locs <- data.table(
    location = loc_id,
    sobek.id = str_trim(loc_name, side = 'both')
  )
  # try to read .HIA
  hia_file <- stri_replace_first_regex(
    his.file,
    "\\.his$",  "\\.hia",
    opts_regex = stri_opts_regex(case_insensitive = TRUE))
  hia_file <- file_path(hia_file, dirname(his.file))
  if (file.exists(hia_file)) {
    hia_dt <- fread(
      file = hia_file,
      sep = "\n",
      header = F,
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
    hia_dt <- hia_dt[!grepl("^\\ {1,}$", V1), ]
    # check if there is a Long Location Section
    hia_check <- TRUE %in% grepl("^\\[Long Locations]", hia_dt$V1)
    # check if Long Locations is the last section, and empty?
    if (hia_check) {
      long_loc_pos <- which(hia_dt$V1 == "[Long Locations]")
      if (long_loc_pos >= length(hia_dt$V1)) hia_check <- FALSE
    }
    # check if Long Locations is an empty section in between
    if (hia_check) {
      # get the first character of the next line after the "[Long Locations]
      first_char <- str_sub(hia_dt$V1[long_loc_pos + 1], 1, 1)
      if (first_char == "[") hia_check <- FALSE
    }
    # finally get Long Locations if till here hia_check is TRUE
    if (hia_check) {
      hia_sbegin <- grep("^\\[", hia_dt$V1) + 1
      hia_send <- data.table::shift(hia_sbegin, type = "lead",
                                    fill = length(hia_dt$V1) + 2) - 2
      pos_long_loc <- grep("^\\[Long Locations]", hia_dt$V1)
      i_long_loc <- which(hia_sbegin == pos_long_loc + 1)
      if (length(i_long_loc) > 0) {
        long_loc <- hia_dt[hia_sbegin[i_long_loc]:hia_send[i_long_loc], ]
        long_loc[, c("location", "sobek.id") := tstrsplit(V1, "=", fixed = TRUE)]
        long_loc[, V1 := NULL]
        his_locs <- rbind(long_loc, his_locs)
      }
    }
  }
  his_locs[, location := as.integer(location)]
  setkey(his_locs, sobek.id)
  return(his_locs)
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
his_info <- function(his.file) {
  if (!file.exists(his.file)) {
    stop(paste("HIS file:", his.file, "does not exit!"))
  }
  con <- file(his.file, open = "rb", encoding = "native.enc")
  # first 160 bytes are characters for the title part
  txt_title <- stri_conv(readBin(con, what = "raw", n = 160),
                         from = 'windows-1252')
  his_title <- str_extract_all(txt_title, ".{40}", simplify = TRUE)
  # bytes 160-167 are for 2 int
  seek(con, where = 160, origin = "start")
  param_nr <- readBin(con, what = "int", size = 4, endian = "little")
  total_loc <- readBin(con, what = "int", size = 4, endian = "little")
  seek(con, where = 168, origin = "start")
  # read parameter names
  params_str <- readBin(con, what = "character", size = 20 * param_nr,
                        endian = "little")
  # each parameter name is stored in a fixed string having length = 20
  param_names <- vector(mode = "character", length = param_nr)
  # removing padding strings at the end
  for (i in 1:param_nr) param_names[i] <- str_sub(params_str,
                                                 start = 20 * (i - 1) + 1,
                                                 end = 20 * i)
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
  case_name <- str_sub(his_title[3], start = 8, end = nchar(his_title[3]))
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
  return(result)
}


#' Export data for nodes/reaches using IDs from a list
#' @param his.file Path to the .HIS file, string
#' @param id.list List of Sobek IDs
#' @param param Index or Name of the Paramter to get the data, default = 1.
#' @return A data.table
#' @export
his_from_list <- function(
  his.file,
  id.list,
  param = 1L) {
  id.list <- as.character(unlist(id.list))
  if (!file.exists(his.file)) {
    stop("HIS file: ", his.file, " does not exist!")
  }
  con <- file(his.file, 'rb')
  seek(con, 160)
  param_nr <- readBin(con, 'int', n = 1L, size = 4)
  close(con)
  if (!is.vector(id.list)) stop("id must be a vector")
  if (!is.numeric(param)) {
    pardf <- his_parameter(his.file)
    par_int <- param_name_2_id(param, pardf)
    if (is.na(par_int)) {
      cat('List of Parameters in the .HIS file\n')
      print(pardf)
      stop('Parameter: ', param, ' not found')
    }
  } else {
    par_int <- as.integer(param)
    if (!par_int %in% seq.int(1, param_nr, 1)) {
      stop('Parameter: ', param, ' out of range(1, ', param_nr,')')
    }
  }
  locdf <- his_location(his.file)
  locs <- locdf[id.list, location]
  hisdf <- his_df(his.file)
  matrix_chk <- FALSE
  if (nrow(hisdf) == 1) matrix_chk <- TRUE
  # creating a mask for the matrix, to get only columns for the param
  cols_mask <- par_int + param_nr * (locs - 1)
  hisdf <- hisdf[, cols_mask]
  if (matrix_chk) hisdf <- matrix(hisdf, nrow = 1)
  tsdf <- his_time_df(his.file = his.file)
  df_out <- data.table(tsdf, hisdf)
  colnames(df_out) <- c('ts', id.list)
  return(df_out)
}


#' Export data for nodes/reaches using IDs from a file
#' @param his.file Path to the .HIS file
#' @param id.file Path to file contain list of IDs.
#' ID file should have one-two columns, first for the sobek ID and the second (optional) for names that will be column names in the output data.table.
#' @param param Index/Name of the Paramter to get the data, default = 1
#' @param f.header The id.file contain header?, boolean, default = FALSE
#' @param f.sep Seperator of the id.file, default = 'TAB'
#' @param f.comment Commment character of the file, default = '#'
#' @return A data.table
#' @export
his_from_file <- function(
                          his.file, # path to .HIS file
                          id.file, # path to list of sobek.id.list file
                          param = 1L,
                          f.header = FALSE, # does node list contain header
                          f.sep = "\t", # seperation of node list
                          f.comment = '#'
                          ) {
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
                          id.list = as.character(id.list[, 1]),
                          param = param)

  if (ncol(id.list) > 1) {
    colnames(df_out) <- c("ts", id.list[, 2])
  }
  return(df_out)
}
