# Contains all internal functions


################################################################################
# Return location for given node/reach ID
# @param sobek.id Node/Reach ID
# @param his.locs Location table
.id2loc <- function(sobek.id, his.locs) {
  location <- NA_integer_
  # using exact matching to prevent potential problem caused by special characters
  if(sobek.id %in% his.locs$sobek.id){
    location <- his.locs$location[his.locs$sobek.id == sobek.id]
  } else {
      if ("long.id" %in% colnames(his.locs)){
        location <- ifelse(sobek.id %in% his.locs$long.id,
                           yes = his.locs$location[his.locs$long.id == sobek.id],
                           no = NA_integer_
        )
      } else {
        location <- NA_integer_
      }
  }
  if (is.na(location)) warning("sobek.id: '", sobek.id,
                               "' does not found in the location table")
  return(as.integer(location))
  }


################################################################################
# Return location for given node/reach ID
.get_case_number <- function(case.name, case.list) {
  if (typeof(case.list) != "list") stop("case.list must be a list")
  if (case.name %in% case.list$case_name){
    x <- case.list$case_number[case.list$case_name == case.name]
    return(x)
  } else{
    warning("case ", case.name, " not found in the case list")
    return(NA_integer_)
  }
}


################################################################################
# Return data matrix of the .HIS file for one parameter
# @param his.file Path to .HIS file, string
# @param param Index of the parameter, integer
# @return a numeric matrix with ncol = total_loc, nrow = total_tstep
.his_df <- function(his.file, param = 1L) {
  # param: index of the parameter
  if (!file.exists(his.file)) {
    stop(paste("HIS file:", his.file, "does not exit!"))
  }
  con <- file(his.file, open = "rb", encoding = "native.enc")
  # check .HIS file simple way
  his_title <- readBin(con, "character", size = 160, endian = "little")
  if (length(his_title) == 0) {
    close(con)
    stop(paste("HIS file:", his.file, "has wrong format"))
  }
  if (!grepl("SOBEK[[:space:]]{1,}", his_title)) {
    close(con)
    stop(paste("HIS file:", his.file, "has wrong format"))
  }
  # get the total bytes of the his file
  his_fsize <- file.size(his.file)
  # get number of parameters and number of locations
  seek(con, 160)
  param_nr <- readBin(con, "int", size = 4, endian = "little")
  total_loc <- readBin(con, "int", size = 4, endian = "little")
  seek(con, 168)
  # read parameter names
  params_str <- readBin(con, what = "character", size = 20*param_nr,
                        endian = "little")
  # each parameter name is stored in a fixed string having length = 20
  param_names <- vector(mode = "character", length = param_nr)
  # removing padding strings at the end
  for (i in 1:param_nr) param_names[i] <- substr(params_str,
                                                 start = 20*(i - 1) + 1,
                                                 stop = 20*i)
  if (is.numeric(param)) param <- as.integer(param)
  if (is.integer(param)){
    if (param > param_nr) stop("param cannot bigger than total parameters")
    work_param <- param
  } else{
    if (!is.character(param)) stop("param must be given as the index (integer)",
                                   " or as name (characters)")
    work_param <- grep(tolower(param), tolower(param_names), fixed = TRUE)
    if (length(work_param) == 0) stop("Parameter: ", param, " not found")
    if (length(work_param) > 1) stop("Parameter: ", param, " is ambiguous")
  }
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
  dt_pattern <- "scu=[[:space:]]{1,}([0-9]{1,})s"
  his_t0 <- regmatches(his_title, regexpr(t0_pattern, his_title))
  his_t0 <- as.POSIXct(his_t0, format = "%Y.%m.%d %H:%M:%S", tz = "GMT")
  his_dt <- as.integer(gsub(
    dt_pattern,
    "\\1",
    regmatches(
      his_title,
      gregexpr(dt_pattern, his_title)
    )[[1]]
  ))
  his_lines <- matrix(nrow = total_tstep, ncol = param_nr * total_loc)
  his_time <- matrix(
    nrow = total_tstep, ncol = 1,
    dimnames = list(list(), "ts")
  )
  seek(con, where = 168 + 20 * param_nr + 24 * total_loc)
  # this reading can be done with chunk = max 1000.
  for (i in 1:total_tstep) {
    seek(con, where = 4, origin = "current")
    his_lines[i, ] <- readBin(con, what = "double", size = 4,
                              n = param_nr * total_loc,
                              endian = "little"
    )
  }
  close(con)
  # creating a mask for the matrix, to get only columns for the param
  # print(paste("Param given: ", param, "Param index: ", work_param))
  his_mask <- seq.int(
    from = work_param,
    to = param_nr * total_loc,
    by = param_nr
  )
  his_lines <- his_lines[, his_mask]
  return(his_lines)
}


.his_df2 <- function(his.file, param = 1L) {
  # param: index of the parameter
  if (!file.exists(his.file)) {
    stop(paste("HIS file:", his.file, "does not exit!"))
  }
  con <- file(his.file, open = "rb", encoding = "native.enc")
  # check .HIS file simple way
  his_title <- readBin(con, "character", size = 160, endian = "little")
  if (length(his_title) == 0) {
    close(con)
    stop(paste("HIS file:", his.file, "has wrong format"))
  }
  if (!grepl("SOBEK[[:space:]]{1,}", his_title)) {
    close(con)
    stop(paste("HIS file:", his.file, "has wrong format"))
  }
  # get the total bytes of the his file
  his_fsize <- file.size(his.file)
  # get number of parameters and number of locations
  seek(con, 160)
  param_nr <- readBin(con, "int", size = 4, endian = "little")
  total_loc <- readBin(con, "int", size = 4, endian = "little")
  seek(con, 168)
  # read parameter names
  params_str <- readBin(con, what = "character", size = 20*param_nr,
                        endian = "little")
  # each parameter name is stored in a fixed string having length = 20
  param_names <- vector(mode = "character", length = param_nr)
  # removing padding strings at the end
  for (i in 1:param_nr) param_names[i] <- substr(params_str,
                                                 start = 20*(i - 1) + 1,
                                                 stop = 20*i)
  if (is.numeric(param)) param <- as.integer(param)
  if (is.integer(param)){
    if (param > param_nr) stop("param cannot bigger than total parameters")
    work_param <- param
  } else{
    if (!is.character(param)) stop("param must be given as the index (integer)",
                                   " or as name (characters)")
    work_param <- grep(tolower(param), tolower(param_names), fixed = TRUE)
    if (length(work_param) == 0) stop("Parameter: ", param, " not found")
    if (length(work_param) > 1) stop("Parameter: ", param, " is ambiguous")
  }
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
  dt_pattern <- "scu=[[:space:]]{1,}([0-9]{1,})s"
  his_t0 <- regmatches(his_title, regexpr(t0_pattern, his_title))
  his_t0 <- as.POSIXct(his_t0, format = "%Y.%m.%d %H:%M:%S", tz = "GMT")
  his_dt <- as.integer(gsub(
    dt_pattern,
    "\\1",
    regmatches(
      his_title,
      gregexpr(dt_pattern, his_title)
    )[[1]]
  ))
  his_lines <- matrix(nrow = total_tstep, ncol = param_nr * total_loc + 1)
  his_time <- matrix(
    nrow = total_tstep, ncol = 1,
    dimnames = list(list(), "ts")
  )
  seek(con, where = 168 + 20 * param_nr + 24 * total_loc)
  # this reading can be done with chunk = max 1000.
  for (i in 1:total_tstep) {
    # seek(con, where = 4, origin = "current")
    his_lines[i, ] <- readBin(con, what = "double", size = 4,
                              n = param_nr * total_loc + 1,
                              endian = "little"
    )
  }
  close(con)
  # creating a mask for the matrix, to get only columns for the param
  # print(paste("Param given: ", param, "Param index: ", work_param))
  his_mask <- seq.int(
    from = work_param + 1,
    to = param_nr * total_loc + 1,
    by = param_nr
  )
  his_lines <- his_lines[, his_mask]
  return(his_lines)
}

################################################################################
# Return data matrix of the .HIS file for one parameter
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
  his_time <- matrix(nrow = total_tstep, ncol = 1)
  seek(con, where = 168 + 20 * param_nr + 24 * total_loc)
  # this reading can be done with chunk = max 1000.
  for (i in 1:total_tstep) {
    his_time[i, 1] <- readBin(con, "integer", size = 4, endian = "little") * his_dt
    seek(con, where = 4 * param_nr * total_loc, origin = "current")
  }
  close(con)
  # his_time <- as.POSIXct(his_time, origin = his_t0)
  his_time <- as.POSIXct(his_time, origin = his_t0, tz = "GMT")
  colnames(his_time) <- c("ts")

  return(his_time)
}


################################################################################
# Return data matrix of the .HIS file for one parameter
# @param his.file Path to .HIS file, string
# @param param Index of the parameter, integer
# @return a numeric matrix with ncol = total_loc, nrow = total_tstep
.his_id_df <- function(his.file, id, param = 1L) {
  # param: index of the parameter
  if (!file.exists(his.file)) {
    stop(paste("HIS file:", his.file, "does not exit!"))
  }
  con <- file(his.file, open = "rb", encoding = "native.enc")
  # check .HIS file simple way
  his_title <- readBin(con, "character", size = 160, endian = "little")
  if (length(his_title) == 0) {
    close(con)
    stop(paste("HIS file:", his.file, "has wrong format"))
  }
  if (!grepl("SOBEK[[:space:]]{1,}", his_title)) {
    close(con)
    stop(paste("HIS file:", his.file, "has wrong format"))
  }
  # get the total bytes of the his file
  his_fsize <- file.size(his.file)
  # get number of parameters and number of locations
  seek(con, 160)
  param_nr <- readBin(con, "int", size = 4, endian = "little")
  total_loc <- readBin(con, "int", size = 4, endian = "little")
  seek(con, 168)
  # read parameter names
  params_str <- readBin(con, what = "character", size = 20*param_nr,
                        endian = "little")
  # each parameter name is stored in a fixed string having length = 20
  param_names <- vector(mode = "character", length = param_nr)
  # removing padding strings at the end
  for (i in 1:param_nr) param_names[i] <- substr(params_str,
                                                 start = 20*(i - 1) + 1,
                                                 stop = 20*i)
  if (is.numeric(param)) param <- as.integer(param)
  if (is.integer(param)){
    if (param > param_nr) stop("param cannot bigger than total parameters")
    work_param <- param
  } else{
    if (!is.character(param)) stop("param must be given as the index (integer)",
                                   " or as name (characters)")
    work_param <- grep(tolower(param), tolower(param_names), fixed = TRUE)
    if (length(work_param) == 0) stop("Parameter: ", param, " not found")
    if (length(work_param) > 1) stop("Parameter: ", param, " is ambiguous")
  }
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
  dt_pattern <- "scu=[[:space:]]{1,}([0-9]{1,})s"
  his_t0 <- regmatches(his_title, regexpr(t0_pattern, his_title))
  his_t0 <- as.POSIXct(his_t0, format = "%Y.%m.%d %H:%M:%S", tz = "GMT")
  his_dt <- as.integer(gsub(
    dt_pattern,
    "\\1",
    regmatches(
      his_title,
      gregexpr(dt_pattern, his_title)
    )[[1]]
  ))

  #
  his_lines <- matrix(nrow = total_tstep, ncol = length(id))
  # his_time <- matrix(
  #   nrow = total_tstep, ncol = 1,
  #   dimnames = list(list(), "ts")
  # )
  # position of the data matrix in the binary his.file
  dmtx_pos <- 168 + 20 * param_nr + 24 * total_loc
  seek(con, where = dmtx_pos, origin = "start")
  # reading data for the ids
  loc_tbl <- his_location(his.file)
  id.location <- .id2loc(id, his.locs = loc_tbl)
  for (i in 1:total_tstep) {
    # seek(con, where = dmtx_pos + 4*(total_loc*param_nr + 1)*(i-1),
    #      origin = "start")
    # by pass the first integer for time
    seek(con, where = 4, origin = "current")
    loc_offset <- 4*(id.location - 1)*param_nr + 4*(work_param - 1)
    seek(con, where = loc_offset, origin = "current")
    his_lines[i, 1] <- readBin(con, what = "double", size = 4,
                                  n = 1L,
                                  endian = "little"
                                  )
    seek(con,
         where = dmtx_pos + (4 + 4*total_loc*param_nr)*i,
         origin = "start"
    )
  }
  close(con)
  # creating a mask for the matrix, to get only columns for the param
  # print(paste("Param given: ", param, "Param index: ", work_param))
  # his_mask <- seq.int(
  #   from = work_param,
  #   to = param_nr * total_loc,
  #   by = param_nr
  # )
  # his_lines <- his_lines[, his_mask]
  return(his_lines)
}

################################################################################
# Return data matrix of the .HIS file for one parameter
# @param his.file Path to .HIS file, string
# @param param Index of the parameter, integer
# @return a numeric matrix with ncol = total_loc, nrow = total_tstep
.his_id_list_df <- function(his.file, id.list, param = 1L) {
  if (!is.vector(id.list)) stop("id.list must be a vector")
  # param: index of the parameter
  if (!file.exists(his.file)) {
    stop(paste("HIS file:", his.file, "does not exit!"))
  }
  con <- file(his.file, open = "rb", encoding = "native.enc")
  # check .HIS file simple way
  his_title <- readBin(con, "character", size = 160, endian = "little")
  if (length(his_title) == 0) {
    close(con)
    stop(paste("HIS file:", his.file, "has wrong format"))
  }
  if (!grepl("SOBEK[[:space:]]{1,}", his_title)) {
    close(con)
    stop(paste("HIS file:", his.file, "has wrong format"))
  }
  # get the total bytes of the his file
  his_fsize <- file.size(his.file)
  # get number of parameters and number of locations
  seek(con, 160)
  param_nr <- readBin(con, "int", size = 4, endian = "little")
  total_loc <- readBin(con, "int", size = 4, endian = "little")
  seek(con, 168)
  # read parameter names
  params_str <- readBin(con, what = "character", size = 20*param_nr,
                        endian = "little")
  # each parameter name is stored in a fixed string having length = 20
  param_names <- vector(mode = "character", length = param_nr)
  # removing padding strings at the end
  for (i in 1:param_nr) param_names[i] <- substr(params_str,
                                                 start = 20*(i - 1) + 1,
                                                 stop = 20*i)
  if (is.numeric(param)) param <- as.integer(param)
  if (is.integer(param)){
    if (param > param_nr) stop("param cannot bigger than total parameters")
    work_param <- param
  } else{
    if (!is.character(param)) stop("param must be given as the index (integer)",
                                   " or as name (characters)")
    work_param <- grep(tolower(param), tolower(param_names), fixed = TRUE)
    if (length(work_param) == 0) stop("Parameter: ", param, " not found")
    if (length(work_param) > 1) stop("Parameter: ", param, " is ambiguous")
  }
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
  dt_pattern <- "scu=[[:space:]]{1,}([0-9]{1,})s"
  his_t0 <- regmatches(his_title, regexpr(t0_pattern, his_title))
  his_t0 <- as.POSIXct(his_t0, format = "%Y.%m.%d %H:%M:%S", tz = "GMT")
  his_dt <- as.integer(gsub(
    dt_pattern,
    "\\1",
    regmatches(
      his_title,
      gregexpr(dt_pattern, his_title)
    )[[1]]
  ))

  #
  his_lines <- matrix(nrow = total_tstep, ncol = length(id.list))
  # his_time <- matrix(
  #   nrow = total_tstep, ncol = 1,
  #   dimnames = list(list(), "ts")
  # )
  # position of the data matrix in the binary his.file
  dmtx_pos <- 168 + 20 * param_nr + 24 * total_loc
  # seek(con, where = dmtx_pos, origin = "start")
  # reading data for the ids
  loc_tbl <- his_location(his.file)
  id.location <- lapply(id.list,
                        FUN = .id2loc, his.locs = loc_tbl)
  for (i in 1:total_tstep) {
    # seek(con, where = dmtx_pos + 4*(total_loc*param_nr + 1)*(i-1),
    #      origin = "start")
    # by pass the first integer for time
    # read data for each id

    pos_step_i <- dmtx_pos + # position of first line
      4*i + # first int for ts step
      4*total_loc*param_nr*(i - 1) # size of one data line
    # move to line i
    seek(con, where = pos_step_i, origin = "start")
    for (j in seq_along(id.list)){
      loc_offset <- 4*(id.location[[j]] - 1)*param_nr + 4*(work_param - 1)
      seek(con, where = loc_offset + pos_step_i,
           origin = "start")
      his_lines[i, j] <- readBin(con, what = "double", size = 4,
                                 n = 1L,
                                 endian = "little"
                                 )
    }
  }
  close(con)
  return(his_lines)
}
