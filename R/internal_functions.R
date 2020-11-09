# Convert parameter name to parameter index
param_name_2_id <- function(param.name, param.df) {
  # using exact matching to prevent potential problem caused by special characters
  ret <- param.df[grepl(param.name, param_name, ignore.case = TRUE), unique(param_id)]
  if (length(ret) != 1) ret <- NA_integer_ else ret <- as.integer(ret)
  return(ret)
}


#' Get data matrix of the .HIS file
#'
#' @param his.file Path to .HIS file, string
#' @return a numeric matrix with ncol = total_loc*total_param, nrow = total_tstep
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
  his_t0 <- as.POSIXct(his_t0, format = "%Y.%m.%d %H:%M:%S")
  his_dt <- as.integer(stri_match_first_regex(txt_title, dt_pattern)[2])
  his_time <- vector(mode = 'numeric', length = total_tstep)
  seek(con, where = 168 + 20 * param_nr + 24 * total_loc)
  for (i in 1:total_tstep) {
    his_time[i] <- readBin(con, "integer", size = 4) * his_dt
    seek(con, where = 4 * param_nr * total_loc, origin = "current")
  }
  close(con)
  his_time <- as.POSIXct(his_time, origin = his_t0)
  his_time <- data.table(ts = his_time)

  return(his_time)
}


#' Get parameter table from .HIS & .HIA files
#'
#' @param his.file Path to the .HIS file
#' @return a data.table with two column: location & sobek.id
his_parameter <- function(his.file = "") {
  cat("Please use his_info function for all information about HIS file\n")
  hinfo <- his_info(his.file)
  return(hinfo$param_tbl)
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
