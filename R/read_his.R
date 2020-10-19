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
  cat("Please use his_info function for all information about HIS file\n")
  hinfo <- his_info(his.file)
  return(hinfo$loc_tbl)
}


#' Read basic information of the .HIS file
#'
#' @param path Path to the .HIS file
#' @export
his_info <- function(his.file) {
  con <- file(his.file, open = "rb", encoding = "native.enc")
  # first 160 bytes are characters for the title part
  txt_title <- stri_conv(readBin(con, what = "raw", n = 160),
                         from = 'windows-1252')
  his_dt <- as.integer(stri_match_first_regex(txt_title, "scu= *(\\d*)s")[2])
  t0 <- stri_extract_all_regex(txt_title, "\\d{4}.\\d{2}.\\d{2} \\d{2}.\\d{2}.\\d{2}")[[1]]
  # bytes 160-167 are for 2 int
  total_param <- readBin(con, what = "int", size = 4, endian = "little")
  total_loc <- readBin(con, what = "int", size = 4, endian = "little")
  # read parameter names
  params_str <- stri_conv(readBin(con, what = "raw", n = 20 * total_param,
                                  endian = "little"), from = 'windows-1252')
  # each parameter name is stored in a fixed string having length = 20
  param_names <- stri_extract_all_regex(params_str, ".{20}", simplify = TRUE)[1,]
  param_names <- stri_trim_both(param_names)
  param_tbl <- data.table(param_nr = seq_along(param_names), param_name = param_names)
  loc_names <- vector()
  for (i in 1:total_loc) {
    seek(con, 4, "current")
    loc_names[i] <- stri_conv(readBin(con, what = "raw", n = 20),
                              from = 'windows-1252')
    seek(con, where = 168 + 20 * total_param + 24 * i, origin = "start")
  }
  loc_names <- stri_trim_both(loc_names)
  loc_tbl <- data.table(location = seq_along(loc_names), sobek.id = loc_names)
  hia_f <- stri_replace_all_regex(his.file, "\\.his$", "\\.HIA",
                                  opts_regex = stri_opts_regex(TRUE))
  if (file.exists(hia_f)) {
    hiaINI <- ini::read.ini(hia_f, encoding = 'Latin1')
    long_loc <- hiaINI[["Long Locations"]]
    if (!is.null(long_loc)) {
      long_loc_tbl <- data.table(location = as.integer(names(long_loc)),
                                 sobek.id = unlist(long_loc, use.names = FALSE))
      loc_tbl <- rbind(loc_tbl[!location %in% long_loc_tbl$location], long_loc_tbl)
      setorder(loc_tbl, location)
    }
    param_long <- hiaINI[["Long Parameters"]]
    if (!is.null(param_long)) {
      param_long_tbl <- data.table(param_nr = as.integer(names(param_long)),
                                   param_name = unlist(param_long, use.names = FALSE))
      param_tbl <- rbind(param_tbl[!param_nr %in% param_long_tbl$param_nr], param_long_tbl)
      setorder(param_tbl, param_nr)
    }
  }
  param_tbl[, param_name := gsub('water level|w\\.level', 'Waterlevel', ignore.case = TRUE,
                                 param_name)]
  data_loc <- (160 + # for the .HIS information ("title")
                 2 * 4 + # for total parameters & total locations
                 20 * total_param + # for the paramter names
                 24 * total_loc) # location table
  data_bytes <- file.size(his.file) - data_loc
  # int(4) for time, double(4) for data
  total_tstep <- data_bytes / (4 + 4 * total_param * total_loc)
  close(con)
  return(
    structure(
      list(
        t0 = t0,
        dt = his_dt,
        n_ts = total_tstep,
        n_param = total_param,
        n_id = total_loc,
        param_tbl = param_tbl,
        loc_tbl = loc_tbl,
        data_loc = data_loc
      ),
      class = "hisInfo"
    )
  )
}


#' Method for print hisInfo S3 class
#'
#' @export
#' @noRd
print.hisInfo <- function(his) {
  cat("His file information: \n")
  cat("    Number of time steps:      ", his$n_ts,    "\n")
  cat("    Number of parameters:      ", his$n_param, "\n")
  cat("    Number of Sobek IDs:       ", his$n_id,    "\n")
  cat("    Simulation start:          ", his$t0,      "\n")
}


#' Export data for nodes/reaches using IDs from a list
#'
#' @inheritParams his_from_list
#' @return A data.table
his_from_list_1param <- function(his.file, id.list, param = 1, from.ts = 1, n.ts = NULL) {
  id.list <- unlist(id.list, use.names = FALSE)
  stopifnot(length(param) == 1 & length(his.file) == 1)
  con <- file(his.file, open = "rb", encoding = "native.enc")
  on.exit(close(con))
  info_lst <- his_info(his.file)
  if (is.null(n.ts))
    n.ts <- info_lst$n_ts
  if (n.ts > info_lst$n_ts)
    stop("n.ts out of range(1, ", info_lst$n_ts, ")")
  if (!from.ts %in% c(1, n.ts))
    stop("from.ts out of range(1, ", n.ts, ")")
  if (isTRUE(!is.numeric(param))) {
    param <- info_lst$param_tbl[grepl(param, param_name, ignore.case = TRUE),
                                unique(param_nr)]
    if (length(param) != 1) {
      cat("Following are possible parameter names: \n")
      cat(paste(info_lst$param_tbl$param_name, collapse = "\n"))
      stop("Wrong parameter name!\n")
    }
  }
  if (all(id.list == "all")) {
    locs <- unique(info_lst$loc_tbl$location)
    id.list <- info_lst$loc_tbl[locs, sobek.id]
  } else {
    setkey(info_lst$loc_tbl, sobek.id)
    locs <- info_lst$loc_tbl[id.list, location]
  }
  data_1line_size <- (1 + info_lst$n_id * info_lst$n_param) * 4
  start_loc <- info_lst$data_loc + (from.ts - 1) * data_1line_size
  data_mtx <- matrix(nrow = n.ts, ncol = length(locs) + 1)
  cols_mask <- param + info_lst$n_param * (locs - 1)
  seek(con, start_loc, origin = "start")
  for (i in 1:n.ts) {
    data_mtx[i, 1] <- readBin(con, "integer", size = 4, n = 1)
    this_line <- readBin(con, what = "double", size = 4,
                         n = info_lst$n_param * info_lst$n_id
    )
    data_mtx[i, -1] <- this_line[cols_mask]
  }
  ret <- as.data.table(data_mtx)
  colnames(ret) <- c("ts", id.list)
  ret[, ts := as.POSIXct(ts * info_lst$dt, tz = "GMT",
                         origin = as.POSIXct(info_lst$t0, format = "%Y.%m.%d %H:%M:%S"))]
  ret
}


#' Export data for nodes/reaches using IDs from a list
#'
#' This version of his_from_list read more than one parameter at a time
#' @param his.file Path to the .HIS file, string
#' @param id.list List of Sobek IDs
#' @param param Indexes or names of the parameters to get the data, default = 1.
#' @param from.ts Integer. Starting timestep to read
#' @param n.ts Integer. How many timesteps to read
#' @export
#' @return A data.table
his_from_list <- function(his.file, id.list, param = 1, from.ts = 1, n.ts = NULL) {
  if (length(param) <= 1) {
    return(
      his_from_list_1param(his.file = his.file, id.list = id.list, param = param,
                           from.ts = from.ts, n.ts = n.ts)
    )
  }
  id.list <- unlist(id.list, use.names = FALSE)
  stopifnot(length(his.file) == 1)
  con <- file(his.file, open = "rb", encoding = "native.enc")
  on.exit(close(con))
  info_lst <- his_info(his.file)
  if (is.null(n.ts))
    n.ts <- info_lst$n_ts
  if (n.ts > info_lst$n_ts)
    stop("n.ts out of range(1, ", info_lst$n_ts, ")")
  if (!from.ts %in% c(1, n.ts))
    stop("from.ts out of range(1, ", n.ts, ")")
  param_int <- param
  if (isTRUE(!all(is.numeric(param_int)))) {
    for (i in seq_along(param)) {
      param_int[[i]] <- info_lst$param_tbl[grepl(param[[i]], param_name, ignore.case = TRUE),
                                           unique(param_nr)]
      if (length(param_int[[i]]) != 1) {
        cat("Following are possible parameter names: \n")
        cat(paste(info_lst$param_tbl$param_name, collapse = "\n"))
        stop("Wrong parameter name!\n")
      }
    }
  }
  param_int <- as.integer(param_int)
  if (all(id.list == "all")) {
    locs <- unique(info_lst$loc_tbl$location)
    id.list <- info_lst$loc_tbl[locs, sobek.id]
  } else {
    setkey(info_lst$loc_tbl, sobek.id)
    locs <- info_lst$loc_tbl[id.list, location]
  }
  data_1line_size <- (1 + info_lst$n_id * info_lst$n_param) * 4
  start_loc <- info_lst$data_loc + (from.ts - 1) * data_1line_size
  data_mtx <- matrix(nrow = n.ts, ncol = length(locs) * length(param_int) + 1)
  cols_mask <- list()
  for (i in seq_along(param_int)) {
    cols_mask[[i]] <- param_int[[i]] + info_lst$n_param * (locs - 1)
  }
  cols_mask <- unlist(cols_mask)
  seek(con, start_loc, origin = "start")
  for (i in 1:n.ts) {
    data_mtx[i, 1] <- readBin(con, "integer", size = 4, n = 1)
    this_line <- readBin(con, what = "double", size = 4,
                         n = info_lst$n_param * info_lst$n_id
    )
    data_mtx[i, -1] <- this_line[cols_mask]
  }
  ret <- as.data.table(data_mtx)
  colnames(ret)[-1] <- c("ts", as.vector(outer(id.list, param, FUN = paste, sep = "_")))
  ret[, ts := as.POSIXct(ts * info_lst$dt, tz = "GMT",
                         origin = as.POSIXct(info_lst$t0, format = "%Y.%m.%d %H:%M:%S"))]
  ret
}


#' Export data for nodes/reaches using IDs from a list
#'
#'
#' @inheritParams his_from_list
#' @param mask Mask for columns to get data
#' @export
#' @return A matrix
his_from_col_mask <- function(his.file, mask, from.ts = 1, n.ts = NULL) {
  stopifnot(is.vector(mask, mode = "integer"))
  con <- file(his.file, open = "rb", encoding = "native.enc")
  on.exit(close(con))
  info_lst <- his_info(his.file)
  if (is.null(n.ts))
    n.ts <- info_lst$n_ts
  if (n.ts > info_lst$n_ts)
    stop("n.ts out of range(1, ", info_lst$n_ts, ")")
  if (!from.ts %in% c(1, n.ts))
    stop("from.ts out of range(1, ", n.ts, ")")
  data_1line_size <- (1 + info_lst$n_id * info_lst$n_param) * 4
  start_loc <- info_lst$data_loc + (from.ts - 1) * data_1line_size
  data_mtx <- matrix(nrow = n.ts, ncol = length(mask) + 1)
  seek(con, start_loc, origin = "start")
  for (i in 1:n.ts) {
    data_mtx[i, 1] <- readBin(con, "integer", size = 4, n = 1)
    this_line <- readBin(con, what = "double", size = 4,
                         n = info_lst$n_param * info_lst$n_id
    )
    data_mtx[i, -1] <- this_line[mask]
  }
  data_mtx[, 1] <- data_mtx[, 1] * info_lst$dt
  data_mtx
}


#' Get column indexex in the data matrix for IDs
#'
#' @param his.info hisFile class
#' @inheritParams his_from_list
#' @return an integer vector. NA indicates that there is no data for the IDs in the HIS file.
get_column_index <- function(
  his.info, id.list, param = 1, set.key = FALSE
) {
  if (set.key)
    setkey(his.info$loc_tbl, sobek.id)
  locs <- his.info$loc_tbl[id.list, location]
  param_int <- param
  if (isTRUE(!all(is.numeric(param_int)))) {
    for (i in seq_along(param)) {
      param_int[[i]] <- his.info$param_tbl[grepl(param[[i]], param_name, ignore.case = TRUE),
                                           unique(param_nr)]
      if (length(param_int[[i]]) != 1) {
        cat("Following are possible parameter names: \n")
        cat(paste(his.info$param_tbl$param_name, collapse = "\n"))
        stop("Wrong parameter name!\n")
      }
    }
  }
  param_int <- as.integer(param_int)
  cols_mask <- list()
  for (i in seq_along(param_int)) {
    cols_mask[[i]] <- param_int[[i]] + his.info$n_param * (locs - 1)
  }
  cols_mask <- as.integer(unlist(cols_mask))
  cols_mask
}
