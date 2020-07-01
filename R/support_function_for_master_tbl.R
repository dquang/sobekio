#' Parse case name to get standard information
#' @param case.desc Case description (standard naming)
#' @param orig.name Case original name
#' @return a data.table
#' @export
parse_case <- function(case.desc, orig.name = case.desc, stringAsFactors = FALSE){
  stopifnot(length(case.desc) == length(orig.name))
  case_tbl <- data.table(
    case = orig.name,
    case_desc = case.desc
  )
  case_tbl[, zustand := str_match(case.desc, '^([^_]+)')[, 2]]
  case_tbl[, zielpegel := str_match(case.desc, '^[^_]+_([^_]+)_')[, 2]]
  case_tbl[, hwe := str_match(case.desc, '^[^_]+_[^_]+_([^_]+)_')[, 2]]
  case_tbl[, vgf := str_match(case.desc, '^[^_]+_[^_]+_[^_]+_([^_]+)')[, 2]]
  case_tbl[, notiz := str_match(case.desc, '^[^_]+_[^_]+_[^_]+_[^_]+_(.+)')[, 2]]
  if (any(is.na(case_tbl[, c('zustand', 'zielpegel', 'hwe', 'vgf')]))) {
    stop('At least one of case description has wrong format')
  }
  if (isTRUE(stringAsFactors)) {
    id_levels <- seq_along(case_tbl$zustand)
    f_levels <- purrr::map2(case_tbl$zustand, id_levels, paste, sep = '_')
    case_tbl$zustand  <- factor(case_tbl$zustand,
                              levels = f_levels,
                              labels = case_tbl$zustand,
                              ordered = TRUE)
  }
  return(case_tbl)
}

#' Parse case name to get standard information
#' @param case.desc Case description (standard naming)
#' @param orig.name Case original name
#' @return a data.table
#' @export
parse_case2 <- function(case.desc, orig.name = case.desc, stringAsFactors = FALSE){
  stopifnot(length(case.desc) == length(orig.name))
  case_tbl <- data.table(
    case = orig.name,
    case_desc = case.desc
  )
  case_tbl[, Zustand := str_match(case.desc, '^([^_]+)')[, 2]]
  case_tbl[, Zielpegel := str_match(case.desc, '^[^_]+_([^_]+)_')[, 2]]
  case_tbl[, HWE := str_match(case.desc, '^[^_]+_[^_]+_([^_]+)_')[, 2]]
  case_tbl[, VGF := str_match(case.desc, '^[^_]+_[^_]+_[^_]+_([^_]+)')[, 2]]
  case_tbl[, notiz := str_match(case.desc, '^[^_]+_[^_]+_[^_]+_[^_]+_(.+)')[, 2]]
  if (any(is.na(case_tbl[, c('Zustand', 'Zielpegel', 'HWE', 'VGF')]))) {
    stop('At least one of case description has wrong format')
  }
  if (isTRUE(stringAsFactors)) {
    id_levels <- seq_along(case_tbl$Zustand)
    f_levels <- purrr::map2(case_tbl$Zustand, id_levels, paste, sep = '_')
    case_tbl$Zustand  <- factor(case_tbl$Zustand,
                                levels = f_levels,
                                labels = case_tbl$Zustand,
                                ordered = TRUE)
  }
  return(case_tbl)
}

#' Get id tables for one measure from the master.tbl
#' @param name Name of the measure
#' @param case.list List of the cases
#' @param case.desc Case name standardized
#' @param master.tbl Master table
#' @return a data.table
#' @export
get_id_tbl <- function(
  name = NULL,
  case.list = NULL,
  case.desc = case.list,
  drv = 'auto',
  to.upstream = 0,
  to.downstream = 0,
  master.tbl
){
  case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
  id_tbl <- master.tbl[grepl(name, besonderheit)]

  if (drv == 'auto') {
    st_mtx <- str_match(id_tbl$besonderheit,
                            paste('(^.+)_', name, sep = '')
                            )
    measure_type <- toupper(unique(st_mtx[,2]))
      # warning('Type of the measure is not unique: ', measure_type)
    drv <- ifelse('DRV' %in% measure_type, TRUE, FALSE)
  }
  if (isTRUE(drv)) {
    drv_begin <- id_tbl[grepl('.+_Begin', besonderheit), km][[1]]
    drv_end <- id_tbl[grepl('.+_End', besonderheit), km][[1]]
    if (drv_begin < drv_end){
      drv_begin <- drv_begin - to.upstream
      drv_end <- drv_end + to.downstream
    } else{
      drv_begin <- drv_begin + to.upstream
      drv_end <- drv_end - to.downstream
    }
    km_range <- sort(c(drv_begin, drv_end))
    drv_river <- id_tbl[grepl('.+_Begin', besonderheit), river][[1]]
    id_tbl <- master.tbl[river == drv_river &
                           km >= km_range[1] &
                           km <= km_range[2]]
  }
  if (nrow(id_tbl) == 0) stop('Found no IDs for name: ', name, ' in the master.tbl')
  # creating new columns based on case name
  for (i in seq_along(case.desc)) {
    col_n <- paste('case_desc', i, sep = '_')
    id_tbl[, eval(col_n) := case.desc[[i]]]
  }
  measure_vars <- paste('case_desc', seq_along(case.desc), sep = '_')
  id_tbl <- melt(id_tbl, measure.vars = measure_vars,
                 value.name = 'case_desc', sort = FALSE)
  id_tbl$variable <- NULL
  id_tbl <- merge(id_tbl, case_tbl[, c('case', 'case_desc', 'zustand')],
                  by = 'case_desc', sort = FALSE)
  # check if the zustand in cases are in listed in the master.tbl
  zustand_in_cases <- unique(as.character(case_tbl$zustand))
  id_tbl[, ID_F := ID]
  id_tbl_cols <- colnames(id_tbl)
  for (i in seq_along(zustand_in_cases)) {
    if (zustand_in_cases[i] %in% id_tbl_cols) {
      # get final, correct ID for each case
      id_tbl[zustand == zustand_in_cases[i] & nchar(get(zustand_in_cases[i])) > 0,
             ID_F := get(zustand_in_cases[i])]
    }

  }
  id_tbl$zustand <- NULL
  id_tbl$case_desc <- NULL
  return(id_tbl)
}


#' Get id tables for one segment of a river
#' @param river Name of the river
#' @param from.km Start location (km)
#' @param to.km End location (km)
#' @param case.list List of the cases
#' @param case.desc Case name standardized
#' @param master.tbl Master table
#' @param param Parameter
#' @return a data.table
#' @export
get_segment_id_tbl <- function(
  river = NULL,
  from.km = -Inf,
  to.km = Inf,
  case.list = NULL,
  case.desc = case.list,
  master.tbl = NULL,
  param
){
  param <- tolower(param)
  stopifnot(is.numeric(from.km) & is.numeric(to.km))
  stopifnot(from.km < to.km)
  # get the main river that has the most IDs if river == NULL
  river_ids <- master.tbl[, .N, by = river]
  if (is.null(river)) {
    setorder(river_ids, -N)
    river <- river_ids$river[[1]]
  } else{
    stopifnot(river %in% river_ids$river)
  }
  case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
  river_name <- river # because it is confused with the 'river' column
  id_tbl <- master.tbl[river == river_name &
                         km >= from.km & km <= to.km & !is.na(km)
                       ]
  stopifnot(nrow(id_tbl) > 1)
  # creating new columns based on case name
  for (i in seq_along(case.desc)) {
    col_n <- paste('case_desc', i, sep = '_')
    id_tbl[, eval(col_n) := case.desc[[i]]]
  }
  measure_vars <- paste('case_desc', seq_along(case.desc), sep = '_')
  id_tbl <- melt(id_tbl, measure.vars = measure_vars,
                 value.name = 'case_desc', sort = FALSE)
  id_tbl$variable <- NULL
  id_tbl <- merge(id_tbl, case_tbl[, c('case', 'case_desc', 'zustand')],
                  by = 'case_desc', sort = FALSE)
  # check if the zustand in cases are in listed in the master.tbl
  zustand_in_cases <- unique(as.character(case_tbl$zustand))
  id_tbl[, ID_F := ID]
  id_tbl_cols <- colnames(id_tbl)
  for (i in seq_along(zustand_in_cases)) {
    if (zustand_in_cases[i] %in% id_tbl_cols) {
      # get final, correct ID for each case
      id_tbl[zustand == zustand_in_cases[i] & nchar(get(zustand_in_cases[i])) > 0,
             ID_F := get(zustand_in_cases[i])]
    }

  }
  id_tbl$zustand <- NULL
  id_tbl$case_desc <- NULL
  if (param == 'waterlevel') {
    id_tbl <- id_tbl[ID_TYPE == 'wID']
  } else if (param == 'discharge') {
    id_tbl <- id_tbl[ID_TYPE == 'qID']
  } else {
    stop('wrong parameter.')
  }
  return(id_tbl)
}


#' Get data table for a river segment
#' @param river Name of the river
#' @param from.km Start location (km)
#' @param to.km End location (km)
#' @param case.list List of the cases
#' @param case.desc Case name standardized
#' @param param Parameter discharge/waterlevel
#' @param sobek.project Path to sobek project
#' @param to.upstream Distance to upstream (km) to get data
#' @param to.downstream Distance to downstream (km) to get data
#' @param get.max Should max value or whole time series return? Default only max value
#' @param master.tbl Master table
#' @param get.max Should maximal value return or whole time series? Default only max value.
#' @param verbose Should some message be displayed?
#' @param do.par If TRUE, parallel computing will be executed
#' @param ts.trim.left Default NULL. Number of days from the begining of simulation time to remove from timeseries (useful to remove "cold start period")
#' @param remove.inf Remove ID that does not have data
#' @return a data.table
#' @export
get_segment_data <- function(
  river = NULL,
  from.km = -Inf,
  to.km = Inf,
  case.list,
  case.desc = case.list,
  param,
  sobek.project,
  get.max = TRUE,
  master.tbl,
  agg.fun = NULL,
  verbose = TRUE,
  do.par = FALSE,
  ts.trim.left = NULL,
  donau.wehr = FALSE,
  remove.inf = FALSE
) {
  param <- tolower(param)
  if (!is.null(agg.fun)) get.max = FALSE
  id_tbl <- get_segment_id_tbl(
    river = river,
    from.km = from.km,
    to.km = to.km,
    case.list = case.list,
    case.desc = case.desc,
    master.tbl = master.tbl,
    param = param
  )
  if (nrow(id_tbl) == 0) stop('No ID found! Check master.tbl, name of river or from.km, to.km')
  # param == 'waterlevel' -> does not accept duplicated KM in the same case
  if (param != 'discharge') {
    for (i in seq_along(case.list)) {
      # get list of qIDs for this case
      wid = id_tbl[case == case.list[[i]], list(km, ID_F)]
      # finding the IDs that have duplicated km
      km_dup = wid[duplicated(km), km]
      if (length(km_dup) > 0) {
        warning('There are more than one wIDs for the same KM at KM(s): ',
                paste(km_dup, collapse = ', '),
                '. These KM(s) will be remove out of the graphic')
        id_tbl <- id_tbl[!km %in% km_dup]
      }
    }
  }
  # parallel reading data from cases
  if (isTRUE(do.par)) {
    if (isTRUE(verbose)) {
      cat(paste('Getting data for',
                  round(nrow(id_tbl)/length(case.list)),
                  'ID(s) in', length(case.list), 'case(s)\n'))
      cat('Your computer will be overloaded for a short time. Please be patient...\n')
    }
    doParallel::registerDoParallel(parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    segment_data_list <-
      foreach::foreach(i = 1:length(case.list)) %dopar% {
      if (param == 'discharge') {
          tmp <- his_from_case(
            case.list = case.list[[i]],
            sobek.project = sobek.project,
            param = param,
            qID = id_tbl[case == case.list[[i]], ID_F],
            verbose = FALSE
          )
      } else{
          tmp <- his_from_case(
            case.list = case.list[[i]],
            sobek.project = sobek.project,
            param = param,
            wID = id_tbl[case == case.list[[i]], ID_F],
            verbose = FALSE
          )
      }
      tmp
    }
    doParallel::stopImplicitCluster()
  } else{
    segment_data_list <- list(rep(NA, length(case.list)))
    if (isTRUE(verbose)) {
      cat(paste('Getting data for',
                  round(nrow(id_tbl)/length(case.list)),
                  'ID(s) in', length(case.list), 'case(s)\n'))
      cat('Try it with do.par = TRUE if you have more than one case and so many IDs\n')
      cat('Please be patient...\n')
    }
    if (param == 'discharge') {
      for (i in seq_along(case.list)) {
        segment_data_list[[i]] <- his_from_case(
          case.list = case.list[[i]],
          sobek.project = sobek.project,
          param = param,
          qID = id_tbl[case == case.list[[i]], ID_F],
          verbose = FALSE
        )
      }
    } else{
      for (i in seq_along(case.list)) {
        segment_data_list[[i]] <- his_from_case(
          case.list = case.list[[i]],
          sobek.project = sobek.project,
          param = param,
          wID = id_tbl[case == case.list[[i]], ID_F],
          verbose = FALSE
        )
      }
    }
  }
  segment_data <- rbindlist(segment_data_list, use.names = TRUE, fill = TRUE)
  rm(segment_data_list)
  # processing river segment that are divided in two or more branches
  # to sum discharge at reaches that have same chainages along branches together
  q_dup_new_col <- vector(mode = 'character')
  q_dup_id <- vector(mode = 'character')
  if (param == 'discharge') {
    for (i in seq_along(case.list)) {
      # get list of qIDs for this case
      qid <- id_tbl[case == case.list[[i]], list(km, ID_F)]
      # finding the IDs that have duplicated km
      km_dup <- qid[duplicated(km), km]
      for (k in km_dup) {
        qid_k <- qid[km == k, ID_F]
        segment_data[case == case.list[[i]],
                     eval(qid_k[1]) := rowSums(.SD, na.rm = TRUE),
                     .SDcols = qid_k
                     ]
      }
      # we have to run this for twice, to avoid removing ID
      # that are used more than one time for many km
      for (k in km_dup) {
        qid_k <- qid[km == k, ID_F]
        segment_data[case == case.list[[i]],
                     c(qid_k[-1]) := as.list(rep(NA, length(qid_k[-1])))
                     ]
      }
    }
  }
  if (!is.null(ts.trim.left)) {
    segment_data[, ts_left := min(ts, na.rm = TRUE) +
                   ts.trim.left * 3600 * 24, by = case]
    segment_data <- segment_data[ts >= ts_left]
    segment_data[, ts_left := NULL]
  }
  if (donau.wehr & param == "waterlevel") {
    # manually defined peak area of the HW2013 and HW2005
    ts_2005_begin <-  c(as.POSIXct("2005-07-01 00:00", tz = "GMT"),
                        as.POSIXct("2005-08-20 23:00", tz = "GMT"))
    ts_2005_end <-  c(as.POSIXct("2005-08-29 00:00", tz = "GMT"),
                      as.POSIXct("2005-12-31 00:00", tz = "GMT"))
    ts_2013_begin <- c(as.POSIXct("2013-05-01 00:00", tz = "GMT"),
                       as.POSIXct("2013-06-03 00:00", tz = "GMT"))
    ts_2013_end <- c(as.POSIXct("2013-06-04 00:00", tz = "GMT"),
                     as.POSIXct("2013-12-31 00:00", tz = "GMT"))
    for (a_case in case.list) {
      if (grepl("2013|2005", case.desc[case.list == a_case])) {
        wehr_ids <- id_tbl[km %between% c(2203, 2212) & ID_TYPE == "wID" &
                             case == a_case, ID_F]
        segment_data[ts %between% ts_2013_begin &
                       case == a_case, (wehr_ids) := NA]
        segment_data[ts %between% ts_2005_begin &
                       case == a_case, (wehr_ids) := NA]
        segment_data[ts %between% ts_2013_end &
                       case == a_case, (wehr_ids) := NA]
        segment_data[ts %between% ts_2005_end &
                       case == a_case, (wehr_ids) := NA]
      }
    }
  }
  if (isTRUE(get.max)) {
    if (isTRUE(verbose)) cat('Calculating max values....\n')
    segment_data <- segment_data[, lapply(.SD, max, na.rm = TRUE),
                         .SDcols = -c('ts'), by = case] %>%
      melt(id.vars = 'case', variable.name = 'ID_F', value.name = 'scheitel')
    # removing ID_F that were aggregated to the first ID_F at the duplicated KM
    if (param == 'discharge') {
      for (i in seq_along(case.list)) {
        # get list of qIDs for this case
        qid = id_tbl[case == case.list[[i]], list(km, ID_F)]
        # finding the IDs that have duplicated km
        km_dup = qid[duplicated(km), km]
        for (k in km_dup) {
          qid_k <- qid[km == k, ID_F]
          segment_data <- segment_data[!(case == case.list[[i]] &
                                           ID_F %in% qid_k[-1])]
        }
      }
    }
    if (remove.inf) {
      segment_data <- segment_data[!is.infinite(scheitel)]
    } else {
      segment_data[is.infinite(scheitel), scheitel := NA]
    }
    segment_data <- merge(
      segment_data,
      id_tbl[, .SD, .SDcols = c('ID_F', 'besonderheit', 'km', 'river', 'case')],
      by = c('case', 'ID_F'),
      sort = FALSE
    )
  }
  #TODO: rewrite, remove get.max
  if (!is.null(agg.fun)) {
    if (isTRUE(verbose)) cat('Aggregating values....\n')
    segment_data <- segment_data[, lapply(.SD, agg.fun, na.rm = TRUE),
                                 .SDcols = -c('ts'), by = case] %>%
      melt(id.vars = 'case', variable.name = 'ID_F', value.name = 'scheitel')
    # removing ID_F that were aggregated to the first ID_F at the duplicated KM
    if (param == 'discharge') {
      for (i in seq_along(case.list)) {
        # get list of qIDs for this case
        qid = id_tbl[case == case.list[[i]], list(km, ID_F)]
        # finding the IDs that have duplicated km
        km_dup = qid[duplicated(km), km]
        for (k in km_dup) {
          qid_k <- qid[km == k, ID_F]
          segment_data <- segment_data[!(case == case.list[[i]] &
                                           ID_F %in% qid_k[-1])]
        }
      }
    }
    if (remove.inf) {
      segment_data <- segment_data[!is.infinite(scheitel)]
    } else {
      segment_data[is.infinite(scheitel), scheitel := NA]
    }
    id_tbl_cols <- colnames(id_tbl)
    id_tbl_cols <- id_tbl_cols[id_tbl_cols %in% c('ID_F', 'besonderheit', 'km', 'river', 'case')]
    segment_data <- merge(
      segment_data,
      id_tbl[, .SD, .SDcols = id_tbl_cols],
      by = c('case', 'ID_F'),
      sort = FALSE
    )
  }

  return(segment_data)
}


#' Get related time series for a measure
#' @param name Name of the measure
#' @param case.list List of the cases
#' @param case.desc Case name standardized
#' @param param Parameter discharge/waterlevel
#' @param sobek.project Path to sobek project
#' @param w.canal Should w.canal be inculded?
#' @param volume Should volumn (Mio. m³) be inculded?
#' @param master.tbl Master table
#' @param verbose Should some message be displayed?
#' @return a data.table
#' @export
get_polder_data <- function(
  name = NULL,
  case.list = NULL,
  case.desc = case.list,
  param,
  sobek.project = NULL,
  w.canal = TRUE,
  volume = TRUE,
  upstream = FALSE,
  master.tbl = NULL,
  verbose = TRUE
){
  param <- tolower(param)
  # search for IDs
  id.tbl <- get_id_tbl(
    name = name,
    case.list = case.list,
    drv = FALSE,
    case.desc = case.desc,
    master.tbl = master.tbl
  )

  if (param == 'discharge') {
    id.tbl <- id.tbl[!grepl('w', ID_TYPE) | grepl('Innen', besonderheit)]
  } else{
    id.tbl <- id.tbl[!grepl('q', ID_TYPE) | grepl('Einlass|Auslass', besonderheit)]
  }
  id.tbl[, col_name := str_match(besonderheit,
                      '.+_(Einlass[^;]*|Auslass[^;]*|Nach|Vor|Innen)')[,2]]
  # get results for each case
  id_data_list <- list()
  for (i in seq_along(case.list)) {
    id_tbl_tmp <- id.tbl[case == case.list[[i]]]
    if (param == 'discharge') {
      # this take only the first row, if there is none, it should get an NA
      if (isTRUE(upstream)) {
        id_vor <- id_tbl_tmp[grepl('_Vor', besonderheit) &
                             grepl('mID|qID', ID_TYPE)
                             ][1]
      }
      id_nach <- id_tbl_tmp[grepl('_Nach', besonderheit) &
                              grepl('mID|qID', ID_TYPE)
                            ][1]
    } else{
      if (isTRUE(upstream)) {
        id_vor <- id_tbl_tmp[grepl('_Vor', besonderheit) &
                               grepl('mID|wID', ID_TYPE)][1]
      }
      id_nach <- id_tbl_tmp[grepl('_Nach', besonderheit) &
                              grepl('mID|wID', ID_TYPE)
                            ][1]
    }
    #----get Vor/Nach data----
    # cannot combine with the Auslass, Einlass because they always take 'discharge'
    if (isTRUE(upstream)) {
      if (id_vor$ID_TYPE != id_nach$ID_TYPE) {
        id_vor_args <- list(
          case.list = case.list[[i]],
          sobek.project = sobek.project,
          id_type = id_vor$ID_F,
          param = param,
          verbose = FALSE
        )
        names(id_vor_args)[3] <- id_vor$ID_TYPE
        id_vor_data <- do.call(his_from_case, id_vor_args)
        colnames(id_vor_data) <- c('ts', 'Vor', 'case')
        id_vor_data$case <- NULL
        id_nach_args <- list(
          case.list = case.list[[i]],
          sobek.project = sobek.project,
          id_type = id_nach$ID_F,
          param = param,
          verbose = FALSE
        )
        names(id_nach_args)[3] <- id_nach$ID_TYPE
        id_vor_data <- do.call(his_from_case, id_nach_args)
        colnames(id_vor_data) <- c('ts', 'Nach', 'case')
        id_vor_data$case <- NULL
        id_vor_nach_data <-
          merge(id_vor_data, id_nach_data, by = 'ts',
                sort = FALSE)
      } else{
        id_vor_nach_args <- list(
          case.list = case.list[[i]],
          sobek.project = sobek.project,
          id_type = c(id_vor$ID_F, id_nach$ID_F),
          param = param,
          verbose = FALSE
        )
        names(id_vor_nach_args)[3] <- id_nach$ID_TYPE
        id_vor_nach_data <- do.call(his_from_case, id_vor_nach_args)
        id_vor_nach_data$case <- NULL
        colnames(id_vor_nach_data) <- c('ts', 'Vor', 'Nach')
      }
    } else{
      id_vor_nach_args <- list(
        case.list = case.list[[i]],
        sobek.project = sobek.project,
        id_type = id_nach$ID_F,
        param = param,
        verbose = FALSE
      )
      names(id_vor_nach_args)[3] <- id_nach$ID_TYPE
      id_vor_nach_data <- do.call(his_from_case, id_vor_nach_args)
      id_vor_nach_data$case <- NULL
      colnames(id_vor_nach_data) <- c('ts', 'Nach')
      # Reserve this column for the final data table, will be replace later
      id_vor_nach_data$Vor <- NA
    }
    #----get data for Einlass/Auslass----
    id_ein <- id_tbl_tmp[grepl('.+_Einlass', besonderheit) &
                           grepl('qID|sID', ID_TYPE)
                         ]
    if (nrow(id_ein) == 0) {
      id_ein <- id_tbl_tmp[grepl('.+_Einlass', besonderheit) &
                             grepl('mID', ID_TYPE)
                           ]
    }
    id_ein[, col_name := str_match(besonderheit, '.+_(Einlass[^;]*)')[, 2]]
    id_aus <- id_tbl_tmp[grepl('.+_Auslass', besonderheit) &
                           grepl('qID|sID', ID_TYPE)
                         ]
    if (nrow(id_aus) == 0) {
      id_aus <- id_tbl_tmp[grepl('.+_Auslass', besonderheit) &
                             grepl('mID', ID_TYPE)
                           ]
    }
    id_aus[, col_name := str_match(besonderheit, '.+_(Auslass[^;]*)')[, 2]]
    # check if all Einlass, Auslass besonderheit are different
    stopifnot(unique(id_ein$besonderheit) == id_ein$besonderheit)
    stopifnot(unique(id_aus$besonderheit) == id_aus$besonderheit)
    id_ein_aus_list <- c(id_ein$ID_F, id_aus$ID_F)
    id_ein_aus_type <- c(id_ein$ID_TYPE, id_aus$ID_TYPE)
    id_ein_aus_cols <- c(id_ein$col_name, id_aus$col_name)
    # identify list and names of each type of IDs (m, q, s)
    mID_list <- id_ein_aus_list[grepl('m', id_ein_aus_type)]
    mID_list_name <- id_ein_aus_cols[grepl('m', id_ein_aus_type)]
    qID_list <- id_ein_aus_list[grepl('q', id_ein_aus_type)]
    qID_list_name <- id_ein_aus_cols[grepl('q', id_ein_aus_type)]
    sID_list <- id_ein_aus_list[grepl('s', id_ein_aus_type)]
    sID_list_name <- id_ein_aus_cols[grepl('s', id_ein_aus_type)]
    id_data_tmp_sid <- NULL
    id_data_tmp_mid <- NULL
    id_data_tmp <- data.table()
    if (length(qID_list) > 0) {
      qID_list_args <- list(
        case.list = case.list[[i]],
        sobek.project = sobek.project,
        qID = qID_list,
        param = 'discharge',
        verbose = FALSE
      )
      id_data_tmp <- do.call(his_from_case, args = qID_list_args)
      # set colnames to avoid conflict of Einlass/Auslass at the same structure
      colnames(id_data_tmp) <- c('ts', qID_list_name, 'case')
    }
    if (length(sID_list) > 0) {
      sID_list_args <- list(
        case.list = case.list[[i]],
        sobek.project = sobek.project,
        sID = sID_list,
        param = 'discharge',
        verbose = FALSE
      )
      id_data_tmp_sid <- do.call(his_from_case, args = sID_list_args)
      # set colnames to avoid conflict of Einlass/Auslass at the same structure
      colnames(id_data_tmp_sid) <- c('ts', sID_list_name, 'case')
      if (nrow(id_data_tmp) > 0) {
        id_data_tmp <- merge(id_data_tmp_sid, id_data_tmp,
                             by = c('ts', 'case'),
                             sort = FALSE)
      } else{
        id_data_tmp <- id_data_tmp_sid
      }
    }
    if (length(mID_list) > 0) {
      mID_list_args <- list(
        case.list = case.list[[i]],
        sobek.project = sobek.project,
        mID = mID_list,
        param = param,
        verbose = FALSE
      )
      id_data_tmp_mid <- do.call(his_from_case, args = mID_list_args)
      # set colnames to avoid conflict of Einlass/Auslass at the same structure
      colnames(id_data_tmp_mid) <- c('ts', mID_list_name, 'case')
      if (nrow(id_data_tmp) > 0) {
        id_data_tmp <- merge(id_data_tmp_mid, id_data_tmp,
                             by = c('ts', 'case'),
                             sort = FALSE)
      } else{
        id_data_tmp <- id_data_tmp_mid
      }
    }
    setcolorder(id_data_tmp, c('ts', id_ein_aus_cols, 'case'))
    id_data_tmp <- merge(id_data_tmp, id_vor_nach_data,
                         by = 'ts', sort = FALSE)
    #---- get Waterlevel Innen----
    if (isTRUE(w.canal)) {
      id_mitte <- id_tbl_tmp[col_name == 'Innen' &
                               grepl('mID|wID', ID_TYPE)][1]
      id_mitte_args <- list(
        case.list = case.list[[i]],
        sobek.project = sobek.project,
        id_mitte_type = id_mitte$ID_F,
        param = 'waterlevel',
        verbose = FALSE
      )
      names(id_mitte_args)[3] <- id_mitte$ID_TYPE
      wt_id_mitte <- do.call(his_from_case, id_mitte_args)
      colnames(wt_id_mitte) <- c('ts', 'W_innen', 'case')
      wt_id_mitte$case <- NULL
      # merging data
      id_data_tmp <- merge(id_data_tmp, wt_id_mitte, by = 'ts', sort = FALSE)
    }
    id_data_list[[i]] <- id_data_tmp
  }
  id_data <- rbindlist(id_data_list)
  if (isTRUE(volume)) {
    id_vol <- get_polder_volume2(
      name = name,
      case.list = case.list,
      case.desc = case.desc,
      sobek.project = sobek.project,
      master.tbl = master.tbl
    )
    id_data <- merge(id_data, id_vol, by = c('case', 'ts'))
  }
  rm(id_data_list, id_data_tmp)
  return(id_data)
}


#' Get total volume for one measure
#'
#' @param name Name of the measure
#' @param case.list List of the cases
#' @param case.desc Case name standardized
#' @param param Parameter discharge/waterlevel
#' @param sobek.project Path to sobek project
#' @param master.tbl Master table
#' @param get.max Return only max value (by case)
#' @return a data.table
#' @export
get_polder_volume <- function(
  name,
  case.list,
  case.desc = case.list,
  sobek.project,
  sub.v0 = FALSE,
  do.par = TRUE,
  master.tbl,
  get.max = TRUE
){
  case.list <- unlist(case.list)
  n_case <- length(case.list)
  id_tbl <- get_id_tbl(name = name, case.list = case.list,
                       case.desc = case.desc, master.tbl = master.tbl
  )
  id_vol <- id_tbl[grepl('.*_Vol', besonderheit) & ID_TYPE == 'wID']
  case_tbl <- data.table(case = case.list, case_desc = case.desc)
  case_tbl[, his_file := sapply(case, get_file_path,
                                sobek.project = sobek.project, type ="calcpnt.his")]
  if (do.par) {
    doParallel::registerDoParallel(parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    vol_tbl <- foreach::foreach(i = 1:n_case, .combine = rbind) %dopar% {
      this_case_name <- case_tbl$case[[i]]
      this_case_desc <- case_tbl$case_desc[[i]]
      this_case_hfile <- case_tbl$his_file[[i]]
      this_case_ids <- id_vol$ID_F[id_vol$case == this_case_name]
      his_data <- sobekio::his_from_list(
        his.file = this_case_hfile, id.list = this_case_ids, param = "Volume"
      )
      his_data$case <- this_case_desc
      his_data
    }
    doParallel::stopImplicitCluster()
  } else {
    vol_tbl <- list()
    for (i in seq_along(case.list)) {
      this_case_name <- case_tbl$case[[i]]
      this_case_desc <- case_tbl$case_desc[[i]]
      this_case_hfile <- case_tbl$his_file[[i]]
      this_case_ids <- id_vol[case == this_case_name, ID_F]
      vol_tbl[[i]] <- sobekio::his_from_list(
        his.file = this_case_hfile, id.list = this_case_ids, param = "Volume"
      )
      vol_tbl[[i]]$case <- this_case_desc
    }
    vol_tbl <- rbindlist(vol_tbl, use.names = TRUE)
  }
  vol_tbl <- vol_tbl[, rowSums(.SD, na.rm = TRUE), by = case,
                     .SDcols = -c('ts')]
  if (sub.v0) {
    vol_tbl[, V1 := V1 - V1[[1]]]
  }
  if (get.max) {
    vol_tbl <- vol_tbl[, round(max(V1)/10^6, 2), by = case]
    colnames(vol_tbl) <- c('case', 'Volume_max')
  }
  return(vol_tbl)
}


get_polder_volume2 <- function(
  name = NULL,
  case.list = NULL,
  case.desc = case.list,
  sobek.project = NULL,
  master.tbl
){
  # get results for each case
  id_tbl <- get_id_tbl(name = name, case.list = case.list,
                       case.desc = case.desc, master.tbl = master.tbl
  )
  id_vol <- id_tbl[grepl('.*_Vol', besonderheit) & ID_TYPE == 'wID']
  id_data_list <- list()
  for (i in seq_along(case.list)) {
    id_tbl_tmp <- id_vol[case == case.list[i]]
    if (nrow(id_tbl_tmp) > 0) {
      id_vol_args <- list(case.list = case.list[[i]],
                          sobek.project = sobek.project,
                          wID = id_vol$ID_F,
                          param = 'Volume',
                          verbose = FALSE)
      id_data_tmp <- do.call(his_from_case, id_vol_args)
      id_data_tmp <- id_data_tmp[, Volume := rowSums(.SD, na.rm = TRUE),
                                 .SDcols = -c('ts', 'case')]
    } else{
      id_data_tmp <- data.table(case = case.list[[i]], Volume = NA, ts = NA)
    }
    id_data_list[[i]] <- id_data_tmp[, c('ts', 'Volume', 'case')]
  }
  id_data_vol <- rbindlist(id_data_list)
  return(id_data_vol)
}


#' Get data table for a DRV
#' @param name Name of the measure
#' @param case.list List of the cases
#' @param case.desc Case name standardized
#' @param param Parameter discharge/waterlevel
#' @param sobek.project Path to sobek project
#' @param to.upstream Distance to upstream (km) to get data
#' @param to.downstream Distance to downstream (km) to get data
#' @param get.max Should max value or whole time series return? Default only max value
#' @param master.tbl Master table
#' @param verbose Should some message be displayed?
#' @param do.par If TRUE, parallel computing will be executed
#' @param ts.trim.left Default NULL. Number of days from the begining of simulation time to remove from timeseries (useful to remove "cold start period")
#' @return a data.table
#' @export
get_drv_data <- function(
  name = NULL,
  case.list = NULL,
  case.desc = case.list,
  param = NULL,
  sobek.project = NULL,
  to.upstream = 0,
  to.downstream = 0,
  get.max = TRUE,
  master.tbl = NULL,
  verbose = TRUE,
  do.par = FALSE,
  ts.trim.left = NULL
){
  param <- tolower(param)
  stopifnot(is.numeric(to.upstream) & is.numeric(to.downstream))
  id_tbl <- get_id_tbl(
    name = name,
    case.list = case.list,
    drv = TRUE,
    to.upstream = to.upstream,
    to.downstream = to.downstream,
    case.desc = case.desc,
    master.tbl = master.tbl
  )
  if (nrow(id_tbl) == 0) stop('No ID found! Check master.tbl, name of the DRV or to.upstream, to.downstream')
  if (param != 'discharge') {
    id_tbl <- id_tbl[ID_TYPE == 'wID']
    for (i in seq_along(case.list)) {
      # get list of qIDs for this case
      wid = id_tbl[case == case.list[[i]], list(km, ID_F)]
      # finding the IDs that have duplicated km
      km_dup = wid[duplicated(km), km]
      if (length(km_dup) > 0) {
        warning('There are more than one wIDs for the same KM at KM(s): ',
                paste(km_dup, collapse = ', '),
                '. These KM(s) will be remove out of the graphic')
        id_tbl <- id_tbl[!km %in% km_dup]
      }
    }
  } else {
    id_tbl <- id_tbl[ID_TYPE == 'qID']
  }
  if (isTRUE(do.par)) {
    if (isTRUE(verbose)) {
      cat(paste('Getting data for',
                  round(nrow(id_tbl)/length(case.list)),
                  'ID(s) in', length(case.list), 'case(s)\n'))
      cat('Your computer will be overloaded for a short time. Please be patient...\n')
    }
    doParallel::registerDoParallel(parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    drv_data_list <-
      foreach::foreach(i = 1:length(case.list)) %dopar% {
        if (param == 'discharge') {
          tmp <- sobekio::his_from_case(
            case.list = case.list[[i]],
            sobek.project = sobek.project,
            param = param,
            qID = id_tbl[case == case.list[[i]] &
                           ID_TYPE == 'qID', ID_F],
            verbose = FALSE
          )
        } else{
          tmp <- sobekio::his_from_case(
            case.list = case.list[[i]],
            sobek.project = sobek.project,
            param = param,
            wID = id_tbl[case == case.list[[i]] &
                           ID_TYPE == 'wID', ID_F],
            verbose = FALSE
          )
        }
        tmp
      }
    doParallel::stopImplicitCluster()
  } else {
    drv_data_list <- list()
    cat(paste('Getting data for',
                round(nrow(id_tbl)/length(case.list)),
                'ID(s) in', length(case.list), 'case(s)\n'))
    if (param == 'discharge') {
      for (i in seq_along(case.list)) {
        drv_data_list[[i]] <- his_from_case(
          case.list = case.list[[i]],
          sobek.project = sobek.project,
          param = param,
          qID = id_tbl[case == case.list[[i]] &
                         ID_TYPE == 'qID', ID_F],
          verbose = FALSE
        )
      }
    } else{
      for (i in seq_along(case.list)) {
        drv_data_list[[i]] <- his_from_case(
          case.list = case.list[[i]],
          sobek.project = sobek.project,
          param = param,
          wID = id_tbl[case == case.list[[i]] &
                         ID_TYPE == 'wID', ID_F],
          verbose = FALSE
        )
      }
    }
  }
  drv_data <- rbindlist(drv_data_list, use.names = TRUE, fill = TRUE)
  # to sum discharge at reaches that have same chainages along branches together
  q_dup_new_col <- vector(mode = 'character')
  q_dup_id <- vector(mode = 'character')
  if (param == 'discharge') {
    for (i in seq_along(case.list)) {
      # get list of qIDs for this case
      qid = id_tbl[case == case.list[[i]], list(km, ID_F)]
      # finding the IDs that have duplicated km
      km_dup = qid[duplicated(km), km]
      for (k in km_dup) {
        qid_k <- qid[km == k, ID_F]
        drv_data[case == case.list[[i]],
                     eval(qid_k[1]) := rowSums(.SD, na.rm = TRUE),
                     .SDcols = qid_k
                     ]
        drv_data[case == case.list[[i]],
                     c(qid_k[-1]) := as.list(rep(NA, length(qid_k[-1])))
                     ]
      }
    }
  }
  if (!is.null(ts.trim.left)) {
    drv_data[, ts_left := min(ts, na.rm = TRUE) +
                   ts.trim.left * 3600 * 24, by = case]
    drv_data <- drv_data[ts >= ts_left]
  }
  if (isTRUE(get.max)) {
    drv_data <- drv_data[, lapply(.SD, max, na.rm = TRUE),
                             .SDcols = -c('ts'), by = case] %>%
      melt(id.vars = 'case', variable.name = 'ID_F', value.name = 'scheitel')
    # removing ID_F that were aggregated to the first ID_F at the duplicated KM
    if (param == 'discharge') {
      for (i in seq_along(case.list)) {
        # get list of qIDs for this case
        qid = id_tbl[case == case.list[[i]], list(km, ID_F)]
        # finding the IDs that have duplicated km
        km_dup = qid[duplicated(km), km]
        for (k in km_dup) {
          qid_k <- qid[km == k, ID_F]
          drv_data <- drv_data[!(case == case.list[[i]] & ID_F %in% qid_k[-1])]
        }
      }
    }
    drv_data[is.infinite(scheitel), scheitel := NA]
  }
  drv_data <- merge(drv_data,
                    id_tbl[, .SD, .SDcols = -c('ID')],
                    by = c('case', 'ID_F'), sort = FALSE)
  return(drv_data)
}


#' Calculate maximum values for a measure and its referenced location
#' @param name Name of the measure (with/without the measure)
#' @param case.list List of cases
#' @param case.desc Correct (according to case naming standard in NHWSP) version of case.list, it will be used for legend
#' @param sobek.project Path to sobek project
#' @param master.tbl Table of ID Coding of the sobek network
#' @param param 'Waterlevel' or 'Discharge' Hydrograph
#' @param w.canal Logical. Should max water level inside the measure be included?
#' @param upstream Logical. Should value for "before" polder be included?
#' @param volume Logical. Should max volume be included?
#' @param ref.mID ID of Bezugspegel
#' @param compare.by How should the delta value be calculated (compare by location, or by ex. vgf)
#' @param group.by How the case should be grouped for comparing. Default is grouping by (1 1, 2 2, 3 3...)
#' @return a data.table
#' @export
get_polder_max <- function(
  name,
  case.list,
  case.desc = case.list,
  param,
  sobek.project,
  upstream = TRUE,
  w.canal = TRUE,
  volume = TRUE,
  ref.mID = NULL,
  delta = TRUE,
  cmp.sort = FALSE,
  compare.by = 'zustand',
  group.by = compare.by,
  master.tbl,
  verbose = TRUE
){
  param <- tolower(param)
  delta <- isTRUE(delta)
  case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
  if (delta) {
    stopifnot(length(group.by) <= 2)
    stopifnot(
      compare.by %in% c('zustand', 'vgf', 'hwe', 'notiz', 'zielpegel') &
        all(group.by %in% c('zustand', 'vgf', 'hwe', 'notiz', 'zielpegel'))
    )
    cmp_vars <- unique(case_tbl[, get(compare.by)])
    if (isTRUE(cmp.sort)) cmp_vars <- sort(cmp_vars)
    if (length(cmp_vars) != 2) {
      stop('compare.by must have two values not: ',
           str_flatten(cmp_vars, collapse = ', ' ))
    }
    if (length(group.by) == 1) {
      case_tbl[, group__by := get(group.by)]
    } else {
      case_tbl[, group__by := paste(get(group.by[1]), get(group.by[2]), sep = '_')]
    }
    grp_vars <- unique(case_tbl[, group__by])
    total_case <- unique(as.vector(outer(cmp_vars, grp_vars, paste, sep = '_')))
    if (length(group.by) > length(compare.by) | compare.by != group.by[1]) {
      if (length(total_case) != length(case.list)) {
        stop('Combination of compare.by and group.by is not distinct
             for calculating delta')
      }
    } else {
      if (!near(length(total_case) / 2, length(case.list), 0.1)) {
        stop('Combination of compare.by and group.by is not distinct
             for calculating delta')
      }
    }
  }
  if (isTRUE(verbose)) cat('Reading data at the measure...\n')
  id_data <- get_polder_data(
    name = name,
    case.list = case.list,
    case.desc = case.desc,
    param = param,
    w.canal = w.canal,
    upstream = upstream,
    sobek.project = sobek.project,
    master.tbl = master.tbl
  )

  # reading data for ref.mID
  if (!is.null(ref.mID)) {
    if (isTRUE(verbose)) cat('Reading data for ref.mID...\n')
    if (length(ref.mID) > 1) {
      ref.mID_id <- ref.mID[[1]]
      if (hasName(ref.mID, 'ID'))
        ref.mID_id <- ref.mID$ID
      ref.mID_name <- ref.mID[[2]]
      if (hasName(ref.mID, 'name'))
        ref.mID_name <- ref.mID$name
      # ref.mID_color <- ref.mID_name
      ref.mID_type <- ifelse(param == 'discharge', 'qID', 'wID')
      if (hasName(ref.mID, 'type'))
        ref.mID_type <- ref.mID$type
      ref_mID_args <- list(
        case.list = case.list,
        sobek.project = sobek.project,
        id_type = ref.mID_id,
        param = param,
        verbose = FALSE
      )
      names(ref_mID_args)[3] <- ref.mID_type
      ref_mID <- do.call(his_from_case, ref_mID_args)
    } else{
      ref_mID <- his_from_case(
        case.list = case.list,
        sobek.project = sobek.project,
        mID = ref.mID[[1]],
        param = param,
        verbose = FALSE
      )
    }
    colnames(ref_mID) <- c('ts', 'Bezugspegel', 'case')
    id_data <- merge(id_data, ref_mID, by = c('ts', 'case'))
  }
  id_data_max <- id_data[, lapply(.SD, max, na.rm = TRUE),
                         .SDcols = -c('ts'),
                         by = case
                         ]
  if (isTRUE(volume)) {
    if (isTRUE(verbose)) cat('Reading volume...\n')
    id_vol <- get_polder_volume(
      name = name,
      case.list = case.list,
      case.desc = case.desc,
      sobek.project = sobek.project,
      master.tbl = master.tbl
    )
    id_data_max <- merge(id_data_max, id_vol, by = 'case', sort = FALSE)
  }
  if (delta) {
    col_get_delta <- colnames(id_data_max)[-1]
    id_data_max <- merge(id_data_max, case_tbl, by = 'case', sort = FALSE)
    id_data_max[, group := seq_len(.N), by = eval(compare.by)]
    if (all(compare.by != group.by)) {
      for (i in col_get_delta) {
        data_tbl_delta <-
          dcast(id_data_max,
                group  ~ get(compare.by) + group__by  ,
                value.var = i)
        for (j in grp_vars) {
          col_j <- paste('Delta', i, j, sep = '_')
          col_1 <- paste(cmp_vars[1], j, sep = '_')
          col_2 <- paste(cmp_vars[2], j, sep = '_')
          data_tbl_delta[, eval(col_j) := get(col_1) - get(col_2)]
          data_tbl_delta[, eval(col_1) := NULL]
          data_tbl_delta[, eval(col_2) := NULL]
        }
        data_tbl_delta <- melt(
          data_tbl_delta,
          id.vars = 'group',
          variable.name = 'group__by',
          value.name = paste('Delta', i, sep = '_'),
          sort = FALSE
        )
        data_tbl_delta[, group__by :=
                         str_replace(group__by,
                                     paste('Delta_', i, '_', sep = ''), '')
                       ]
        id_data_max <- merge(id_data_max, data_tbl_delta,
                             by = c('group__by', 'group'), sort = FALSE)
      }
    } else{
      for (i in col_get_delta) {
        data_tbl_delta <-
          dcast(id_data_max, group  ~ get(compare.by),
                value.var = i)
        col_1 <- cmp_vars[1]
        col_2 <- cmp_vars[2]
        delta_col_name <- paste('Delta', i, sep = '_')
        data_tbl_delta[, eval(delta_col_name) := get(col_1) - get(col_2)]
        data_tbl_delta[, eval(col_1) := NULL]
        data_tbl_delta[, eval(col_2) := NULL]
        id_data_max <-
          merge(id_data_max, data_tbl_delta, by = 'group', sort = FALSE)
      }
    }
    col_keep <- c('case',
                  col_get_delta,
                  paste('Delta', col_get_delta, sep = '_'),
                  group.by)
    if (all(group.by != compare.by)) col_keep <- c(col_keep, compare.by)
    id_data_max <- id_data_max[, .SD, .SDcols = col_keep]
  }

  return(id_data_max)
}


#' Get information table for a polder to compare the outputs of with/without it
#'
#' @param name Name of polder
#' @param case.mit List of cases with (the effect of) the polder
#' @param case.ohne List of cases without (the effect of) the polder
#' @param hwe.names Names of the floods
#' @param sobek.project Path to sobek project
#' @param master.tbl Master table
#' @param html Logical. If TRUE, a html table will be generated
#' @param verbose Display some message if TRUE
#' @export
get_polder_tbl <- function(
  name,
  case.mit,
  case.ohne,
  ref.mID = NULL,
  hwe.names,
  sobek.project,
  master.tbl,
  html = TRUE,
  verbose = TRUE
) {
  stopifnot(length(name) == 1)
  all_cases <- c(case.mit, case.ohne)
  stopifnot(length(all_cases) == length(unique(all_cases)))
  pegel <- parse_ref_id(ref.mID)
  case_tbl <- get_polder_case_tbl(case.mit, case.ohne, sobek.project)
  id_tbl <- get_polder_id_tbl(name, case.mit, case.ohne,
                              master.tbl, sobek.project,
                              case_tbl, pegel)
  id_tbl_vol <- id_tbl[grepl(paste0(name, '_Vol'), besonderheit)]
  id_tbl <- id_tbl[!grepl(paste0(name, '_Vol'), besonderheit)]
  # reading volume
  vol_max_list <- vector()
  all_cases <- unique(id_tbl_vol$case)
  for (i in seq_along(all_cases)) {
    this_case <- all_cases[[i]]
    this_his <- unique(id_tbl_vol[case == this_case, his_file])
    if (verbose) cat('Get volume for case: ', this_case, '\n')
    this_vol_tbl <- his_from_list(his.file = this_his,
                                  id.list = id_tbl_vol[his_file == this_his, ID_F],
                                  param = 'Volume')
    this_vol_tbl[, Volume := rowSums(.SD), .SDcols = -c('ts')]
    vol_max_list[[i]] <- round(max(this_vol_tbl$Volume, na.rm = TRUE) / 10^6, 2)
  }
  vol_max_tbl <- data.table(Volume = vol_max_list, case = all_cases)
  # reading discharge on the river at outlet location and at the measurement
  id_tbl_qt <- id_tbl[grepl(paste0(name, '_Nach|Pegel'), besonderheit) & ID_TYPE != 'wID']
  qt_max_list <- list()
  for (i in seq_along(all_cases)) {
    this_case <- all_cases[i]
    if (verbose) cat('Get discharge for case: ', this_case, '\n')
    this_his <- unique(id_tbl_qt[case == this_case, his_file])
    id_nach <- id_tbl_qt[case == this_case & grepl('_Nach', besonderheit), ID_F]
    # this_his should have length of 1 or 2
    # length 2 means there is a pegel and pegel_type and id_nach_type are different
    if (length(this_his) == 2) {
      qt_pegel <- his_from_list(
        his.file = id_tbl_qt[case == this_case & ID_F == pegel$ID, his_file],
        id.list = pegel$ID,
        param = 'discharge'
        )
      qt_nach <- his_from_list(
        his.file = id_tbl_qt[case == this_case & ID_F == id_nach, his_file],
        id.list = id_nach,
        param = 'discharge')
      this_qt_tbl <- merge(qt_pegel, qt_nach, by = 'ts')
    } else {
      if (!is.null(pegel$ID)) {
        this_qt_tbl <- his_from_list(
          his.file = this_his,
          id.list = c(id_nach, pegel$ID),
          param = 'discharge'
          )
        colnames(this_qt_tbl) <- c('ts',  'Q', 'Q_Pegel')
      } else {
        this_qt_tbl <- his_from_list(
          his.file = this_his,
          id.list = id_nach,
          param = 'discharge'
        )
        colnames(this_qt_tbl) <- c('ts',  'Q')
      }
    }
    qt <- this_qt_tbl[, lapply(.SD, max, na.rm = TRUE),
                      .SDcols = -c('ts')]
    qt$case <- this_case
    qt_max_list[[i]] <- qt
  }
  qt_max_tbl <- rbindlist(qt_max_list)
  # reading waterlevel on the river at outlet location
  id_tbl_wt <-
    id_tbl[grepl(paste0(name, '_Innen|', name, '_Nach'), besonderheit) &
             ID_TYPE != 'qID']
  wt_max_list <- list()
  for (i in seq_along(all_cases)) {
    this_case <- all_cases[i]
    if (verbose) cat('Get waterlevel for case: ', this_case, '\n')
    this_his <- unique(id_tbl_wt[case == this_case, his_file])
    this_id_tbl_wt <- id_tbl_wt[case == this_case]
    id_nach <- this_id_tbl_wt[grepl(paste0(name, '_Nach'), besonderheit), ID_F]
    id_innen <- this_id_tbl_wt[grepl(paste0(name, '_Innen'), besonderheit), ID_F]
    if (length(this_his) > 1) {
      his_nach <- this_id_tbl_wt[grepl(paste0(name, '_Nach'), besonderheit), his_file]
      his_innen <- this_id_tbl_wt[grepl(paste0(name, '_Innen'), besonderheit), his_file]
      his_pegel <- this_id_tbl_wt[besonderheit == 'Pegel', his_file]
      wt_nach <- his_from_list(
        his.file = his_nach,
        id.list = id_nach,
        param = 'waterlevel'
      )[, max(get(id_nach), na.rm = TRUE)]
      wt_innen <- his_from_list(
        his.file = his_innen,
        id.list = id_innen,
        param = 'waterlevel'
      )[, max(get(id_innen), na.rm = TRUE)]
      if (!is.null(pegel$ID)) {
        wt_pegel <- his_from_list(
          his.file = his_pegel,
          id.list = pegel$ID,
          param = 'waterlevel'
        )[, max(get(id_innen), na.rm = TRUE)]
        wt <- data.table(W = wt_nach, W_Polder = wt_innen, W_Pegel = wt_pegel)
      } else {
        wt <- data.table(W = wt_nach, W_Polder = wt_innen)
      }
    } else {
      if (!is.null(pegel$ID)) {
        wt <- his_from_list(
          his.file = this_his,
          id.list = c(id_nach, id_innen, pegel$ID),
          param = 'waterlevel'
        )
        colnames(wt) <- c('ts', 'W', 'W_Polder', 'W_Pegel')
      } else {
        wt <- his_from_list(
          his.file = this_his,
          id.list = c(id_nach, id_innen),
          param = 'waterlevel'
        )
        colnames(wt) <- c('ts', 'W', 'W_Polder')
      }
    }
    wt <- wt[, lapply(.SD, max), .SDcols = -c('ts')]
    wt$case <- this_case
    wt_max_list[[i]] <- wt
  }
  wt_max_tbl <- rbindlist(wt_max_list)
  polder_tbl <- merge(wt_max_tbl, qt_max_tbl, by = 'case')
  polder_tbl <- merge(polder_tbl, vol_max_tbl, by = 'case')
  polder_tbl[case %in% case.mit, zustand := 'Mit']
  polder_tbl[case %in% case.ohne, zustand := 'Ohne']
  polder_tbl <- merge(polder_tbl, case_tbl[, c('case', 'grp')], by = 'case')
  value_vars <- c('W', 'W_Polder', 'Q', 'Volume', 'W_Pegel', 'Q_Pegel')
  final_cols <- c(
    'Q_Mit', 'Q_Ohne', 'Delta_Q',
    'W_Mit', 'W_Ohne', 'Delta_W',
    'Volume_Mit',
    'W_Polder_Mit',
    'Q_Pegel_Mit', 'Q_Pegel_Ohne', 'Delta_Q_Pegel',
    'W_Pegel_Mit', 'W_Pegel_Ohne', 'Delta_W_Pegel'
  )
  if (is.null(pegel$ID)) {
    value_vars <- value_vars[!grepl('_Pegel', value_vars)]
    final_cols <- final_cols[!grepl('_Pegel', final_cols)]
  }
  polder_tbl <- dcast(polder_tbl, grp ~ zustand, value.var = value_vars)
  polder_tbl[, Delta_Q := Q_Mit - Q_Ohne]
  polder_tbl[, Delta_W := W_Mit - W_Ohne]
  if (!is.null(pegel$ID)) {
    polder_tbl[, Delta_Q_Pegel := Q_Pegel_Mit - Q_Pegel_Ohne]
    polder_tbl[, Delta_W_Pegel := W_Pegel_Mit - W_Pegel_Ohne]
  }
  polder_tbl[, (final_cols) := lapply(.SD, round, digits = 2), .SDcols = final_cols]
  if (length(hwe.names) == max(polder_tbl$grp)) {
    polder_tbl$HWE <- hwe.names
  } else {
    cat('Cannot assign hwe names, set hwe to group index\n')
    polder_tbl[, HWE := grp]
  }
  final_cols <- c('HWE', final_cols)
  polder_tbl <- polder_tbl[, ..final_cols]
  if (html) {
    headers <-  c(' ' = 1,
                  'Abfluss nach dem Polder' = 3,
                  'Wasserstand nach dem Polder' = 3,
                  'Im Polder' = 2,
                  'Abfluss am Pegel' = 3,
                  'Wasserstand am Pegel' = 3
    )
    names(headers)[5] <- trimws(paste('Abfluss am Pegel', pegel$name))
    names(headers)[6] <- trimws(paste('Wasserstand am Pegel', pegel$name))
    if (is.null(pegel$ID)) {
      headers <- headers[-c(5,6)]
    }
    polder_tbl <- kable(polder_tbl) %>% kable_styling(c('hover', 'striped')) %>%
      add_header_above(headers)
  }
  return(polder_tbl)
}

parse_ref_id <- function(ref.mID) {
  if (length(ref.mID) > 1) {
    if (hasName(ref.mID, 'ID')) {
      ref_id <- ref.mID[['ID']]
    } else {
      ref_id <- ref.mID[[1]]
    }
    if (hasName(ref.mID, 'name')) {
      ref_name <- ref.mID[['name']]
    } else {
      ref_name <- ref.mID[[2]]
    }
    if (hasName(ref.mID, 'type')) {
      ref_type <- ref.mID[['type']]
    } else {
      if (length(ref.mID) > 2) {
        ref_type <- ref.mID[[3]]
      } else {
        ref_type <- 'mID'
      }
    }
  if (is.null(ref_name)) ref_name <- ref_id
  } else {
    ref_id <- ref_name <- ref.mID[[1]]
    ref_type <- 'mID'
  }
  return(list(ID = ref_id, type = ref_type, name = ref_name))
}


get_polder_case_tbl <- function(
  case.mit, case.ohne, sobek.project
) {
  # parsing information from cases
  case_tbl_mit <- data.table(case = case.mit)
  case_tbl_ohne <- data.table(case = case.ohne)
  case_tbl_mit[, grp := .GRP, by = case]
  case_tbl_ohne[, grp := .GRP, by = case]
  case_tbl <- rbind(case_tbl_mit, case_tbl_ohne)
  # reading caselist.cmt
  case_cmt <- fread(file_path('caselist.cmt', sobek.project), sep = ' ',
                    quote = "'", col.names = c('case_number', 'case'))
  case_cmt[, case := str_remove_all(case, '"')]
  case_cmt <- case_cmt[case %in% c(case.mit, case.ohne)]
  case_chk <- assertthat::are_equal(sort(case_cmt$case),
                                    sort(c(case.mit, case.ohne)))
  if (!case_chk) {
    stop('Not all cases were found in the caselist.cmt, check case names or sobek.project')
  }
  case_cmt[, case_folder := file.path(sobek.project, case_number)]
  case_tbl <- merge(case_tbl, case_cmt, by = 'case')
  return(case_tbl)
}

get_polder_id_tbl <- function(
  name, case.mit, case.ohne, master.tbl, sobek.project, case_tbl, pegel
) {
  id_tbl <- get_id_tbl(name = name,
                       case.list = c(case.mit, case.ohne),
                       master.tbl = master.tbl
  )
  id_tbl <- id_tbl[ID_TYPE != 'sID'][, c('km', 'river', 'ID') := rep(NULL, 3)]
  refid_tbl <- case_tbl[, c('case')]
  if (!is.null(pegel$ID)) {
    refid_tbl[, ID_F := pegel$ID][, ID_TYPE := pegel$type][, besonderheit := 'Pegel']
    id_tbl <- rbind(id_tbl, refid_tbl)
  }
  id_tbl <- id_tbl[grepl(paste0(name, '_Vol|',  name, '_Innen|', name, '_Nach|Pegel'),
                         besonderheit)]
  id_tbl$his_file <- sapply(tolower(id_tbl$ID_TYPE), function(x) switch(
    tolower(x),
    wid = 'calcpnt.his',
    mid = 'measstat.his',
    qid = 'reachseg.his',
    stop('master.tbl has wrong format!')
  ),
  USE.NAMES = FALSE
  )
  id_tbl <- merge(id_tbl, case_tbl[, c('case', 'case_folder', 'grp')], by = 'case')
  id_tbl[, his_file := file.path(case_folder, his_file)]
  id_tbl[, his_grp := .GRP, by = his_file]
  return(id_tbl)
}
