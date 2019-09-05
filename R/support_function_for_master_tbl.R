#' Parse case name to get standard information
#' @param case.desc Case description (standard naming)
#' @param orig.name Case original name
#' @return a data.table
#' @export
parse_case <- function(case.desc, orig.name = case.desc, stringAsFactors = FALSE){
  stopifnot(length(case.desc) == length(orig.name))
  case_str <- str_match(case.desc, "^([^_]+)_([^_]+)_([^_]+)_([^_]+)(.*)$")
  if (ncol(case_str) < 5 | ncol(case_str) > 6) {
    stop('case name: ', case, ' has wrong format')
  }
  result <- data.table(
    case = orig.name,
    case_desc = case_str[, 1],
    zustand = case_str[, 2],
    zielpegel = case_str[, 3],
    hwe = case_str[, 4],
    vgf = case_str[, 5],
    notiz = case_str[, 6]
  )
  if (isTRUE(stringAsFactors)){
    id_levels <- seq_along(result$zustand)
    f_levels <- purrr::map2(result$zustand, id_levels, paste, sep = "_")
    result$zustand  <- factor(result$zustand,
                              levels = f_levels,
                              labels = result$zustand,
                              ordered = TRUE)
  }
  return(result)
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
  master.tbl = NULL
){
  case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
  id_tbl <- master.tbl[grepl(name, besonderheit)]
  
  if (drv == 'auto') {
    str_mt <- str_match(id_tbl$besonderheit,
                            paste('(^.+)_', name, sep = "")
                            )
    measure_type <- toupper(unique(str_mt[,2]))
      # warning('Type of the measure is not unique: ', measure_type)
    drv <- ifelse('DRV' %in% measure_type, TRUE, FALSE)
  }
  if (isTRUE(drv)) {
    drv_begin <- id_tbl[grepl(".+_Begin", besonderheit), km][[1]]
    drv_end <- id_tbl[grepl(".+_End", besonderheit), km][[1]]
    if (drv_begin < drv_end){
      drv_begin <- drv_begin - to.upstream
      drv_end <- drv_end + to.downstream
    } else{
      drv_begin <- drv_begin + to.upstream
      drv_end <- drv_end - to.downstream
    }
    km_range <- sort(c(drv_begin, drv_end))
    drv_river <- id_tbl[grepl(".+_Begin", besonderheit), river][[1]]
    id_tbl <- master.tbl[river == drv_river &
                           km >= km_range[1] &
                           km <= km_range[2]]
  }
  stopifnot(nrow(id_tbl) > 1)
  # creating new columns based on case name
  for (i in seq_along(case.desc)) {
    col_n <- paste('case_desc', i, sep = "_")
    id_tbl[, eval(col_n) := case.desc[[i]]]
  }
  measure_vars <- paste('case_desc', seq_along(case.desc), sep = "_")
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
      id_tbl[nchar(get(zustand_in_cases[i])) > 0,
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
#' @return a data.table
#' @export
get_segment_id_tbl <- function(
  river = NULL,
  from.km = -Inf,
  to.km = Inf,
  case.list = NULL,
  case.desc = case.list,
  master.tbl = NULL
){
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
  for (i in seq_along(case.desc)){
    col_n <- paste('case_desc', i, sep = "_")
    id_tbl[, eval(col_n) := case.desc[[i]]]
  }
  measure_vars <- paste('case_desc', seq_along(case.desc), sep = "_")
  id_tbl <- melt(id_tbl, measure.vars = measure_vars,
                 value.name = 'case_desc', sort = FALSE)
  id_tbl$variable <- NULL
  id_tbl <- merge(id_tbl, case_tbl[, c('case', 'case_desc', 'zustand')],
                  by = 'case_desc', sort = FALSE)
  # check if the zustand in cases are in listed in the master.tbl
  zustand_in_cases <- unique(as.character(case_tbl$zustand))
  id_tbl[, ID_F := ID]
  id_tbl_cols <- colnames(id_tbl)
  for (i in seq_along(zustand_in_cases)){
    if (zustand_in_cases[i] %in% id_tbl_cols){
      # get final, correct ID for each case
      id_tbl[zustand == zustand_in_cases[i],
             ID_F := get(zustand_in_cases[i])]
    }

  }
  id_tbl$zustand <- NULL
  id_tbl$case_desc <- NULL
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
#' @return a data.table
#' @export
get_segment_data <- function(
  river = NULL,
  from.km = NULL,
  to.km = NULL,
  case.list = NULL,
  case.desc = case.list,
  param = NULL,
  sobek.project = NULL,
  get.max = TRUE,
  master.tbl = NULL,
  verbose = TRUE,
  do.par = FALSE
){
  id_tbl <- get_segment_id_tbl(
    river = river,
    from.km = from.km,
    to.km = to.km,
    case.list = case.list,
    case.desc = case.desc,
    master.tbl = master.tbl
  )
  # parallel reading data from cases
  if (isTRUE(do.par)) {
    if (isTRUE(verbose)) {
      print(paste('Getting data for',
                  round(nrow(id_tbl)/length(case.list)),
                  'ID(s) in', length(case.list), 'case(s)'))
      print('Your computer will be overloaded for a short time. Please be patient...')
    }
    # require("doParallel", quietly = TRUE)
    # require("foreach", quietly = TRUE)
    doParallel::registerDoParallel(parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    segment_data <- 
      foreach::foreach(i = 1:length(case.list), .combine = rbind) %dopar% {
      if (param == 'discharge') {
          tmp <- his_from_case(
            case.list = case.list[[i]],
            sobek.project = sobek.project,
            param = param,
            qID = id_tbl[case == case.list[[i]] &
                           ID_TYPE == 'qID', ID_F],
            verbose = FALSE
          )
      } else{
          tmp <- his_from_case(
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
  } else{
    segment_data_list <- list(rep(NA, length(case.list)))
    if (isTRUE(verbose)) {
      print(paste('Getting data for',
                  round(nrow(id_tbl)/length(case.list)),
                  'ID(s) in', length(case.list), 'case(s)'))
      print('Try it with do.par = TRUE if you have more than one case and so many IDs')
      print('Please be patient...')
    }
    if (param == 'discharge') {
      for (i in seq_along(case.list)) {
        segment_data_list[[i]] <- his_from_case(
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
        segment_data_list[[i]] <- his_from_case(
          case.list = case.list[[i]],
          sobek.project = sobek.project,
          param = param,
          wID = id_tbl[case == case.list[[i]] &
                         ID_TYPE == 'wID', ID_F],
          verbose = FALSE
        )
      }
    }
    segment_data <- rbindlist(segment_data_list, use.names = FALSE)
  }
  # processing river segment that are divided in two or more branches
  # to sum discharge at reaches that have same chainages along branches together
  q_dup_new_col <- vector(mode = 'character')
  q_dup_id <- vector(mode = 'character')
  if (param == 'discharge') {
    for (i in seq_along(case.list)) {
      # get list of qIDs for this case
      qid = id_tbl[case == case.list[[i]] &
                     ID_TYPE == 'qID', list(km, ID_F)]
      # finding the IDs that have duplicated km
      qid_dup = qid[duplicated(km), km]
      #setorder(qid_dup, km)
      for (k in qid_dup) {
        qid_k <- qid[km == k, ID_F]
        #new_col = paste('q_km', k, sep = '_')
        segment_data[case == case.list[[i]],
                     eval(qid_k[1]) := rowSums(.SD, na.rm = TRUE), 
                     .SDcols = qid_k
                     ]
        segment_data[case == case.list[[i]], 
                     c(qid_k[-1]) := as.list(rep(NA, length(qid_k[-1])))
                     ]
        # change new_col to qid_k[1] for merging with id_tbl and get KM
        #names(segment_data)[names(segment_data) == new_col] <- qid_k[1]
      }
    }
  }
  if (isTRUE(get.max)) {
    if (isTRUE(verbose)) print('Calculating max values....')
    segment_data <- segment_data[, lapply(.SD, max, na.rm = TRUE),
                         .SDcols = -c("ts"), by = case] %>%
      melt(id.vars = 'case', variable.name = 'ID_F', value.name = 'scheitel')
    segment_data[is.infinite(scheitel), scheitel := NA]
    segment_data <- merge(segment_data,
                          id_tbl[, .SD, .SDcols = -c('ID')],
                          by = c('case', 'ID_F'), sort = FALSE)
  } #else{
  #   segment_data <- segment_data %>%
  #     melt(id.vars = c('ts', 'case'),
  #          variable.name = 'ID_F',
  #          value.name = 'value') %>%
  #     merge(
  #           id_tbl[, .SD, .SDcols = -c('ID')],
  #           by = c('case', 'ID_F'), sort = FALSE)
  # }


  return(segment_data[!is.na(scheitel)])
}


#' Get related time series for a measure
#' @param name Name of the measure
#' @param case.list List of the cases
#' @param case.desc Case name standardized
#' @param param Parameter discharge/waterlevel
#' @param sobek.project Path to sobek project
#' @param w.canal Should w.canal be inculded
#' @param master.tbl Master table
#' @param verbose Should some message be displayed?
#' @return a data.table
#' @export
get_polder_data <- function(
  name = NULL,
  case.list = NULL,
  case.desc = case.list,
  param = NULL,
  sobek.project = NULL,
  w.canal = TRUE,
  upstream = FALSE,
  master.tbl = NULL,
  verbose = TRUE,
  q.in = TRUE,
  q.out = TRUE
){
  # search for IDs
  id.tbl <- get_id_tbl(
    name = name,
    case.list = case.list,
    drv = FALSE,
    case.desc = case.desc,
    master.tbl = master.tbl
  )
  # id.tbl <- id.tbl[!grepl(".+_Vol", besonderheit)]
  if (param == 'discharge'){
    id.tbl <- id.tbl[!grepl('w', ID_TYPE)|grepl("Innen", besonderheit)]
  } else{
    id.tbl <- id.tbl[!grepl('q', ID_TYPE)|grepl("Einlass|Auslass", besonderheit)]
  }
  id.tbl[, col_name := str_match(besonderheit,
                      ".+_(Einlass[^;]*|Auslass[^;]*|Nach|Vor|Innen)")[,2]]
  # get results for each case
  id_data_list <- list()
  for (i in seq_along(case.list)){
    id_tbl_tmp <- id.tbl[case == case.list[[i]]]
    if (param == 'discharge'){
      # this take only the first row, if there is none, it should get an NA
      if (isTRUE(upstream)){
        id_vor <- id_tbl_tmp[grepl('_Vor', besonderheit) &
                             grepl('mID|qID', ID_TYPE)
                             ][1]
      }
      id_nach <- id_tbl_tmp[grepl('_Nach', besonderheit)&
                              grepl('mID|qID', ID_TYPE)
                            ][1]
    } else{
      if (isTRUE(upstream)) {
        id_vor <- id_tbl_tmp[grepl('_Vor', besonderheit) &
                               grepl('mID|wID', ID_TYPE)][1]
      }
      id_nach <- id_tbl_tmp[grepl('_Nach', besonderheit)&
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
        colname(id_vor_data) <- c('ts', 'Vor', 'case')
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
        colname(id_vor_data) <- c('ts', 'Nach', 'case')
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
    if (nrow(id_ein) == 0){
      id_ein <- id_tbl_tmp[grepl('.+_Einlass', besonderheit) &
                             grepl('mID', ID_TYPE)
                           ]
    }
    id_ein[, col_name := str_match(besonderheit, ".+_(Einlass[^;]*)")[, 2]]
    id_aus <- id_tbl_tmp[grepl('.+_Auslass', besonderheit) &
                           grepl('qID|sID', ID_TYPE)
                         ]
    if (nrow(id_aus) == 0){
      id_aus <- id_tbl_tmp[grepl('.+_Auslass', besonderheit) &
                             grepl('mID', ID_TYPE)
                           ]
    }
    id_aus[, col_name := str_match(besonderheit, ".+_(Auslass[^;]*)")[, 2]]
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
    if (length(sID_list) > 0){
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
      if (nrow(id_data_tmp) > 0){
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
      if (nrow(id_data_tmp) > 0){
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
    if (isTRUE(w.canal)){
      id_mitte <- id_tbl_tmp[col_name == 'Innen' &
                               grepl("mID|wID", ID_TYPE)][1]
      id_mitte_args <- list(
        case.list = case.list[[i]],
        sobek.project = sobek.project,
        id_mitte_type = id_mitte$ID_F,
        param = "waterlevel",
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
  rm(id_data_list, id_data_tmp)
  return(id_data)
}


#' Get total volume for one measure
#' @param name Name of the measure
#' @param case.list List of the cases
#' @param case.desc Case name standardized
#' @param param Parameter discharge/waterlevel
#' @param sobek.project Path to sobek project
#' @param master.tbl Master table
#' @param verbose Should some message be displayed?
#' @return a data.table
#' @export
get_polder_volume <- function(
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
  id_vol <- id_tbl[grepl(".*_Vol", besonderheit) & ID_TYPE == 'wID']
  id_data_list <- list()
  for (i in seq_along(case.list)){
    id_tbl_tmp <- id_vol[case == case.list[i]]
    if (nrow(id_tbl_tmp) > 0){
      # id_vol <- id_tbl_tmp[grepl('.*_Vol', besonderheit) & ID_TYPE == 'wID']
      id_vol_args <- list(case.list = case.list[[i]],
                          sobek.project = sobek.project,
                          wID = id_vol$ID_F,
                          param = "Volume",
                          verbose = FALSE)
      id_data_tmp <- do.call(his_from_case, id_vol_args)
      id_data_tmp <- id_data_tmp[, rowSums(.SD, na.rm = TRUE), by = case,
                                 .SDcols = -c('ts')]
      id_data_tmp <- id_data_tmp[, round(max(V1)/10^6, 2), by = case]
      colnames(id_data_tmp) <- c('case', 'Volume_max')
    } else{
      id_data_tmp <- data.table(case = case.list[[i]], Volume_max = "k.A.")
    }
    id_data_list[[i]] <- id_data_tmp
  }
  id_data_vol <- rbindlist(id_data_list)
  rm(id_data_list, id_data_tmp)
  # id_data_vol
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
  do.par = FALSE
){
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
  if (isTRUE(do.par)) {
    if (isTRUE(verbose)) {
      print(paste('Getting data for',
                  round(nrow(id_tbl)/length(case.list)),
                  'ID(s) in', length(case.list), 'case(s)'))
      print('Your computer will be overloaded for a short time. Please be patient...')
    }
    # require("doParallel", quietly = TRUE)
    # require("foreach", quietly = TRUE)
    doParallel::registerDoParallel(parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    drv_data <- 
      foreach::foreach(i = 1:length(case.list), .combine = rbind) %dopar% {
        if (param == 'discharge') {
          tmp <- his_from_case(
            case.list = case.list[[i]],
            sobek.project = sobek.project,
            param = param,
            qID = id_tbl[case == case.list[[i]] &
                           ID_TYPE == 'qID', ID_F],
            verbose = FALSE
          )
        } else{
          tmp <- his_from_case(
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
    print(paste('Getting data for',
                round(nrow(id_tbl)/length(case.list)),
                'ID(s) in', length(case.list), 'case(s)'))
    drv_data_list <- list()
    if (param == 'discharge') {
      for (i in seq_along(case.list)){
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
      for (i in seq_along(case.list)){
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
    drv_data <- rbindlist(drv_data_list, use.names = FALSE)
  }
  
  
  if (isTRUE(get.max)){
    drv_data <- drv_data[, lapply(.SD, max, na.rm = TRUE),
                             .SDcols = -c("ts"), by = case] %>%
      melt(id.vars = 'case', variable.name = 'ID_F', value.name = 'scheitel')
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
#' @param q.in Logical. Should discharge through the inlet be included?
#' @param q.out Logical. Should discharge through the outlet be included?
#' @param w.canal Logical. Should max water level inside the measure be included?
#' @param volume Logical. Should max volume be included?
#' @param ref.mID ID of Bezugspegel
#' @param compare.by How should the delta value be calculated (compare by location, or by ex. vgf)
#' @param group.by How the case should be grouped for comparing. Default is grouping by (1 1, 2 2, 3 3...)
#' @return a data.table
#' @export
get_polder_max <- function(
  name = NULL,
  case.list = NULL,
  case.desc = case.list,
  param = NULL,
  sobek.project = NULL,
  q.in = TRUE,
  q.out = TRUE,
  w.canal = TRUE,
  volume = TRUE,
  ref.mID = NULL,
  cmp.sort = FALSE,
  compare.by = 'zustand',
  group.by = compare.by,
  master.tbl = NULL,
  verbose = TRUE
){
  stopifnot(
    compare.by %in% c('zustand', 'vgf', 'hwe', 'notiz', 'zielpegel') &
      group.by %in% c('zustand', 'vgf', 'hwe', 'notiz', 'zielpegel')
  )
  stopifnot(!is.null(name) & !is.null(master.tbl) &
              !is.null(sobek.project) & !is.null(case.list)
            )
  case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
  cmp_vars <- unique(case_tbl[, get(compare.by)])
  if (isTRUE(cmp.sort)) cmp_vars <- sort(cmp_vars)
  if (length(cmp_vars) != 2) {
    stop('compare.by must have two values not: ',
         str_flatten(cmp_vars, collapse = ", " ))
  }
  grp_vars <- unique(case_tbl[, get(group.by)])
  # if (is.null(compare.by) | is.null(group.by)) {
  #   stop('For caculating delta, compare.by and group.by must be specified!')
  # }
  total_case <-
    unique(as.vector(outer(cmp_vars, grp_vars, paste, sep = "_")))
  if (compare.by != group.by){
    if (length(total_case) != length(case.list)) {
      stop("Combination of compare.by and group.by is not distinct
             for calculating delta")
    }
  } else{
    if (!near(length(total_case)/ 2, length(case.list), 0.1)) {
      stop("Combination of compare.by and group.by is not distinct
             for calculating delta")
    }
  }
  if (isTRUE(verbose)) print('Reading data at the measure...')
  id_data <- get_polder_data(
    name = name,
    case.list = case.list,
    case.desc = case.desc,
    param = param,
    upstream = TRUE,
    sobek.project = sobek.project,
    master.tbl = master.tbl
  )
  # reading data for ref.mID
  if (!is.null(ref.mID)) {
    if (isTRUE(verbose)) print('Reading data for ref.mID...')
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
  if (isTRUE(volume)){
    if (isTRUE(verbose)) print('Reading volume...')
    id_vol <- get_polder_volume(
      name = name,
      case.list = case.list,
      case.desc = case.desc,
      sobek.project = sobek.project,
      master.tbl = master.tbl
    )
    id_data_max <- merge(id_data_max, id_vol, by = 'case', sort = FALSE)
  }
  col_get_delta <- colnames(id_data_max)[-1]
  id_data_max <- merge(id_data_max, case_tbl, by = 'case', sort = FALSE)
  id_data_max[, group := seq_len(.N), by = eval(compare.by)]
  if (compare.by != group.by) {
    for (i in col_get_delta) {
      data_tbl_delta <-
        dcast(id_data_max,
              group  ~ get(compare.by) + get(group.by)  ,
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
        variable.name = group.by,
        value.name = paste('Delta', i, sep = '_'),
        sort = FALSE
      )
      data_tbl_delta[, eval(group.by) :=
                       str_replace(get(group.by),
                                   paste('Delta_', i, "_", sep = ""), '')
                     ]
      id_data_max <- merge(id_data_max, data_tbl_delta,
                           by = c(group.by, 'group'), sort = FALSE)
    }
  } else{
    for (i in col_get_delta){
      data_tbl_delta <-
        dcast(id_data_max, group  ~ get(compare.by),
              value.var = i)
      col_1 <- cmp_vars[1]
      col_2 <- cmp_vars[2]
      delta_col_name <- paste('delta', i, sep = '_')
      data_tbl_delta[, eval(delta_col_name) := get(col_1) - get(col_2)]
      data_tbl_delta[, eval(col_1) := NULL]
      data_tbl_delta[, eval(col_2) := NULL]
      id_data_max <-
        merge(id_data_max, data_tbl_delta, by = 'group', sort = FALSE)
    }
  }
  col_keep <- c('case',
                  col_get_delta,
                  paste('Delta', col_get_delta, sep = "_"),
                  group.by)
  if (group.by != compare.by) col_keep <- c(col_keep, compare.by)
  id_data_max <- id_data_max[, .SD, .SDcols = col_keep]
  return(id_data_max)
}