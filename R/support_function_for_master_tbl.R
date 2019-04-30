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
  drv = FALSE,
  to.upstream = 0,
  to.downstream = 0,
  master.tbl = NULL
){
  case_tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
  id_tbl <- master.tbl[grepl(name, besonderheit)]
  if (isTRUE(drv)){
    drv_begin <- id_tbl[grepl(".*_Begin", besonderheit), km][[1]] + to.downstream
    drv_end <- id_tbl[grepl(".*_End", besonderheit), km][[1]] + to.upstream
    km_range <- sort(c(drv_begin, drv_end))
    river <- id_tbl[grepl(".*_Begin", besonderheit), river][[1]]
    id_tbl <- master.tbl[river == river & km >= km_range[1] & km <= km_range[2]]
  }
  stopifnot(nrow(id_tbl) > 1)
  # creating new columns based on case name
  for (i in seq_along(case.desc)){
    col_n <- paste('case_desc', i, sep = "_")
    id_tbl[, eval(col_n) := case.desc[[i]]]
  }
  measure_vars <- paste('case_desc', seq_along(case.desc), sep = "_")
  id_tbl <- melt(id_tbl, measure.vars = measure_vars,
                 value.name = 'case_desc')
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
  return(id_tbl)
}

#' Get related time series for one measure
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
  verbose = TRUE
){
  # search for IDs
  # case.tbl <- parse_case(case.desc = case.desc, orig.name = case.list)
  id.tbl <- get_id_tbl(name = name, case.list = case.list,
                        case.desc = case.desc, master.tbl = master.tbl
  )
  # id.tbl <- id.tbl[!grepl(".+_Vol", besonderheit)]
  if (param == 'discharge'){
    id.tbl <- id.tbl[!grepl('w', ID_TYPE)|grepl("Innen", besonderheit)]
  } else{
    id.tbl <- id.tbl[!grepl('q', ID_TYPE)|grepl("Einlass|Auslass", besonderheit)]
  }
  # id.tbl[, parameter := param]
  # id.tbl[grepl('_Einlass|_Auslass', besonderheit), parameter := 'discharge']
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
      id_vor <- id_tbl_tmp[grepl('_Vor', besonderheit) &
                             grepl('mID|wID', ID_TYPE)
                           ][1]
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
      id_vor_nach_data$Vor <- NA
    }
    #----get data for Einlass/Auslass----
    id_ein <- id_tbl_tmp[grepl('.+_Einlass', besonderheit) &
                           grepl('qID|mID', ID_TYPE)
                         ]
    if (nrow(id_ein) == 0){
      id_ein <- id_tbl_tmp[grepl('.+_Einlass', besonderheit) &
                             grepl('sID', ID_TYPE)
                           ]
    }
    id_aus <- id_tbl_tmp[grepl('.+_Auslass', besonderheit) &
                           grepl('qID|mID', ID_TYPE)
                         ]
    if (nrow(id_aus) == 0){
      id_aus <- id_tbl_tmp[grepl('.+_Auslass', besonderheit) &
                             grepl('sID', ID_TYPE)
                           ]
    }
    # check if all Einlass, Auslass besonderheit are different
    stopifnot(unique(id_ein$besonderheit) == id_ein$besonderheit)
    stopifnot(unique(id_aus$besonderheit) == id_aus$besonderheit)
    id_ein_aus_list <- c(id_ein$ID_F, id_aus$ID_F)
    id_ein_aus_type <- c(id_ein$ID_TYPE, id_aus$ID_TYPE)
    id_ein_aus_cols <- c(id_ein$col_name, id_aus$col_name)
    mID_list <- id_ein_aus_list[grepl('m', id_ein_aus_type)]
    # qID or sID can be only one
    qID_list <- id_ein_aus_list[grepl('q', id_ein_aus_type)]
    sID_list <- id_ein_aus_list[grepl('s', id_ein_aus_type)]
    if (length(qID_list) > 0) {
      qID_list_args <- list(
        case.list = case.list[[i]],
        sobek.project = sobek.project,
        qID = qID_list,
        param = 'discharge',
        verbose = FALSE
      )
      id_data_tmp <- do.call(his_from_case, args = qID_list_args)
      if (length(mID_list) > 0) {
        mID_list_args <- list(
          case.list = case.list[[i]],
          sobek.project = sobek.project,
          mID = mID_list,
          param = param,
          verbose = FALSE
        )
        mID_list_ft <- do.call(his_from_case, args = mID_list_args)
        mID_list_ft$case <- NULL
        id_data_tmp <- merge(id_data_tmp, mID_list_ft, by = 'ts', sort = FALSE)
      }
    } else{
      if (length(sID_list) > 0){
        sID_list_args <- list(
          case.list = case.list[[i]],
          sobek.project = sobek.project,
          sID = sID_list,
          param = 'discharge',
          verbose = FALSE
        )
        id_data_tmp <- do.call(his_from_case, args = sID_list_args)
        if (length(mID_list) > 0) {
          mID_list_args <- list(
            case.list = case.list[[i]],
            sobek.project = sobek.project,
            mID = mID_list,
            param = param,
            verbose = FALSE
          )
          mID_list_ft <- do.call(his_from_case, args = mID_list_args)
          mID_list_ft$case <- NULL
          id_data_tmp <- merge(id_data_tmp, mID_list_ft, by = 'ts', sort = FALSE)
        }
      }
      else{
        mID_list_args <- list(
          case.list = case.list[[i]],
          sobek.project = sobek.project,
          mID = mID_list,
          param = param,
          verbose = FALSE
        )
        id_data_tmp <- do.call(his_from_case, args = mID_list_args)
      }
    }

    setcolorder(id_data_tmp, c('ts', id_ein_aus_list, 'case'))
    colnames(id_data_tmp) <- c('ts', id_ein_aus_cols, 'case')
    id_data_tmp <- merge(id_data_tmp, id_vor_nach_data, by = 'ts', sort = FALSE)
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
  id_data_vol
  return(id_data_vol)
}


#' Get related time series for one DRV
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
#' @param get.max Should maximal value return or whole time series? Default only max value.
#' @return a data.table
#' @export
get_drv_data <- function(
  name = 'Muendelheim',
  case.list = NULL,
  case.desc = case.list,
  param = NULL,
  sobek.project = NULL,
  to.upstream = 0,
  to.downstream = 0,
  get.max = TRUE,
  master.tbl = NULL,
  verbose = TRUE
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
  drv_begin <- id_tbl[grepl(".*_Begin", besonderheit), km][[1]] + to.downstream
  drv_end <- id_tbl[grepl(".*_End", besonderheit), km][[1]] + to.upstream
  if(param == 'discharge'){
    drv_data <- his_from_case(
      case.list = case.list,
      sobek.project = sobek.project,
      param = param,
      qID = id_tbl[ID_TYPE == 'qID', ID_F],
      verbose = FALSE
    )
  } else{
    drv_data <- his_from_case(
      case.list = case.list,
      sobek.project = sobek.project,
      param = param,
      wID = id_tbl[ID_TYPE == 'wID', ID_F],
      verbose = FALSE
    )

  }
  if (isTRUE(get.max)){
    drv_data_max <- drv_data[, lapply(.SD, max, na.rm = TRUE),
                             .SDcols = -c("ts"), by = case] %>%
      melt(id.vars = 'case', variable.name = name, sort = FALSE)
    return(drv_data_max)
  } else{
    return(drv_data)
  }
}
