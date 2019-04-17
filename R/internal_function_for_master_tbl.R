#' Get id tables for one measure from the master.tbl
#' @param name Name of the measure
#' @param case.list List of the cases
#' @param case.desc Case name standardized
#' @param master.tbl Master table
#' @import data.table
#' @return a data.table
.get_id_tbl <- function(
  name = NULL,
  case.list = NULL,
  case.desc = NULL,
  master.tbl = NULL
){
  case_tbl <- .parsing_case_name(case.desc = case.desc, orig.name = case.list)
  zustand_list <- colnames(master.tbl[, .SD,
                                      .SDcols = -c('ID', 'besonderheit',
                                                   'ID_TYPE', 'km', 'river')]
  )
  zustand_list <- sort(zustand_list)
  id_tbl <- master.tbl[grepl(name, besonderheit)]
  stopifnot(nrow(id_tbl) > 1)
  # check if the zustand in cases are in listed in the master.tbl
  zustand_in_cases <- sort(unique(case_tbl$zustand))
  zustand_in_cases_len <- length(zustand_in_cases)
  stopifnot(length(zustand_list) >= zustand_in_cases_len)
  testthat::expect_equivalent(zustand_in_cases,
                              zustand_list[1:zustand_in_cases_len]
  )
  # creating new columns based on case name
  for (i in seq_along(case.desc)){
    col_n <- paste('case_desc', i, sep = "_")
    id_tbl[, eval(col_n) := case.desc[[i]]]
  }
  measure_vars <- paste('case_desc', seq_along(case.desc), sep = "_")
  id_tbl <- melt(id_tbl, measure.vars = measure_vars, sort = FALSE,
                 value.name = 'case_desc')
  id_tbl <- merge(id_tbl, case_tbl[, c('case', 'case_desc', 'zustand')],
                  by = 'case_desc')
  for (i in 1:zustand_in_cases_len){
    # get final, correct ID for each case
    id_tbl[zustand == zustand_in_cases[i],
           ID_F := get(zustand_in_cases[i])]
  }
  return(id_tbl)
}

#' Get Nach, Vor, Innen, Einlass, Auslass data for one measure
#' @param name Name of the measure
#' @param case.list List of the cases
#' @param case.desc Case name standardized
#' @param param Parameter discharge/waterlevel
#' @sobek.project Path to sobek project
#' @param master.tbl Master table
#' @param verbose Should some message be displayed?
#' @import data.table
#' @return a data.table
.get_data_for_cases <- function(
  name = NULL,
  case.list = NULL,
  case.desc = NULL,
  param = NULL,
  sobek.project = NULL,
  master.tbl = NULL,
  verbose = TRUE
){
  # search for IDs
  # case.tbl <- .parsing_case_name(case.desc = case.desc, orig.name = case.list)
  id.tbl <- .get_id_tbl(name = name, case.list = case.list,
                        case.desc = case.desc, master.tbl = master.tbl
  )
  # get results for each case
  id_data_list <- list()
  for (i in seq_along(case.list)){
    if (isTRUE(verbose)) print(paste('Getting data for case:', case.list[[i]]))
    id_tbl_tmp <- id.tbl[case == case.list[i]]
    id_mitte <- id_tbl_tmp[grepl('.+_Innen', besonderheit)&
                             grepl('mID|wID', ID_TYPE)][1]
    # id_vol <- id_tbl_tmp[grepl('.+_Vol', besonderheit) & ID_TYPE == 'wID']
    if (param == 'discharge'){
      # this take only the first row, if there is none, it should get an NA
      id_vor <- id_tbl_tmp[grepl('_Vor', besonderheit) &
                             grepl('mID|qID', ID_TYPE)
                           ][1]
      id_nach <- id_tbl_tmp[grepl('_Nach', besonderheit)&
                              grepl('mID|qID', ID_TYPE)
                            ][1]
      id_ein <- id_tbl_tmp[grepl('.+_Einlass', besonderheit) &
                             grepl('mID|qID', ID_TYPE)
                           ]
      id_aus <- id_tbl_tmp[grepl('.*_Auslass', besonderheit) &
                             grepl('mID|qID', ID_TYPE)
                           ]
    } else{
      id_vor <- id_tbl_tmp[grepl('_Vor', besonderheit) &
                             grepl('mID|wID', ID_TYPE)
                           ][1]
      id_nach <- id_tbl_tmp[grepl('_Nach', besonderheit)&
                              grepl('mID|wID', ID_TYPE)
                            ][1]
      id_ein <- id_tbl_tmp[grepl('.+_Einlass', besonderheit) &
                             grepl('mID|wID', ID_TYPE)
                           ]
      id_aus <- id_tbl_tmp[grepl('.*_Auslass', besonderheit) &
                             grepl('mID|wID', ID_TYPE)
                           ]
    }
    id_ein_cols <- 'Einlass'
    id_aus_cols <- 'Auslass'
    if (length(id_ein$ID) > 1){
      id_ein_cols <- paste('Einlass', seq_along(id_ein$ID), sep = "_")
    }
    if (length(id_aus$ID) > 1) {
      id_aus_cols <- paste('Auslass', seq_along(id_aus$ID), sep = "_")
    }
    id_list <- unlist(c(id_vor$ID, id_nach$ID, id_ein$ID, id_aus$ID))
    id_list_cols <- c('Vor', 'Nach', id_ein_cols, id_aus_cols)
    id_type_list <- unlist(
      c(id_vor$ID_TYPE, id_nach$ID_TYPE,
        id_ein$ID_TYPE, id_aus$ID_TYPE
      )
    )
    mID_list <- id_list[grepl('mID', id_type_list)]
    wqID_list <- id_list[!grepl('mID', id_type_list)]
    if (length(wqID_list) > 0){
      wqID_list_args <- list(
        case.list = case.list[[i]],
        sobek.project = sobek.project,
        id_type = wqID_list,
        param = param,
        verbose = FALSE
      )
      names(wqID_list_args)[3] <- id_nach$ID_TYPE
      id_data_tmp <- do.call(his_from_case, args = wqID_list_args)
      if (length(mID_list) > 0){
        mID_list_args <- list(
          case.list = case.list[[i]],
          sobek.project = sobek.project,
          mID = mID_list,
          param = param,
          verbose = FALSE
        )
        mID_list_ft <- do.call(his_from_case, args = wqID_list_args)
        mID_list_ft$case <- NULL
        id_data_tmp <- merge(id_data_tmp, mID_list_ft, by = 'ts', sort = FALSE)
      }
    } else{
      mID_list_args <- list(
        case.list = case.list[[i]],
        sobek.project = sobek.project,
        mID = mID_list,
        param = param,
        verbose = FALSE
      )
      id_data_tmp <- do.call(his_from_case, args = wqID_list_args)
    }
    setcolorder(id_data_tmp, c('ts', id_list, 'case'))
    colnames(id_data_tmp) <- c('ts', id_list_cols, 'case')
    # st_ab_zu <- his_from_case(case.list = case.name,
    #                           sobek.project = sobek.project,
    #                           sID = id_st$ID_F, param = 'discharge',
    #                           verbose = FALSE
    # )
    # st_ab_zu_cname <- c('ts', id_st$besonderheit, 'case')
    # st_ab_zu_cname <- sub('.*_Zu', 'Einlass', id_st$besonderheit)
    # st_ab_zu_cname <- sub('.*_Ab', 'Auslass', st_ab_zu_cname)
    # colnames(st_ab_zu) <- c('ts', st_ab_zu_cname, 'case')
    # get Waterlevel
    id_mitte_args <- list(case.list = case.list[[i]],
                          sobek.project = sobek.project,
                          id_mitte_type = id_mitte$ID_F,
                          param = "waterlevel",
                          verbose = FALSE)
    names(id_mitte_args)[3] <- id_mitte$ID_TYPE
    wt_id_mitte <- do.call(his_from_case, id_mitte_args)
    colnames(wt_id_mitte) <- c('ts', 'W_innen', 'case')
    # merging data
    # id_data_tmp <- merge(ft_vor_nach, st_ab_zu, by = c('ts', 'case'))
    id_data_tmp <- merge(id_data_tmp, wt_id_mitte, by = c('ts', 'case'))
    id_data_list[[i]] <- id_data_tmp
  }
  id_data <- rbindlist(id_data_list)
  rm(id_data_list, id_data_tmp)
  return(id_data)
}


#' Get Nach, Vor, Innen, Einlass, Auslass data for one measure
#' @param name Name of the measure
#' @param case.list List of the cases
#' @param case.desc Case name standardized
#' @param param Parameter discharge/waterlevel
#' @sobek.project Path to sobek project
#' @param master.tbl Master table
#' @param verbose Should some message be displayed?
#' @import data.table
#' @return a data.table
.get_volume_for_cases <- function(
  case.list = NULL,
  sobek.project = NULL,
  id.vol = NULL
){
  # get results for each case
  id_data_list <- list()
  for (i in seq_along(case.list)){
    id_tbl_tmp <- id.vol[case == case.list[i]]
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
      id_data_tmp <- data.table(case = case.list[[i]], Volume_max = 0.00)
    }
    id_data_list[[i]] <- id_data_tmp
  }
  id_data_vol <- rbindlist(id_data_list)
  rm(id_data_list, id_data_tmp)
  id_data_vol
  return(id_data_vol)
}
