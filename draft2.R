#' Get Nach, Vor, Innen, Einlass, Auslass data for one measure
#' @param name Name of the measure
#' @param case.list List of the cases
#' @param case.desc Case name standardized
#' @param param Parameter discharge/waterlevel
#' @param sobek.project Path to sobek project
#' @param master.tbl Master table
#' @param verbose Should some message be displayed?
#' @import data.table
#' @importFrom stringr str_match
#' @return a data.table
.get_discharge_for_case <- function(
  name = NULL,
  case.list = NULL,
  case.desc = NULL,
  sobek.project = NULL,
  W.innen = NULL,
  master.tbl = NULL,
  verbose = TRUE
){
  # search for IDs
  # case.tbl <- .parsing_case_name(case.desc = case.desc, orig.name = case.list)
  id.tbl <- .get_id_tbl(name = name, case.list = case.list,
                        case.desc = case.desc, master.tbl = master.tbl
  )
  id.tbl <- id.tbl[!grepl(".+_Vol", besonderheit)]
  id.tbl <- id.tbl[!grepl('w', ID_TYPE)]
  # id.tbl <- id.tbl[!grepl('q', ID_TYPE)|grepl("Einlass|Auslass", besonderheit)]
  # id.tbl[, parameter := param]
  # id.tbl[grepl('_Einlass|_Auslass', besonderheit), parameter := 'discharge']
  id.tbl[, col_name := str_match(besonderheit,
                                 ".+_(Einlass[^,;]*|Auslass[^,;]*|Nach|Vor|Innen)")[,2]]
  # get results for each case
  id_data_list <- list()
  for (i in seq_along(case.list)){
    id_tbl_tmp <- id.tbl[case == case.list[[i]]]
    id_mitte <- id_tbl_tmp[col_name == 'Innen'][1]
    # this take only the first row, if there is none, it should get an NA
    id_vor <- id_tbl_tmp[col_name == 'Vor'][1]
    id_nach <- id_tbl_tmp[col_name == 'Nach'][1]
    # cannot combine with the Auslass, Einlass because they always take 'discharge'
    #----get data for Einlass/Auslass----
    id_ein <- id_tbl_tmp[grepl('.+_Einlass', besonderheit) &
                           grepl('qID|mID', ID_TYPE)]
    if (nrow(id_ein) == 0) {
      id_ein <- id_tbl_tmp[grepl('.+_Einlass', besonderheit) &
                             grepl('sID', ID_TYPE)]
    }
    id_aus <- id_tbl_tmp[grepl('.+_Auslass', besonderheit) &
                           grepl('qID|mID', ID_TYPE)]
    if (nrow(id_aus) == 0) {
      id_aus <- id_tbl_tmp[grepl('.+_Auslass', besonderheit) &
                             grepl('sID', ID_TYPE)]
    }
    id_tmp_list <- c(id_vor$ID_F, id_nach$ID_F, 
                     id_ein$ID_F, id_aus$ID_F,
                     id_mitte$ID_F
                     )
    id_tmp_col <- c(id_vor$col_name, id_nach$col_name, 
                     id_ein$col_name, id_aus$col_name,
                     id_mitte$col_name
    )
    id_tmp_type <- c(id_vor$ID_TYPE, id_nach$ID_TYPE, 
                     id_ein$ID_TYPE, id_aus$ID_TYPE,
                     id_mitte$ID_TYPE
    )
    mID_list <- id_tmp_list[grepl('m', id_tmp_type)]
    # qID or sID can be only one
    qID_list <- id_tmp_list[grepl('q', id_tmp_type)]
    sID_list <- id_tmp_list[grepl('s', id_tmp_type)]
    wID_list <- id_tmp_list[grepl('w', id_tmp_type)]
    data_tmp <- list()
    ID_list <- list(mID = mID_list, qID = qID_list, 
                    sID = sID_list, wID = wID_list
                    )
    ID_list <- ID_list[sapply(ID_list, function(x) length(x) >0, 
                       USE.NAMES = FALSE)]
    ID_list_data <- list()
    for (j in seq_along(ID_list)){
      j_args <- list(
        case.list = case.list[[i]],
        sobek.project = sobek.project,
        param = param,
        id_type = ID_list[[j]],
        verbose = FALSE
      )
      names(j_args)[4] <- names(ID_list[j])
      j_data <- do.call(his_from_case, j_args)
      ID_list_data[[j]] <- j_data
    }
    
    
    setcolorder(id_data_tmp, c('ts', id_ein_aus_list, 'case'))
    colnames(id_data_tmp) <- c('ts', id_ein_aus_cols, 'case')
    id_data_tmp <- merge(id_data_tmp, id_vor_nach_data, by = 'ts', sort = FALSE)
    #---- get Waterlevel Innen----
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
    # id_data_tmp <- merge(ft_vor_nach, st_ab_zu, by = c('ts', 'case'))
    id_data_tmp <- merge(id_data_tmp, wt_id_mitte, by = 'ts', sort = FALSE)
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
#' @param sobek.project Path to sobek project
#' @param master.tbl Master table
#' @param verbose Should some message be displayed?
#' @import data.table
#' @return a data.table
.get_volume_for_cases <- function(
  name = NULL,
  case.desc = NULL,
  case.list = NULL,
  sobek.project = NULL,
  master.tbl
){
  # get results for each case
  id_tbl <- .get_id_tbl(name = name, case.list = case.list,
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
      id_data_tmp <- data.table(case = case.list[[i]], Volume_max = 0.00)
    }
    id_data_list[[i]] <- id_data_tmp
  }
  id_data_vol <- rbindlist(id_data_list)
  rm(id_data_list, id_data_tmp)
  id_data_vol
  return(id_data_vol)
}
