library(data.table)
library(testthat)
#----draft settings-----
elbe_prj <- "D:\\NHWSP_PLAN_Skript_Auswertung_TEST\\PZ_TEST.lit"
elbe_tbl <- fread("\\\\mt1.fs.bafg.de\\copy$\\M2\\Reeps\\NHWSP_PLAN_Skript_Auswertung_TEST\\sobekio_master_tbl_example_PZ_v2_q.txt", sep = "\t")
elbe_tbl[nchar(BEZUG) == 0, BEZUG := ID]
elbe_tbl[, PLAN := ID]
cname_orig <- c(
  'BEZ_WB_HW2006_mHAV_VGF1.6890',
  'PZ_WB_HW2006_mHAV_VGF1.6890_v2'
)
case.list <- c(
  'BEZ_WB_HW2006_mHAV_VGF1.6890',
  'PZ_WB_HW2006_mHAV_VGF1.6890_v2'
  )

name = 'Doebeltitz'
case.desc <- c(
  "BEZUG_WB_HW2006_VGF1.6890_mHAV",
  "PLAN_WB_HW2006_VGF1.6890_mHAV_v2"
)
param = 'discharge'
y2.scale = 25
sobek.project = elbe_prj
ref.mID = NULL
Q.zu = TRUE
Q.ab = FALSE
W.innen = FALSE
delta.pegel = FALSE
delta.measure = TRUE
V.max = TRUE
polder.F = NULL
polder.Z = NULL
h.lines = NULL
text.pos = 0.01
v.just = 1
zoom = NULL
master.tbl = elbe_tbl
#----processing case names----
.parsing_case_name <- function(case.desc, orig.name = case.desc){
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
  return(result)
}
case_tbl <- .parsing_case_name(case.desc = case.desc, orig.name = case.list)
elbe_tbl$km <- NA
master.tbl <- elbe_tbl
#----getting IDs-----
# this function create id_tbl for all cases with correct selection of cases
# regarding to Zustand
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

id_tbl <- .get_id_tbl(
  name = 'Doebeltitz',
  case.desc = case.desc,
  case.list = case.list,
  master.tbl = elbe_tbl
)


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
