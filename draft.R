library(data.table)
library(testthat)
library(sobekio)
#----draft settings-----
elbe_prj <- "D:\\NHWSP_PLAN_Skript_Auswertung_TEST\\PZ_TEST.lit"
elbe_tbl <- fread("D:\\NHWSP_PLAN_Skript_Auswertung_TEST\\sobekio_master_tbl_example_PZ_v2_q.txt", sep = "\t")
elbe_tbl[nchar(BEZUG) == 0, BEZUG := ID]
elbe_tbl[, PLAN := ID]
elbe_tbl$km <- NA
master.tbl <- elbe_tbl
case.list <- c(
  'BEZ_WB_HW2006_mHAV_1.6890',
  'PZ_WB_HW2006_mHAV_1.6890_v2'
  
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
h.lines = c(4200, 4500)
text.pos = 0.01
v.just = 1
zoom = NULL
verbose = TRUE
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

id_tbl <- .get_id_tbl(name = name, case.list = case.list, case.desc = case.desc,
                      master.tbl = master.tbl)

id_vol_data <- .get_volume_for_cases(
  name = name,
  case.list = case.list,
  case.desc = case.desc,
  param = 'Volume',
  sobek.project = sobek.project,
  master.tbl = elbe_tbl
)
