library(data.table)
library(testthat)
library(sobekio)
library(tidyverse)
#----draft settings-----
so_prj <- 'd:/so21302/rhein29a.lit'
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
param = 'waterlevel'
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
id_data <- .get_data_for_cases(name = name,
  case.list = case.list, case.desc = case.desc, sobek.project = sobek.project,
  param = param, master.tbl = master.tbl
)
id_tbl_test <- .get_data_for_cases(
  name = 'Doebeltitz',
  case.list <- c(
    'BEZ_WB_HW2006_mHAV_1.6890',
    'PZ_WB_HW2006_mHAV_1.6890_v2'

  ),
  case.desc <- c(
    "BEZUG_WB_HW2006_VGF1.6890_mHAV",
    "PLAN_WB_HW2006_VGF1.6890_mHAV_v2"
  ),
  param = 'waterlevel',
  sobek.project = elbe_prj,
  master.tbl = elbe_tbl
)
#----teset scenario----
plot_measure_scenario(
  name = 'Doebeltitz',
  case.list <- c(
    'BEZ_WB_HW2006_mHAV_1.6890',
    'PZ_WB_HW2006_mHAV_1.6890_v2'

  ),
  case.desc <- c(
    "BEZUG_WB_HW2006_VGF1.6890_mHAV",
    "PLAN_WB_HW2006_VGF1.6890_mHAV_v2"
  ),
  param = 'waterlevel',
  y2.scale = 0.025,
  sobek.project = elbe_prj,
  ref.mID = NULL,
  Q.zu = TRUE,
  Q.ab = FALSE,
  W.innen = FALSE,
  delta.pegel = FALSE,
  delta.measure = TRUE,
  V.max = TRUE,
  polder.F = NULL,
  polder.Z = NULL,
  # h.lines = c(4200, 4500),
  text.pos = 0.01,
  v.just = 1,
  zoom = NULL,
  verbose = TRUE,
  master.tbl = elbe_tbl
)
#----test for rhein-----
plot_measure_scenario(
  name = 'Guntersblum',
  case.list <- c(
    'Planzustand_Eich_TEST_ct0_HW1988_Mittel_CL866',
    'BFG18_LUBW27_LFU27_HW1988_Mittel'

  ),
  case.desc <- c(
    "Planzustand_ZPK_HW1988_Mittel_CL866",
    "Bezugszustand_ZPK_HW1988_Mittel_LUBW_LFU27"
  ),
  param = 'discharge',
  y2.scale = 25,
  sobek.project = so_prj,
  ref.mID = NULL,
  Q.zu = TRUE,
  Q.ab = FALSE,
  W.innen = FALSE,
  delta.pegel = FALSE,
  delta.measure = TRUE,
  V.max = TRUE,
  polder.F = NULL,
  polder.Z = NULL,
  # h.lines = c(4200, 4500),
  text.pos = 0.01,
  v.just = 1,
  zoom = NULL,
  verbose = TRUE,
  master.tbl = rhein_tbl
)
