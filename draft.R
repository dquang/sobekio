library(data.table)
library(testthat)
library(sobekio)
library(tidyverse)
#----draft settings-----
so_prj <- 'd:/rhein.lit'
rhein_tbl <- fread("D:/rhein_kurz.txt", sep = "\t")
elbe_prj <- "D:\\NHWSP_PLAN_Skript_Auswertung_TEST\\PZ_TEST.lit"
elbe_tbl <- fread("D:\\NHWSP_PLAN_Skript_Auswertung_TEST\\sobekio_master_tbl_example_PZ_v2_q.txt", sep = "\t")
elbe_tbl[nchar(BEZUG) == 0, BEZUG := ID]
elbe_tbl[, PLAN := ID]
elbe_tbl$km <- NA
master.tbl <- elbe_tbl

name = NULL
case.list = NULL
case.desc = case.list
sobek.project = NULL
master.tbl = NULL
param = 'discharge'
q.in = FALSE
q.out = FALSE
w.canal = FALSE
ref.mID = NULL
y2.scale = 25
h.lines = NULL
delta.pegel = FALSE
delta.measure = TRUE
compare.by = 'zustand'
plot.title = NULL
lt.name = 'Linienart'
color.name = 'Farbe'
# v.max = TRUE
text.pos.x = 0
text.pos.y = 1
date.break = '3 days'
date.label = '%d.%m.%y'
text.size = 12
text.x.angle = 0L
polder.f = NULL
polder.z = NULL
verbose = TRUE
id_data <- .get_data_for_cases(name = name,
  case.list = case.list, case.desc = case.desc, sobek.project = sobek.project,
  param = param, master.tbl = master.tbl
)
id_tbl_test <- get_data_tbl(
  name = 'Doebeltitz',
  case.list <- c(
    'BEZ_WB_HW2006_mHAV_1.6890',
    'PZ_WB_HW2006_mHAV_1.6890_v2'

  ),
  w.canal = FALSE,
  case.desc <- c(
    "BEZUG_WB_HW2006_VGF1.6890_mHAV",
    "PLAN_WB_HW2006_VGF1.6890_mHAV_v2"
  ),
  param = 'waterlevel',
  sobek.project = elbe_prj,
  master.tbl = elbe_tbl
)
#----teset scenario----
plot_polder(
  name = 'Doebeltitz',
  case.list <- c(
    'BEZ_WB_HW2006_mHAV_1.6890',
    'PZ_WB_HW2006_mHAV_1.6890_v2'

  ),
  case.desc <- c(
    "BEZUG_WB_HW2006_VGF1.6890_mHAV",
    "PLAN_WB_HW2006_VGF1.6890_mHAV_v2"
  ),
  param = 'discharge',
  y2.scale = 25,
  sobek.project = elbe_prj,
  ref.mID = NULL,
  q.in = TRUE,
  q.out = FALSE,
  w.canal = TRUE,
  # delta.pegel = FALSE,
  # delta.measure = TRUE,
  # h.lines = c(4200, 4500),
  # text.pos.x = 0.01,
  verbose = TRUE,
  master.tbl = elbe_tbl
)
#----test for rhein-----
his_from_case('Bezugszustand_ZPK_HW2003_Selten_1663_newReg', 
              so_prj, param = 'discharge', sID = 'guntersblum_zu')
plot_polder_scenario(
  name = 'Langel',
  case.list <- c(
    'Bezugszustand_ZPK_HW1988_Selten_1828_newReg',
    'Planzustand_Eich_TEST_ct0_HW1988_Mittel_CL866'
  ),
  case.desc <- c(
    "2. Bezugszustand_ZPK_HW1988_Selten_1828_newReg",
    "1. Planzustand_ZPK_HW1988_Selten_CL866"
  ),
  param = 'discharge',
  y2.scale = 50,
  sobek.project = so_prj,
  # ref.mID = list(name = 'Pegel Mainz', ID = 'P_Mainz', type = 'mID'),
  ref.mID = list('Bezugspegel Mainz' = 'P_Mainz'),
  # facet.by = 'zustand',
  q.in = TRUE,
  q.out = TRUE,
  w.canal = TRUE,
  delta.pegel = TRUE,
  delta.measure = TRUE,
  h.lines = list("HQ100 Mainz" = 7900),
  text.pos.x = 0.0,
  text.pos.y = 1,
  verbose = TRUE,
  master.tbl = rhein_tbl
)
g_sce
g_pol <- plot_polder(
  name = 'Langel',
  case.list <- c(
    'Planzustand_Eich_TEST_ct0_HW1988_Mittel_CL866',
    'Bezugszustand_ZPK_HW1988_Selten_1828_newReg'
    
  ),
  case.desc <- c(
    "1. Planzustand_ZPK_HW1988_Selten_CL866",
    "2. Bezugszustand_ZPK_HW1988_Selten_1828_newReg"
  ),
  param = 'discharge',
  facet.by = 'zustand',
  y2.scale = 50,
  sobek.project = so_prj,
  # ref.mID = list(name = 'Pegel Mainz', ID = 'P_Mainz', type = 'mID'),
  ref.mID = list('Bezugspegel Mainz' = 'P_Mainz'),
  # facet.by = 'zustand',
  q.in = TRUE,
  q.out = TRUE,
  w.canal = TRUE,
  # delta.pegel = TRUE,
  # delta.measure = TRUE,
  h.lines = list("HQ100 Mainz" = 7900),
  text.pos.x = 0.0,
  text.pos.y = 1,
  verbose = TRUE,
  master.tbl = rhein_tbl
)

# Test Donau
donau_prj <- "D:/Mastertabelle_RSkript/Testcases_Quang"
donau_tbl <- fread("D:/Mastertabelle_RSkript/donau_tbl.txt")
donau_tbl[, BEZUG := ID]
donau_tbl[, PLAN := ID]
Helmeringen

name = 'Helmeringen'
case.list <- c(
  'BEZUG_HW1999_1_000_GW_halbiert',
  'PLAN_gest_HW1999_1_000_GW_halbiert'
  
)
case.desc <- c(
  'BEZUG_zp0_HW1999_1000_GW_halbiert',
  'PLAN_gest_HW1999_1000_GW_halbiert'
  
)
param = 'discharge'
y2.scale = 25
sobek.project = donau_prj
ref.mID = NULL
q.in = FALSE
q.out = FALSE
w.canal = FALSE
delta.pegel = FALSE
delta.measure = TRUE
V.max = TRUE
polder.F = NULL
polder.Z = NULL
h.lines = c(4200, 4500)
text.pos.x = 0.01
v.just = 1
zoom = NULL
verbose = TRUE
master.tbl = donau_tbl

system.time(
  g_helm <- plot_polder_scenario(
    name = 'Helmeringen',
    case.list <- c(
      'BEZUG_HW1999_1_000_GW_halbiert',
      'PLAN_gest_HW1999_1_000_GW_halbiert'
      
    ),
    case.desc <- c(
      'BEZUG_zp0_HW1999_1000_GW_halbiert',
      'PLAN_gest_HW1999_1000_GW_halbiert'
      
    ),
    param = 'discharge',
    y2.scale = 10,
    sobek.project = donau_prj,
    # ref.mID = 'P_Mainz',
    q.in = TRUE,
    # q.out = TRUE,
    w.canal = TRUE,
    delta.pegel = TRUE,
    delta.measure = TRUE,
    
  polder.f = NULL,
  polder.z = NULL,
    # h.lines = c(4200, 4500),
    text.pos.x = 0.01,
    # text.pos.y = 88,
    verbose = TRUE,
    master.tbl = donau_tbl
  )
)

tmp <- donau_tbl[grepl('Helmeringen_Vol', besonderheit)]
system.time(
  his_from_case(case.list = case.list, sobek.project = donau_prj,
                wID = tmp$ID, param = 'volume')
)
