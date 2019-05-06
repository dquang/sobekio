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

river = 'Rhein'
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
peak.nday = NULL
peak.pegel = FALSE
delta.pegel = FALSE
delta.measure = TRUE
delta.line = TRUE
delta.line.at = 'reference'
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

#----test for rhein-----
his_from_case('Bezugszustand_ZPK_HW2003_Selten_1663_newReg',
              so_prj, param = 'discharge', sID = 'guntersblum_zu')
plot_polder_scenario(
  name = 'Worringer',
  case.list = c(
    'Planzustand_ZPK_HW1988_Selten_Nur_Eich',
    'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit'),
  case.desc = c(
    'PZ ohne Worringer_ZPK_HW1988_Selten_Nur_Eich',
    'PZ mit Worringer_ZPK_HW1988_Selten_Eich_Wor'),
  y2.scale = 0.01,
  sobek.project = so_prj,
  param = 'waterlevel',
  # ref.mID = list(name = 'Pegel Mainz', ID = 'P_Mainz', type = 'mID'),
  ref.mID = 'p_koeln',
  # facet.by = 'zustand',
  q.in = TRUE,
  q.out = TRUE,
  w.canal = TRUE,
  delta.line = TRUE,
  delta.pegel = TRUE,
  delta.measure = TRUE,
  peak.nday = 5,
  # h.lines = list("HQ100 Mainz" = 7900),
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
#----test get_polder_max----
eich_max <- get_polder_max(
  name = 'Guntersblum',
  case.list <- c(
    'Bezugszustand_ZPK_HW1988_Selten_1828_newReg',
    'Planzustand_Eich_TEST_ct0_HW1988_Mittel_CL866'
  ),
  case.desc <- c(
    "Bezugszustand_ZPK_HW1988_Selten_1828_newReg",
    "Planzustand_ZPK_HW1988_Selten_CL866"
  ),
  compare.by = 'zustand',
  param = 'discharge',
  sobek.project = so_prj,
  master.tbl = rhein_tbl
)
#----test long profile-----
name = 'Muendelheim'
case.list = c(
  'Bezugszustand_ZPK_HW1988_Mittel_1563_newReg',
  'Bezugszustand_ZPK_HW1988_Selten_1828_newReg',
  'Bezugszustand_ZPK_HW1995_Mittel_1188_newReg',
  'Bezugszustand_ZPK_HW1995_Selten_1282_newReg',
  'Bezugszustand_ZPK_HW2003_Mittel_1527_newReg',
  'Bezugszustand_ZPK_HW2003_Selten_1663_newReg',
  'BFG18_LUBW27_LFU27_HW1988_Mittel',
  'BFG18_LUBW27_LFU27_HW1988_Selten',
  'BFG18_LUBW27_LFU27_HW1995_Mittel',
  'BFG18_LUBW27_LFU27_HW1995_Selten',
  'BFG18_LUBW27_LFU27_HW2003_Mittel',
  'BFG18_LUBW27_LFU27_HW2003_Selten'
)
case.desc =  c(
  'Bezugszustand_ZPK_HW1988_Mittel_gr1',
  'Bezugszustand_ZPK_HW1988_Selten_gr2',
  'Bezugszustand_ZPK_HW1995_Mittel_gr3',
  'Bezugszustand_ZPK_HW1995_Selten_gr4',
  'Bezugszustand_ZPK_HW2003_Mittel_gr5',
  'Bezugszustand_ZPK_HW2003_Selten_gr6',
  'BFG18_ZPK_HW1988_Mittel',
  'BFG18_ZPK_HW1988_Selten',
  'BFG18_ZPK_HW1995_Mittel',
  'BFG18_ZPK_HW1995_Selten',
  'BFG18_ZPK_HW2003_Mittel',
  'BFG18_ZPK_HW2003_Selten'
)
param = 'discharge'
to.upstream = 10
to.downstream = 10
sobek.project = so_prj
master.tbl = rhein_tbl
lt.by = 'zustand'
color.by = 'vgf'
facet.by = 'hwe'
compare.by = NULL
color.name = 'Farbe'
lt.name = 'Linienart'
delta = FALSE
x.lab = 'Lage (KM)'
y.lab = ifelse(param == 'discharge',
               'Abfluss (m³/s)', 'Wasserstand (m+NHN)')
to.upstream = 0
to.downstream = 0
y2.scale = 2
plot.title = NULL
text.size = 12
text.x.top.angle = 90L
text.x.bottom.angle = 0L
reserve.x = FALSE
text.x.top.size = 8L
verbose = TRUE
plot_drv(
  name = 'Muendelheim',
  case.list = c(
    # 'Bezugszustand_ZPK_HW1988_Mittel_1563_newReg',
    # 'Bezugszustand_ZPK_HW1988_Selten_1828_newReg',
    # 'Bezugszustand_ZPK_HW1995_Mittel_1188_newReg',
    # 'Bezugszustand_ZPK_HW1995_Selten_1282_newReg',
    # 'Bezugszustand_ZPK_HW2003_Mittel_1527_newReg',
    'Bezugszustand_ZPK_HW2003_Selten_1663_newReg',
    # 'BFG18_LUBW27_LFU27_HW1988_Mittel',
    # 'BFG18_LUBW27_LFU27_HW1988_Selten',
    # 'BFG18_LUBW27_LFU27_HW1995_Mittel',
    # 'BFG18_LUBW27_LFU27_HW1995_Selten',
    'BFG18_LUBW27_LFU27_HW2003_Mittel',
    'BFG18_LUBW27_LFU27_HW2003_Selten'
  ),
  case.desc =  c(
    # 'Bezugszustand_ZPK_HW1988_Mittel_gr1',
    # 'Bezugszustand_ZPK_HW1988_Selten_gr2',
    # 'Bezugszustand_ZPK_HW1995_Mittel_gr3',
    # 'Bezugszustand_ZPK_HW1995_Selten_gr4',
    # 'Bezugszustand_ZPK_HW2003_Mittel_gr5',
    'Bezugszustand_ZPK_HW2003_Selten_gr6',
    # 'BFG18_ZPK_HW1988_Mittel_gr1',
    # 'BFG18_ZPK_HW1988_Selten_gr2',
    # 'BFG18_ZPK_HW1995_Mittel_gr3',
    # 'BFG18_ZPK_HW1995_Selten_gr4',
    'BFG18_ZPK_HW2003_Mittel_gr5',
    'BFG18_ZPK_HW2003_Selten_gr6'
  ),
  param = 'discharge',
  compare.by = 'zustand',
  group.by = 'vgf',
  # facet.by = 'hwe',
  # delta = TRUE,
  y2.scale = 10,
  to.upstream = 0,
  to.downstream = 0,
  sobek.project = 'd:/rhein.lit',
  master.tbl = rhein_tbl
)
View(g_m$data)
#----test plot_river_segment----
plot_longprofile(
  river = 'Rhein',
  from.km = 660,
  to.km = 780,
  case.list <- c(
    'Bezugszustand_ZPK_HW1988_Mittel_1563_newReg',
    'BFG18_LUBW27_LFU27_HW1988_Mittel'
  ),
  case.desc <- c(
    "Bezugszustand_ZPK_HW1988_Mittel_1563_newReg",
    "Planzustand_ZPK_HW1988_Mittel_CL866"
  ),
  param = 'waterlevel',
  sobek.project = 'd:/rhein.lit',
  highlight = c(700, 750),
  delta = TRUE,
  highlight.text = c('Begin', 'End'),
  master.tbl = rhein_tbl
)
