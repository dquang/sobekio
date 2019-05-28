# this script is for getting results from NRW-Maßnahmen
library(sobekio)
library(data.table)
library(tidyverse)
library(cowplot)
library(pracma)

setwd("Z:/M/M2/work/duong/NHWSP-Rhein/Berichte/modelle/rhein/NRW")
# so_prj <- 'd:/rhein.lit'
#----ZPW & VGF1----
wt <- his_from_case(
  c(
    'Planzustand_ZPW_HW1988_Mittel_ohne_Niederrhein',
    'Planzustand_ZPW_HW1988_Selten_ohne_Niederrhein',
    'Planzustand_ZPW_HW1995_Mittel_ohne_Niederrhein',
    'Planzustand_ZPW_HW1995_Selten_ohne_Niederrhein',
    'Planzustand_ZPW_HW2003_Mittel_ohne_Niederrhein',
    'Planzustand_ZPW_HW2003_Selten_ohne_Niederrhein',
    'Planzustand_ZP0_HW1988_VGF1_ohne_Niederrhein',
    'Planzustand_ZP0_HW1995_VGF1_ohne_Niederrhein',
    'Planzustand_ZP0_HW1999_VGF1_ohne_Niederrhein',
    'Planzustand_ZP0_HW2003_VGF1_ohne_Niederrhein',
    'Planzustand_ZP0_HW2011_VGF1_ohne_Niederrhein',
    'Planzustand_ZP0_HW2013_VGF1_ohne_Niederrhein'
  ),
  sobek.project = so_prj,
  mID = c('p_koeln', 'p_orsoy_in', 'p_lohrwardt_in'),
  get.max = TRUE,
  param = 'waterlevel'
)
#----basic Info-----
pegel_ID <- c('p_worms', 'P_Mainz', 'p_kaub', 'p_andernach', 'p_bonn',
              'p_koeln', 'p_duesseldorf', 'p_ruhrort', 'p_wesel', 'p_rees_out',
              'p_emmerich')
hwe_name <- c(
  'hw88_m', 'hw88_s', 'hw95_m', 'hw95_s', 'hw_03m', 'hw_03s'
)
pegel_name <- c("WORMS",
                "MAINZ",
                "KAUB",
                "ANDERNACH",
                "BONN",
                "KOELN",
                "DUESSELDORF",
                "RUHRORT",
                "WESEL",
                "REES",
                "EMMERICH")
Bezugszustand <- c(
  'Bezugszustand_ZPK_HW1988_Mittel_1563_newReg',
  'Bezugszustand_ZPK_HW1988_Selten_1828_newReg',
  'Bezugszustand_ZPK_HW1995_Mittel_1188_newReg',
  'Bezugszustand_ZPK_HW1995_Selten_1282_newReg',
  'Bezugszustand_ZPK_HW2003_Mittel_1527_newReg',
  'Bezugszustand_ZPK_HW2003_Selten_1663_newReg'
)
Bezugszustand_ZPW <- c(
  'Bezugszustand_ZPW_HW1988_Mittel',
'Bezugszustand_ZPW_HW1988_Selten',
'Bezugszustand_ZPW_HW1995_Mittel',
'Bezugszustand_ZPW_HW1995_Selten',
'Bezugszustand_ZPW_HW2003_Mittel',
'Bezugszustand_ZPW_HW2003_Selten')

Planzustand_ohne_NRW_ZPW <-
  c(
    'Planzustand_ZPW_HW1988_Mittel_ohne_Niederrhein',
    'Planzustand_ZPW_HW1988_Selten_ohne_Niederrhein',
    'Planzustand_ZPW_HW1995_Mittel_ohne_Niederrhein',
    'Planzustand_ZPW_HW1995_Selten_ohne_Niederrhein',
    'Planzustand_ZPW_HW2003_Mittel_ohne_Niederrhein',
    'Planzustand_ZPW_HW2003_Selten_ohne_Niederrhein'
  )
zpw <- get_delta_table(
  case.w = Planzustand_ohne_NRW_ZPW,
  case.w.desc = cases_mit_desc,
  case.wo = Bezugszustand_ZPW,
  case.wo.desc = cases_ohne_desc,
  id.names = pegel_name,
  param = 'discharge',
  mID = pegel_ID,
  sobek.project = so_prj
)
zpw_w <- get_delta_table(
  case.w = Planzustand_ohne_NRW_ZPW,
  case.w.desc = cases_mit_desc,
  case.wo = Bezugszustand_ZPW,
  case.wo.desc = cases_ohne_desc,
  id.names = pegel_name,
  param = 'waterlevel',
  mID = pegel_ID,
  sobek.project = so_prj
)
cases_ohne_desc <- c(
  'ohne_88_Mittel',
  'ohne_88_Selten',
  'ohne_95_Mittel',
  'ohne_95_Selten',
  'ohne_03_Mittel',
  'ohne_03_Selten'
)
cases_mit_desc <- c(
  'mit_88_Mittel',
  'mit_88_Selten',
  'mit_95_Mittel',
  'mit_95_Selten',
  'mit_03_Mittel',
  'mit_03_Selten'
)
#---- Worringer long profiles----
plot_longprofile(
  river = 'Rhein',
  # from.km = 640,
  # to.km = 780,
  case.list = c(
    # case ohne Worringer
    # 'Planzustand_ZPK_HW1988_Mittel_Nur_Eich',
    # 'Planzustand_ZPK_HW1988_Selten_Nur_Eich',
    # 'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein',
    # 'Planzustand_ZPK_HW1995_Selten_ohne_Niederrhein',
    # 'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein',
    'Planzustand_ZPK_HW2003_Selten_Nur_Eich',
    # case mit Worringer
    # 'Planzustand_ZPK_HW1988_Mittel_Nur_Eich',
    # 'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit',
    # 'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein',
    # 'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit',
    # 'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein',
    'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit'
  ),
  case.desc = c(
    # 'Ohne Worringer_ZPK_HW1988_88 Mittel_Nur_Eich',
    # 'Ohne Worringer_ZPK_HW1988_Selten_Nur_Eich',
    # 'Ohne Worringer_ZPK_HW1995_95 Mittel_ohne_Niederrhein',
    # 'Ohne Worringer_ZPK_HW1995_95 Selten_ohne_Niederrhein',
    # 'Ohne Worringer_ZPK_HW2003_03 Mittel_ohne_Niederrhein',
    '2. Ohne Worringer_ZPK_HW2003_Selten_Nur_Eich',
    # 'Mit Worringer_ZPK_HW1988_88 Mittel_Nur_Eich',
    # 'Mit Worringer_ZPK_HW1988_Selten_Eich_Wor_Zeit',
    # 'Mit Worringer_ZPK_HW1995_95 Mittel_ohne_Niederrhein',
    # 'Mit Worringer_ZPK_HW1995_95 Selten_Eich_Wor_Zeit',
    # 'Mit Worringer_ZPK_HW2003_03 Mittel_ohne_Niederrhein',
    '1. Mit Worringer_ZPK_HW2003_Selten_Eich_Wor_Zeit'
  ),
  sobek.project = so_prj,
  compare.by = 'zustand',
  cmp.sort = TRUE,
  y2.scale = 20,
  y2.tick1 = -250,
  # group.by = 'hwe',
  facet.by = 'hwe',
  param = 'discharge',
  delta = TRUE,
  overlap = c('Polder_Langel', 'Ruhr', 'Lippe', 'REES'),
  master.tbl = rhein_tbl
)

#----Worringer polder plot-----
gw_wor_88s <- plot_polder_scenario(
  name = 'Worringer',
  case.list = c(
    'Planzustand_ZPK_HW1988_Selten_Nur_Eich',
    'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit'
    ),
  case.desc = c(
    'ohne Worringer_ZPK_HW1988_Selten_Nur_Eich',
    'mit Worringer_ZPK_HW1988_Selten_Eich_Wor_Zeit'),
  ref.mID = 'p_duesseldorf',
  cmp.sort = TRUE,
  q.in = TRUE,
  q.out = TRUE,
  plot.title = 'Modellergebnis für Maßnahme Worringer. Modellhochwasser 1988 Selten., Zielpegel Köln.',
  # compare.by = 'notiz',
  # facet.by = 'zustand',
  delta.line = TRUE,
  param = 'waterlevel',
  # y2.scale = 20,
  y2.tick1 = -50,
  text.x.angle = 90,
  delta.measure = TRUE,
  delta.pegel = TRUE,
  peak.nday = 5,
  date.break = '6 hours',
  date.label = '%d.%m.%y %H:%M',
  sobek.project = so_prj,
  master.tbl = rhein_tbl
)

gw_wor_95s <- plot_polder_scenario(
  name = 'Worringer',
  case.list = c(
    'Planzustand_ZPK_HW1995_Selten_ohne_Niederrhein',
    'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit'),
  case.desc = c(
    'ohne Worringer_ZPK_HW1995_Selten_ohne_Niederrhein',
    'mit Worringer_ZPK_HW1995_Selten_Eich_Wor_Zeit'),
  ref.mID = 'p_duesseldorf',
  cmp.sort = TRUE,
  q.in = TRUE,
  q.out = TRUE,
  plot.title = 'Modellergebnis für Maßnahme Worringer. Hochwasser 1995, Zielpegel Köln. VGF Selten.',
  # compare.by = 'notiz',
  # facet.by = 'zustand',
  delta.line = TRUE,
  param = 'waterlevel',
  # y2.scale = 20,
  y2.tick1 = -50,
  text.x.angle = 90,
  delta.measure = TRUE,
  delta.pegel = TRUE,
  peak.nday = 5,
  date.break = '6 hours',
  date.label = '%d.%m.%y %H:%M',
  sobek.project = so_prj,
  master.tbl = rhein_tbl
)

gw_wor_03s <- plot_polder_scenario(
  name = 'Worringer',
  case.list = c(
    'Planzustand_ZPK_HW2003_Selten_Nur_Eich',
    'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit'),
  case.desc = c(
    'ohne Worringer_ZPK_HW2003_Selten_Nur_Eich',
    'mit Worringer_ZPK_HW2003_Selten_Eich_Wor_Zeit'),
  ref.mID = 'p_duesseldorf',
  cmp.sort = TRUE,
  q.in = TRUE,
  q.out = TRUE,
  plot.title = 'Modellergebnis für Maßnahme Worringer. Hochwasser 2003, Zielpegel Köln. VGF Selten.',
  # compare.by = 'notiz',
  # facet.by = 'zustand',
  delta.line = TRUE,
  param = 'waterlevel',
  # y2.scale = 20,
  y2.tick1 = -50,
  text.x.angle = 90,
  delta.measure = TRUE,
  delta.pegel = TRUE,
  peak.nday = 5,
  date.break = '6 hours',
  date.label = '%d.%m.%y %H:%M',
  sobek.project = so_prj,
  master.tbl = rhein_tbl
)

gw_wor <- list(gw_wor_88s, gw_wor_95s, gw_wor_03s)
gw_wor_names <- paste(c('gw_worringer_88s.',
                        'gw_worringer_95s.',
                        'gw_worringer_03s.'),
                      'png', sep = ""
                        )
map2(
  gw_wor_names,
  gw_wor,
  cowplot::ggsave,
  width = 30, height = 20, units = 'cm',
  dpi = 600,
  device = 'png'
)
#----- Worringer Table----
# Bezugszustand
Bezugszustand <- c(
  'Bezugszustand_ZPK_HW1988_Mittel_1563_newReg',
  'Bezugszustand_ZPK_HW1988_Selten_1828_newReg',
  'Bezugszustand_ZPK_HW1995_Mittel_1188_newReg',
  'Bezugszustand_ZPK_HW1995_Selten_1282_newReg',
  'Bezugszustand_ZPK_HW2003_Mittel_1527_newReg',
  'Bezugszustand_ZPK_HW2003_Selten_1663_newReg'

)
# cases Planzustände mit Worringer
Planzustand_mit_Worringer <- c(
  'Planzustand_ZPK_HW1988_Mittel_Nur_Eich',
  'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit',
  'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein',
  'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit',
  'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein',
  'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit'
)
# cases Planzustände ohne Worringer
Planzustand_ohne_Worringer <- c(
  'Planzustand_ZPK_HW1988_Mittel_Nur_Eich',
  'Planzustand_ZPK_HW1988_Selten_Nur_Eich',
  'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein',
  'Planzustand_ZPK_HW1995_Selten_ohne_Niederrhein',
  'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein',
  'Planzustand_ZPK_HW2003_Selten_Nur_Eich'
)

plot_longprofile(
  from.km = 668,
  to.km = 670,
  case.list = c(
    # grep('Mittel', Planzustand_mit_Worringer, value = TRUE),
    grep('Mittel', Planzustand_mit_Worringer, value = TRUE)
                ),
  param = 'waterlevel',
  compare.by = NULL,
  color.by = 'hwe',
  # lt.by =
  sobek.project = so_prj,
  delta = FALSE,
  master.tbl = rhein_tbl
)
plot_polder(
  name = 'Langel',
  case.list = c(
    # grep('Mittel', Planzustand_mit_Worringer, value = TRUE),
    grep('Mittel', Planzustand_mit_Worringer, value = TRUE)
  ),
  param = 'waterlevel',
  facet.by = 'hwe',
  q.in = TRUE,
  y2.scale = 0.05,
  # compare.by = NULL,
  # color.by = 'hwe',
  # lt.by =
  sobek.project = so_prj,
  # delta = FALSE,
  master.tbl = rhein_tbl
)
wtk <- his_from_case(
  Planzustand_ohne_Worringer, mID = 'p_koeln', param = 'waterlevel',
  sobek.project = so_prj, get.max = TRUE
)
wtk[, p_koeln:=p_koeln-35.01]
wtk[grepl('Selten', case), c('case', 'p_koeln')]
setorder(wtk, case)
wor_fixCL_Q <- get_summary_tbl(
  name = 'Worringer',
  bezug = Bezugszustand,
  plan.ohne = Planzustand_ohne_Worringer,
  plan.mit = Planzustand_mit_Worringer,
  hwe.list = hwe,
  param = 'discharge',
  id.names = pegel_name,
  mID = pegel_ID,
  sobek.project = so_prj
)
wor_fixCL_W <- get_summary_tbl(
  name = 'Worringer',
  bezug = Bezugszustand,
  plan.ohne = Planzustand_ohne_Worringer,
  plan.mit = Planzustand_mit_Worringer,
  hwe.list = hwe,
  param = 'waterlevel',
  id.names = pegel_name,
  mID = pegel_ID,
  sobek.project = so_prj
)
wor_pzMit_vs_bz <- get_delta_table(
  name = 'Worringer',
  case.w = Planzustand_mit_Worringer,
  case.w.desc = cases_mit_desc,
  case.wo = Bezugszustand,
  case.wo.desc = cases_ohne_desc,
  html.out = TRUE,
  id.names = toupper(str_replace_all(pegel_ID, '[pP]_', '')),
  param = 'discharge',
  sobek.project = so_prj,
  mID =  pegel_ID
)
wor_pzMit_vs_bz_WL <- get_delta_table(
  name = 'Worringer',
  case.w = Planzustand_mit_Worringer,
  case.w.desc = cases_mit_desc,
  case.wo = Bezugszustand,
  case.wo.desc = cases_ohne_desc,
  html.out = TRUE,
  id.names = toupper(str_replace_all(pegel_ID, '[pP]_', '')),
  param = 'waterlevel',
  sobek.project = so_prj,
  mID =  pegel_ID
)
wor_pzOhne_vs_bz <- get_delta_table(
  name = 'Worringer',
  case.w = Planzustand_ohne_Worringer,
  case.w.desc = cases_mit_desc,
  case.wo = Bezugszustand,
  case.wo.desc = cases_ohne_desc,
  html.out = TRUE,
  id.names = toupper(str_replace_all(pegel_ID, '[pP]_', '')),
  param = 'discharge',
  sobek.project = so_prj,
  mID =  pegel_ID
)
wor_pzOhne_vs_bz_WL <- get_delta_table(
  name = 'Worringer',
  case.w = Planzustand_ohne_Worringer,
  case.w.desc = cases_mit_desc,
  case.wo = Bezugszustand,
  case.wo.desc = cases_ohne_desc,
  html.out = TRUE,
  id.names = toupper(str_replace_all(pegel_ID, '[pP]_', '')),
  param = 'waterlevel',
  sobek.project = so_prj,
  mID =  pegel_ID
)
#----- Orsoy Table----
orsoy_w_95s <- plot_polder_scenario(
  name = 'Orsoy',
  case.list = c(
    'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Muendelheim',
    'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy'
  ),
  case.desc = c(
    '2. Ohne Orsoy_ZPK_HW1995_Selten_Eich_Wor_Zeit_Muendelheim',
    '1. Mit Orsoy_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy'
  ),
  # ref.mID = 'p_duesseldorf',
  q.in = TRUE,
  q.out = TRUE,
  cmp.sort = TRUE,
  delta.line = TRUE,
  peak.nday = 4,
  y2.scale = 0.01,
  y2.tick1 = 0,
  master.tbl = rhein_tbl,
  sobek.project = so_prj,
  delta.pegel = TRUE,
  param = 'waterlevel'
)
# cases Planzustände mit Orsoy mit fixed Crest Level 24.69
Planzustand_mit_Orsoy_fixCL <- c(
  'Planzustand_ZPK_HW1988_Mittel_Nur_Eich_Orsoy_CL2469',
  'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469',
  'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein_Orsoy_CL2469',
  'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy_CL2469',
  'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein_Orsoy_CL2469',
  'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469'
)

# cases Planzustände mit Orsoy mit passenden Crest Level gemäß Wirkung von oben
Planzustand_mit_Orsoy_passCL <- c(
  'Planzustand_ZPK_HW1988_Mittel_Nur_Eich_Orsoy',
  'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy',
  'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein_Orsoy',
  'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy',
  'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein_Orsoy',
  'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy'
)
# cases Planzustände ohne Orsoy
Planzustand_ohne_Orsoy <- c(
  'Planzustand_ZPK_HW1988_Mittel_Nur_Eich_Muendelheim',
  'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Muendelheim',
  'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein_Muendelheim',
  'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Muendelheim',
  'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein_Muendelheim',
  'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Muendelheim'

)
#+++fixed CL----
orsoy_fixCL_Q <- get_summary_tbl(
  name = 'Orsoy (feste Schwellenshöhe)',
  bezug = Bezugszustand,
  plan.ohne = Planzustand_ohne_Orsoy,
  plan.mit = Planzustand_mit_Orsoy_fixCL,
  hwe.list = hwe,
  param = 'discharge',
  id.names = pegel_name,
  mID = pegel_ID,
  sobek.project = so_prj
)
orsoy_fixCL_W <- get_summary_tbl(
  name = 'Orsoy (feste Schwellenshöhe)',
  bezug = Bezugszustand,
  plan.ohne = Planzustand_ohne_Orsoy,
  plan.mit = Planzustand_mit_Orsoy_fixCL,
  hwe.list = hwe,
  param = 'waterlevel',
  id.names = pegel_name,
  mID = pegel_ID,
  sobek.project = so_prj
)
#+++mod CL-----
orsoy_modCL_Q <- get_summary_tbl(
  name = 'Orsoy (passende Schwellenshöhe)',
  bezug = Bezugszustand,
  plan.ohne = Planzustand_ohne_Orsoy,
  plan.mit = Planzustand_mit_Orsoy_passCL,
  hwe.list = hwe,
  param = 'discharge',
  id.names = pegel_name,
  mID = pegel_ID,
  sobek.project = so_prj
)
orsoy_modCL_W <- get_summary_tbl(
  name = 'Orsoy (passende Schwellenshöhe)',
  bezug = Bezugszustand,
  plan.ohne = Planzustand_ohne_Orsoy,
  plan.mit = Planzustand_mit_Orsoy_passCL,
  hwe.list = hwe,
  param = 'waterlevel',
  id.names = pegel_name,
  mID = pegel_ID,
  sobek.project = so_prj
)
plot_longprofile(
  river = 'Rhein',
  from.km = 780,
  to.km = 860,
  case.list = c(
    "Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy",
    "Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit"
  ),
  compare.by = 'notiz',
  color.by = 'notiz',
  lt.by = 'notiz',
  param = 'discharge',
  delta = TRUE,
  y2.scale = 10,
  sobek.project = so_prj,
  master.tbl = rhein_tbl
)
#----- Lohrwardt Table----
# cases Planzustände ohne Lohrwardt
# Lohrwardt TMW
# case ohne Lohrwardt
ohne_Lohrwardt <- c(
  'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt')

qt <- his_from_case(
  ohne_Lohrwardt,  so_prj, param = 'discharge', mID = 'p_lohrwardt_out'
  )
# sobek_sim('Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt_3', so_prj, so_dir)
polder_tmv(qt[case == ohne_Lohrwardt[1]], volume = 18.6)
lohrwardt_w_88s <- plot_polder_scenario(
  name = 'Lohrwardt',
  case.list = c(
    'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt_3',
    'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469'
  ),
  case.desc = c(
    'Mit Lohrwardt_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
    'Ohne Lohrwardt_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469'
  ),
  # ref.mID = 'p_duesseldorf',
  q.in = TRUE,
  q.out = TRUE,
  cmp.sort = TRUE,
  delta.line = TRUE,
  peak.nday = 3,
  # w.canal = TRUE,
  # y2.scale = 1,
  y2.tick1 = -50,
  master.tbl = rhein_tbl,
  sobek.project = so_prj,
  delta.pegel = TRUE,
  date.break = '2 hours',
  date.label = "%H",
  param = 'waterlevel'
)
lohrwardt_w_88s
polder_tmv(qt[case == ohne_Lohrwardt[2]], volume = 19.27)
plot_multi_lines(
  ohne_Lohrwardt[3], sobek.project = so_prj, param = 'waterlevel',
  mID = c('p_lohrwardt_in', 'p_lohrwardt_out', 'p_lohrwardt_mitte')
)
plot_multi_lines(
  ohne_Lohrwardt[3], sobek.project = so_prj, param = 'crest level',
  sID = c('lohrwardt_zu_1', 'lohrwardt_zu_2')
)
qplot(st$lohrwardt_zu_2)
lohrwardt_w_95s <- plot_polder_scenario(
  name = 'Lohrwardt',
  case.list = c(
    'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
    'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy_CL2469'
  ),
  case.desc = c(
    'Mit Lohrwardt_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
    'Ohne Lohrwardt_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy_CL2469'
  ),
  # ref.mID = 'p_duesseldorf',
  q.in = TRUE,
  q.out = TRUE,
  cmp.sort = TRUE,
  delta.line = TRUE,
  peak.nday = 2,
  # y2.scale = 0.005,
  y2.tick1 = -50,
  master.tbl = rhein_tbl,
  sobek.project = so_prj,
  delta.pegel = TRUE,
  date.break = '2 hours',
  date.label = "%H",
  param = 'waterlevel'
)
lohrwardt_w_95s
lohrwardt_w_03s <- plot_polder_scenario(
  name = 'Lohrwardt',
  case.list = c(
    'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
    'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469'
  ),
  case.desc = c(
    'Mit Lohrwardt_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
    'Ohne Lohrwardt_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469'
  ),
  # ref.mID = 'p_duesseldorf',
# plot.title = 'Wasserstand Ganglinien für Maßnahme Lohrwardt. Modellhochwasser 2003 Mittel',
  q.in = TRUE,
  q.out = TRUE,
  cmp.sort = TRUE,
  delta.line = TRUE,
  peak.nday = 2,
  # y2.scale = 0.02,
  # y2.tick1 = 0,
  # date.break = '1 day',
  master.tbl = rhein_tbl,
  sobek.project = so_prj,
  delta.pegel = TRUE,
date.break = '2 hours',
date.label = "%H",
  param = 'discharge'
)
lohrwardt_w_03s

Planzustand_ohne_Lohrwardt <- Planzustand_mit_Orsoy_fixCL
Planzustand_mit_Lohrwardt <- c(
  'Planzustand_ZPK_HW1988_Mittel_Nur_Eich_Orsoy_CL2469_Lohrwardt',
  'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
  'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt',
  'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
  'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt',
  'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt'
)
lohr_tbl_Q <- get_summary_tbl(
  name = 'Lohrwardt',
  bezug = Bezugszustand,
  plan.ohne = Planzustand_ohne_Lohrwardt,
  plan.mit = Planzustand_mit_Lohrwardt,
  hwe.list = hwe,
  param = 'discharge',
  id.names = pegel_name,
  mID = pegel_ID,
  sobek.project = so_prj
)
lohr_tbl_W <- get_summary_tbl(
  name = 'Lohrwardt',
  bezug = Bezugszustand,
  plan.ohne = Planzustand_ohne_Lohrwardt,
  plan.mit = Planzustand_mit_Lohrwardt,
  hwe.list = hwe,
  param = 'waterlevel',
  id.names = pegel_name,
  mID = pegel_ID,
  sobek.project = so_prj
)

#-Mündelheim----
Planzustand_mit_Mündelheim <- Planzustand_ohne_Orsoy
Planzustand_ohne_Mündelheim <- Planzustand_mit_Worringer

#-----long long long profiles----
plot_drv(
  name = 'Muendelheim',
  to.upstream = 30,
  to.downstream = 10,
  case.list = c(
    # 'Bezugszustand_ZPK_HW1988_Mittel_1563_newReg',
    'Bezugszustand_ZPK_HW1988_Selten_1828_newReg',
    # 'Bezugszustand_ZPK_HW1995_Mittel_1188_newReg',
    # 'Bezugszustand_ZPK_HW1995_Selten_1282_newReg',
    # 'Bezugszustand_ZPK_HW2003_Mittel_1527_newReg',
    # 'Bezugszustand_ZPK_HW2003_Selten_1663_newReg',
    # 'Planzustand_ZPK_HW1988_Mittel_Nur_Eich_Orsoy_CL2469_Lohrwardt',
    'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt'
    # 'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt',
    # 'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt'
    # 'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt',
    # 'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt'
  ),
  case.desc = c(
    # 'Bezugszustand_ZPK_HW1988_Mittel_1563_newReg',
    '2. Bezugszustand_ZPK_HW1988_Selten_HW1988_Selten',
    # 'Bezugszustand_ZPK_HW1995_Mittel_1188_newReg',
    # 'Bezugszustand_ZPK_HW1995_Selten_HW1995_Selten',
    # 'Bezugszustand_ZPK_HW2003_Mittel_1527_newReg',
    # 'Bezugszustand_ZPK_HW2003_Selten_1663_newReg',
    # 'Planzustand_ZPK_HW1988_Mittel_Nur_Eich_Orsoy_CL2469_Lohrwardt',
    '1. Planzustand_ZPK_HW1988_Selten_HW1988_Selten'
    # 'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt',
    # 'Planzustand_ZPK_HW1995_Selten_HW1995_Selten'
    # 'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt',
    # 'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt'
  ),
  param = 'waterlevel',
  sobek.project = 'c:/rhein.lit',
  compare.by = 'zustand',
  cmp.sort = TRUE,
  delta = TRUE,
  a.fill = 'red',
  a.alpha = 0.5,
  # y2.scale = 10,
  # y2.tick1 = -500,
  facet.by = NULL,
  # plot.title = 'Längschnitt Ablfuss entlang Rhein von KM 443,50 bis KM 866,00. HW 1988 Selten. Zielpegel Köln',
  # group.by = 'hwe',
  # overlap = c('Selz','Wied' ,'Langel', 'RUHR', 'Lippe', 'REES'),
  master.tbl = rhein_tbl
)

rhein_w <- plot_longprofile(
  river = 'Rhein',
  # from.km = 510,
  # to.km = 600,
  case.list = c(
    # 'Bezugszustand_ZPK_HW1988_Mittel_1563_newReg',
    'Bezugszustand_ZPK_HW1988_Selten_1828_newReg',
    # 'Bezugszustand_ZPK_HW1995_Mittel_1188_newReg',
    # 'Bezugszustand_ZPK_HW1995_Selten_1282_newReg',
    # 'Bezugszustand_ZPK_HW2003_Mittel_1527_newReg',
    # 'Bezugszustand_ZPK_HW2003_Selten_1663_newReg',
    # 'Planzustand_ZPK_HW1988_Mittel_Nur_Eich_Orsoy_CL2469_Lohrwardt',
    'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt'
    # 'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt',
    # 'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt'
    # 'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt',
    # 'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt'
  ),
  case.desc = c(
    # 'Bezugszustand_ZPK_HW1988_Mittel_1563_newReg',
    '2. Bezugszustand_ZPK_HW1988_Selten_HW1988_Selten',
    # 'Bezugszustand_ZPK_HW1995_Mittel_1188_newReg',
    # 'Bezugszustand_ZPK_HW1995_Selten_HW1995_Selten',
    # 'Bezugszustand_ZPK_HW2003_Mittel_1527_newReg',
    # 'Bezugszustand_ZPK_HW2003_Selten_1663_newReg',
    # 'Planzustand_ZPK_HW1988_Mittel_Nur_Eich_Orsoy_CL2469_Lohrwardt',
    '1. Planzustand_ZPK_HW1988_Selten_HW1988_Selten'
    # 'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt',
    # 'Planzustand_ZPK_HW1995_Selten_HW1995_Selten'
    # 'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt',
    # 'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt'
  ),
  param = 'waterlevel',
  sobek.project = so_prj,
  compare.by = 'zustand',
  cmp.sort = TRUE,
  delta = TRUE,
  y2.scale = 250,
  y2.tick1 = -0.3,
  facet.by = NULL,
  plot.title = 'Längschnitt Wasserstand entlang Rhein von KM 443,50 bis KM 866,00. HW 1988 Selten. Zielpegel Köln',
  # group.by = 'hwe',
  overlap = c('Selz','Wied' ,'Langel', 'RUHR', 'Lippe', 'REES'),
  master.tbl = rhein_tbl
)
rhein_w
rhein_plot <- list(rhein_w, rhein_q)
rhein_plot_names <- c('rhein_lang_w.png', 'rhein_lang_q.png')
map2(
  rhein_plot_names, rhein_plot, ggsave,
  dpi = 600,
  device = 'png', width = 42, height = 27, units = 'cm'
)

ggsave('lohrwardt_03m.png', lohrwardt_w_03m,
       dpi = 600,
       device = 'png', width = 27, height = 20, units = 'cm'
       )

qt <- his_from_case(
  c('Bezugszustand_ZPK_HW2003_Selten_1663_newReg',
  'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt'),
  sobek.project = 'c:/rhein.lit',
  param = 'discharge',
  mID = c('p_mosel_muendung', 'P_Mainz', 'p_andernach')
  )
findpeaks(qt[case == 'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
             p_mosel_muendung], nups = 10)
ggplot(data = qt, aes(x = ts, y = p_mosel_muendung, color = case)) +
  scale_x_datetime()+
  theme(legend.position = 'bottom')+
  geom_line(size = 1) +
  geom_line(aes(y = P_Mainz), size = 1)
  # geom_vline(xintercept = qt$ts[230]) +
  # geom_vline(xintercept = qt$ts[461])+
  # geom_vline(xintercept = qt$ts[555])
