# this script is for getting results from NRW-Maßnahmen
library(sobekio)
library(data.table)
library(tidyverse)
library(cowplot)
library(htmlTable)

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
  from.km = 640,
  to.km = 780,
  case.list = c(
    # case ohne Worringer
    # 'Planzustand_ZPK_HW1988_Mittel_Nur_Eich',
    'Planzustand_ZPK_HW1988_Selten_Nur_Eich',
    # 'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein',
    'Planzustand_ZPK_HW1995_Selten_ohne_Niederrhein',
    # 'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein',
    'Planzustand_ZPK_HW2003_Selten_Nur_Eich',
    # case mit Worringer
    # 'Planzustand_ZPK_HW1988_Mittel_Nur_Eich',
    'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit',
    # 'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein',
    'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit',
    # 'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein',
    'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit'
  ),
  case.desc = c(
    # 'Ohne Worringer_ZPK_HW1988_88 Mittel_Nur_Eich',
    'Ohne Worringer_ZPK_HW1988_88 Selten_Nur_Eich',
    # 'Ohne Worringer_ZPK_HW1995_95 Mittel_ohne_Niederrhein',
    'Ohne Worringer_ZPK_HW1995_95 Selten_ohne_Niederrhein',
    # 'Ohne Worringer_ZPK_HW2003_03 Mittel_ohne_Niederrhein',
    'Ohne Worringer_ZPK_HW2003_03 Selten_Nur_Eich',
    # 'Mit Worringer_ZPK_HW1988_88 Mittel_Nur_Eich',
    'Mit Worringer_ZPK_HW1988_88 Selten_Eich_Wor_Zeit',
    # 'Mit Worringer_ZPK_HW1995_95 Mittel_ohne_Niederrhein',
    'Mit Worringer_ZPK_HW1995_95 Selten_Eich_Wor_Zeit',
    # 'Mit Worringer_ZPK_HW2003_03 Mittel_ohne_Niederrhein',
    'Mit Worringer_ZPK_HW2003_03 Selten_Eich_Wor_Zeit'
  ),
  sobek.project = so_prj,
  compare.by = 'zustand',
  cmp.sort = TRUE,
  y2.tick1 = -300,
  group.by = 'vgf',
  facet.by = 'hwe',
  param = 'discharge',
  delta = TRUE,
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
  plot.title = 'Modellergebnis für Maßnahme Worringer. Hochwasser 1988, Zielpegel Köln. VGF Selten.',
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
system.time(rhein_q <- plot_longprofile(
  river = 'Rhein',
  # from.km = 510,
  # to.km = 600,
  case.list = c(
    # 'Bezugszustand_ZPK_HW1988_Mittel_1563_newReg',
    'Bezugszustand_ZPK_HW1988_Selten_1828_newReg',
    # 'Bezugszustand_ZPK_HW1995_Mittel_1188_newReg',
    'Bezugszustand_ZPK_HW1995_Selten_1282_newReg',
    # 'Bezugszustand_ZPK_HW2003_Mittel_1527_newReg',
    # 'Bezugszustand_ZPK_HW2003_Selten_1663_newReg',
    # 'Planzustand_ZPK_HW1988_Mittel_Nur_Eich_Orsoy_CL2469_Lohrwardt',
    'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
    # 'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt',
    'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt'
    # 'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt',
    # 'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt'
  ),
  case.desc = c(
    # 'Bezugszustand_ZPK_HW1988_Mittel_1563_newReg',
    'Bezugszustand_ZPK_HW1988_Selten_HW1988_Selten',
    # 'Bezugszustand_ZPK_HW1995_Mittel_1188_newReg',
    'Bezugszustand_ZPK_HW1995_Selten_HW1995_Selten',
    # 'Bezugszustand_ZPK_HW2003_Mittel_1527_newReg',
    # 'Bezugszustand_ZPK_HW2003_Selten_1663_newReg',
    # 'Planzustand_ZPK_HW1988_Mittel_Nur_Eich_Orsoy_CL2469_Lohrwardt',
    'Planzustand_ZPK_HW1988_Selten_HW1988_Selten',
    # 'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt',
    'Planzustand_ZPK_HW1995_Selten_HW1995_Selten'
    # 'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein_Orsoy_CL2469_Lohrwardt',
    # 'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt'
  ),
  param = 'discharge',
  sobek.project = so_prj,
  compare.by = 'zustand',
  delta = TRUE,
  facet.by = NULL,
  group.by = 'hwe',
  master.tbl = rhein_tbl
))

sobek_view('Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
           so_prj, 'c:/so21302')
