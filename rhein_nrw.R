# this script is for getting results from NRW-Maßnahmen
library(sobekio)
library(data.table)
library(tidyverse)
library(cowplot)

setwd("Z:/M/M2/work/duong/NHWSP-Rhein/Berichte/modelle/rhein/NRW")
so_prj <- 'd:/rhein.lit'
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
#----results polder Worringer-----
# cases Planzustände ohne Worringer
pz27_cases_ohne_Worringer <- c(
  'Planzustand_ZPK_HW1988_Mittel_Nur_Eich',
  'Planzustand_ZPK_HW1988_Selten_Nur_Eich',
  'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein',
  'Planzustand_ZPK_HW1995_Selten_ohne_Niederrhein',
  'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein',
  'Planzustand_ZPK_HW2003_Selten_Nur_Eich'
)
pz27_cases_ohne_Worringer_desc <- c(
  'ohne_Worringer_HW1988_Mittel',
  'ohne_Worringer_HW1988_Selten',
  'ohne_Worringer_HW1995_Mittel',
  'ohne_Worringer_HW1995_Selten',
  'ohne_Worringer_HW2003_Mittel',
  'ohne_Worringer_HW2003_Selten'
)
# cases Planzustände mit Worringer
pz27_cases_mit_Worringer <- c(
  'Planzustand_ZPK_HW1988_Mittel_Nur_Eich',
  'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit',
  'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein',
  'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit',
  'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein',
  'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit'
  )
pz27_cases_mit_Worringer_desc <- c(
  'mit_Worringer_HW1988_Mittel',
  'mit_Worringer_HW1988_Selten',
  'mit_Worringer_HW1995_Mittel',
  'mit_Worringer_HW1995_Selten',
  'mit_Worringer_HW2003_Mittel',
  'mit_Worringer_HW2003_Selten'
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
    'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit'),
  case.desc = c(
    '1. ohne Worringer_ZPK_HW1988_Selten_Nur_Eich',
    '2. mit Worringer_ZPK_HW1988_Selten_Eich_Wor_Zeit'),
  ref.mID = 'p_duesseldorf',
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
    '1. ohne Worringer_ZPK_HW1995_Selten_ohne_Niederrhein',
    '2. mit Worringer_ZPK_HW1995_Selten_Eich_Wor_Zeit'),
  ref.mID = 'p_duesseldorf',
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
    '1. ohne Worringer_ZPK_HW2003_Selten_Nur_Eich',
    '2. mit Worringer_ZPK_HW2003_Selten_Eich_Wor_Zeit'),
  ref.mID = 'p_duesseldorf',
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
qt_wor <- his_from_case(
  case.list = pz27_cases_ohne_Worringer,
  sobek.project = so_prj,
  param = 'discharge',
  mID = c('p_worms', 'P_Mainz', 'p_kaub', 'p_andernach', 'p_bonn',
          'p_koeln', 'p_duesseldorf', 'p_ruhrort', 'p_wesel', 'p_rees',
          'p_emmerich'),
  get.max = TRUE
)
qt_wor_ohne <- qt_wor_ohne %>% select(-ts) %>%
  melt(id.vars = 'case') %>%
  dcast(variable ~ case)


wor_tbl <- get_delta_table(
  case.wo = pz27_cases_ohne_Worringer,
  case.wo.desc = pz27_cases_ohne_Worringer_desc,
  case.w = pz27_cases_mit_Worringer,
  case.w.desc = pz27_cases_mit_Worringer_desc,
  sobek.project = so_prj,
  param = 'discharge',
  mID =  c('p_worms', 'P_Mainz', 'p_kaub', 'p_andernach', 'p_bonn',
          'p_koeln', 'p_duesseldorf', 'p_ruhrort', 'p_wesel', 'p_rees',
          'p_emmerich')
)

# wor_tbl <- wor_tbl %>% mutate_at(vars(-variable), list(~round(., 2)))
wor_tbl[, variable := toupper(variable)]
cols <- colnames(wor_tbl)[-1]
wor_tbl[, (cols) := round(.SD, 2), .SDcols = cols]
