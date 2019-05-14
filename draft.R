library(data.table)
library(testthat)
library(cowplot)
library(sobekio)
library(tidyverse)
so_prj <- 'd:/rhein.lit'
#----default parameters-----


sobek.project = so_prj
master.tbl = rhein_tbl
param = 'waterlevel'
q.in = TRUE
q.out = TRUE
w.canal = FALSE
ref.mID = 'p_koeln'
y2.scale = NULL
h.lines = NULL
peak.nday = NULL
peak.pegel = FALSE
delta.pegel = FALSE
delta.measure = TRUE
delta.line = FALSE
rel.heights = c(2, 0.7)
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
#----setting parameters----
  river = 'Rhein'
  from.km = 665
  to.km = 780
  case.list = c(
    'Planzustand_ZPK_HW1995_Selten_ohne_Niederrhein',
    'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit'
  )
  case.desc = c(
    'ohne Worringer_ZPK_HW1995_Selten_Nur_Eich',
    'mit Worringer_ZPK_HW1995_Selten_Eich_Wor'
  )
  param = 'waterlevel'
  delta = TRUE
  y2.scale = 50
  sobek.project = so_prj
  master.tbl = rhein_tbl
#---test full function----
plot_longprofile(
  river = 'Main',
  from.km = 0,
  to.km = 25,
  case.list = c('Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit',
                'Bezugszustand_ZPK_HW1988_Selten_1828_newReg'),
  # case.desc = case.list,
  sobek.project = 'c:/rhein.lit',
  param = 'waterlevel',
  group.by = 'vgf',
  delta = TRUE,
  y2.scale = 20,
  # sobek.project = so_prj,
  master.tbl = rhein_tbl
)

  plot_drv(
    name = 'Hattersheim',
    case.list = c('Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit',
                  'Bezugszustand_ZPK_HW1988_Selten_1828_newReg'),
    # case.desc = case.list,
    to.upstream = 10,
    to.downstream = 20,
    param = 'waterlevel',
    compare.by = 'zustand',
    group.by = 'vgf',
    delta = TRUE,
    y2.scale = 20,
    sobek.project = 'c:/rhein.lit',
    master.tbl = rhein_tbl
  )

  plot_polder_scenario(
    name = 'Lohrwardt',
    case.list = c('Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
                  'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469'),
    case.desc = c(
      'mit Lohrwardt_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
      'ohne Lohrwardt_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469'),
    ref.mID = 'p_rees',
    q.in = TRUE,
    q.out = TRUE,
    # compare.by = 'notiz',
    # facet.by = 'zustand',
    delta.line = TRUE,
    param = 'discharge',
    # y2.scale = 0.05,
    # y2.tick1 = -100,
    delta.measure = TRUE,
    delta.pegel = TRUE,
    peak.nday = 3,
    # date.break = '6 hours',
    # date.label = '%d.%m %H:%M',
    sobek.project = 'D:/rheinNRW.lit',
    master.tbl = rhein_tbl
  )
  plot_polder_scenario(
    name = 'Orsoy',
    case.list = c(
      'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469_Lohrwardt',
      'Bezugszustand_ZPK_HW1988_Selten_1828_newReg'),
    case.desc = c(
      'ohne Worringer_ZPK_HW1988_Selten_Nur_Eich',
      'mit Worringer_ZPK_HW1988_Selten_Eich_Wor'),
    ref.mID = 'p_wesel',
    # q.in = TRUE,
    # q.out = TRUE,
    # compare.by = 'notiz',
    # facet.by = 'zustand',
    delta.line = TRUE,
    param = 'waterlevel',
    # y2.scale = 20,
    delta.measure = TRUE,
    delta.pegel = TRUE,
    peak.nday = 2,
    date.break = '2 hours',
    date.label = '%H',
    sobek.project = so_prj,
    master.tbl = rhein_tbl
  )
  pz27_ohne_orsoy <-
    c(
      'Planzustand_ZPK_HW1988_Mittel_Nur_Eich',
      'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit',
      'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein',
      'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit',
      'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein',
      'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit'
    )
  pz27_mit_orsoy <-
    c(
      'Planzustand_ZPK_HW1988_Mittel_Nur_Eich_Orsoy',
      'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy',
      'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein_Orsoy',
      'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy',
      'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein_Orsoy',
      'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit_Orsoy'
    )
  bz18_zpk <-
    c(
      'Bezugszustand_ZPK_HW1988_Mittel_1563_newReg',
      'Bezugszustand_ZPK_HW1988_Selten_1828_newReg',
      'Bezugszustand_ZPK_HW1995_Mittel_1188_newReg',
      'Bezugszustand_ZPK_HW1995_Selten_1282_newReg',
      'Bezugszustand_ZPK_HW2003_Mittel_1527_newReg',
      'Bezugszustand_ZPK_HW2003_Selten_1663_newReg'
    )
  #----get WL ohne orsoy----
  wt_orsoy_max <- his_from_case(
    case.list = pz27_ohne_orsoy,
    so_prj,
    mID = 'p_orsoy_in',
    param = 'waterlevel',
    get.max = TRUE
  )

  qt <- his_from_case(
    case.list = pz27_ohne_orsoy,
    so_prj,
    mID = 'p_wesel',
    param = 'discharge'
  )
  qt[, max(p_wesel), by = case]


  #----caculate WL reduction at Orsoy_IN PZ vs BZ----
  wl_orsoy <- get_polder_max(
    name = 'Orsoy',
    case.list = c(pz27_ohne_orsoy, bz18_zpk),
    sobek.project = so_prj,
    master.tbl = rhein_tbl,
    ref.mID = 'p_wesel',
    compare.by = 'zustand',
    group.by = 'vgf',
    param = 'waterlevel'
  )
  clist <- c(
    'Planzustand_ZPK_HW1988_Mittel_Nur_Eich',
    'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit',
    'Planzustand_ZPK_HW1995_Mittel_ohne_Niederrhein',
    'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit',
    'Planzustand_ZPK_HW2003_Mittel_ohne_Niederrhein',
    'Planzustand_ZPK_HW2003_Selten_Eich_Wor_Zeit',
    'Bezugszustand_ZPK_HW1988_Mittel_1563_newReg',
    'Bezugszustand_ZPK_HW1988_Selten_1828_newReg',
    'Bezugszustand_ZPK_HW1995_Mittel_1188_newReg',
    'Bezugszustand_ZPK_HW1995_Selten_1282_newReg',
    'Bezugszustand_ZPK_HW2003_Mittel_1527_newReg',
    'Bezugszustand_ZPK_HW2003_Selten_1663_newReg'
  )

  cdesc <- c(
    'Planzustand_ZPK_HW1988_Mittel_HW1988_Mittel',
    'Planzustand_ZPK_HW1988_Selten_HW1988_Selten',
    'Planzustand_ZPK_HW1995_Mittel_HW1995_Mittel',
    'Planzustand_ZPK_HW1995_Selten_HW1995_Selten',
    'Planzustand_ZPK_HW2003_Mittel_HW2003_Mittel',
    'Planzustand_ZPK_HW2003_Selten_HW2003_Selten',
    'Bezugszustand_ZPK_HW1988_Mittel_HW1988_Mittel',
    'Bezugszustand_ZPK_HW1988_Selten_HW1988_Selten',
    'Bezugszustand_ZPK_HW1995_Mittel_HW1995_Mittel',
    'Bezugszustand_ZPK_HW1995_Selten_HW1995_Selten',
    'Bezugszustand_ZPK_HW2003_Mittel_HW2003_Mittel',
    'Bezugszustand_ZPK_HW2003_Selten_HW2003_Selten'
  )
  river = 'Rhein'
  from.km = 798
  to.km = 805
  case.list = clist
  case.desc = cdesc
  sobek.project = so_prj
  param = 'discharge'
  lt.by = 'zustand'
  color.by = 'vgf'
  facet.by = 'hwe'
  compare.by = 'zustand'
  group.by = 'notiz'
  color.name = 'Farbe'
  lt.name = 'Linienart'
  delta = TRUE
  reserve.x = FALSE
  x.lab = 'Lage (KM)'
  y.lab = ifelse(param == 'discharge',
                 'Abfluss (m³/s)', 'Wasserstand (m+NHN)')
  y2.scale = 10
  plot.title = NULL
  text.size = 12
  text.x.top.angle = 90L
  text.x.top.size = 8L
  text.x.bottom.angle = 0L
  ntick.x = 10L
  highlight = NULL
  highlight.text = NULL
  a.fill = exl_std[3]
  a.alpha = 0.1
  master.tbl = rhein_tbl
  verbose = TRUE
  plot_longprofile(
    river = 'Rhein',
    from.km = 798,
    to.km = 805,
    facet.by = 'hwe',
    # y2.scale = 5,
    compare.by = 'zustand',
    group.by = 'notiz',
    case.list = clist,
    case.desc = cdesc,
    sobek.project = so_prj,
    master.tbl = rhein_tbl,
    delta = TRUE,
    param = 'waterlevel'
  )
  plot_polder_scenario(
    c(

    )
  )
