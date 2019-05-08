library(data.table)
library(testthat)
library(cowplot)
library(sobekio)
library(tidyverse)
#----default parameters-----
river = NULL
from.km = -Inf
to.km = Inf
case.list = NULL
case.desc = case.list
sobek.project = NULL
param = 'discharge'
lt.by = 'zustand'
color.by = 'vgf'
facet.by = NULL
compare.by = 'zustand'
group.by = compare.by
color.name = 'Farbe'
lt.name = 'Linienart'
delta = FALSE
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
master.tbl = NULL
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
  river = 'Rhein',
  from.km = 665,
  to.km = 780,
  case.list = c(
    'Planzustand_ZPK_HW1995_Selten_ohne_Niederrhein',
    'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit'
  ),
  case.desc = c(
    'ohne Worringer_ZPK_HW1995_Selten_Nur_Eich',
    'mit Worringer_ZPK_HW1995_Selten_Eich_Wor'
  ),
  param = 'waterlevel',
  group.by = 'vgf',
  delta = TRUE,
  y2.scale = 100,
  sobek.project = so_prj,
  master.tbl = rhein_tbl
)

  plot_drv(
    name = 'Muendelheim',
    case.list = c(
      'Planzustand_ZPK_HW1995_Selten_ohne_Niederrhein',
      'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit'
    ),
    case.desc = c(
      'ohne Worringer_ZPK_HW1995_Selten_Nur_Eich',
      'mit Worringer_ZPK_HW1995_Selten_Eich_Wor'
    ),
    param = 'waterlevel',
    compare.by = 'zustand',
    group.by = 'vgf',
    delta = TRUE,
    y2.scale = 10,
    sobek.project = so_prj,
    master.tbl = rhein_tbl
  )

  plot_polder_scenario(
    name = 'Worringer',
    case.list = c(
      'Planzustand_ZPK_HW1988_Selten_ohne_Niederrhein',
      'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit'),
    case.desc = c(
      'ohne Worringer_ZPK_HW1988_Selten_Nur_Eich',
      'mit Worringer_ZPK_HW1988_Selten_Eich_Wor'),
    # ref.mID = 'p_duesseldorf',
    q.in = TRUE,
    q.out = TRUE,
    # compare.by = 'notiz',
    # facet.by = 'zustand',
    delta.line = TRUE,
    param = 'discharge',
    y2.scale = 10,
    delta.measure = TRUE,
    delta.pegel = TRUE,
    peak.nday = 5,
    # date.break = '6 hours',
    # date.label = '%d.%m %H:%M',
    sobek.project = so_prj,
    master.tbl = rhein_tbl
  )
  plot_polder_scenario(
    name = 'Orsoy',
    case.list = c(
      'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit',
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
