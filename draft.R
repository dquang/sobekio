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
