library(sobekio)
library(data.table)

so_prj <- "/media/quang/4AD88F9FD88F8841/So21503/rhein.lit"
clist <- c('Planzustand_ZPK_HW1988_Mittel_Nur_Eich',
'Planzustand_ZPK_HW1988_Selten_Nur_Eich',
'Planzustand_ZPK_HW2003_Selten_Nur_Eich',
'Planzustand_ZPK_HW1988_Mittel_Nur_Eich_mit_Nahe',
# 'Planzustand_ZPK_HW1988_Selten_Nur_Eich_mit_Nahe',
'Planzustand_ZPK_HW2003_Selten_Nur_Eich_mit_Nahe')

rhein <- get_segment_data(
  case.list = clist,
  from.km = 10,
  to.km = 460,
  param = "discharge",
  sobek.project = so_prj,
  do.par = TRUE,
  master.tbl = rhein_tbl
)
hfile <- "/media/quang/4AD88F9FD88F8841/So21503/rhein.lit/442/REACHSEG.HIS"
tloc <- his_location(hfile)
his.file <- hfile
plot_polder(
  'Langel',
  case.list = c(
    'Planzustand_ZPK_HW1988_Selten_Nur_Eich',
    'Planzustand_ZPK_HW2003_Selten_Nur_Eich'
  ),
  sobek.project = so_prj,
  master.tbl = rhein_tbl,
  param = 'discharge',
  q.in = TRUE
)

qt <- his_from_case('Planzustand_ZPK_HW1988_Selten_Nur_Eich', so_prj, 
                    mID = pegel_ID, param = 'discharge')

vc_peaks <- volume_center(qt, value = FALSE)
all_cols <- colnames(qt)
all_cols <- toupper(all_cols[-12])

dta <- data.table(ts = qt[vc_peaks, ts], Pegel = haven::as_factor(all_cols))
g <- ggplot(data = dta, 
            aes(x = Pegel, y = ts)) + 
  scale_y_datetime(date_breaks = '2 hours') +
  geom_point()
g

qt2 <- get_segment_data(river = 'Rhein',
                        master.tbl = rhein_tbl,
                        from.km = -Inf,
                        to.km = Inf,
                        get.max = FALSE,
                        sobek.project = so_prj,
                        case.list = 'Planzustand_ZPK_HW1988_Selten_Nur_Eich',
                        param = 'discharge')
qt3 <- qt2[, 1:100]
system.time(vc2 <- volume_center(qt2))
system.time(vc3 <- volume_center(qt2))
system.time(vc4 <- volume_center(qt2, do.par = FALSE))
expect_equal(vc3, vc4)

