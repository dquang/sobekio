#----HW 1988 S----
case.list = 'Planzustand_ZPK_HW1988_Selten_Eich_Wor_Zeit_Orsoy_CL2469'
wt <- his_from_case(case.list = case.list,
                    sobek.project = so_prj,
                    param = 'waterlevel',
                    wID = c('lohrwardt_in_1', 'lohrwardt_in_2')
                    )
qt <- his_from_case(
  case.list = case.list,
  sobek.project = so_prj,
  param = 'discharge',
  mID = 'p_lohrwardt_out'
)
wt <- merge(wt, qt, by = c('ts', 'case'))
z_bottom = 15.5
cl_1 = 18.39
cl_2 = 17.37
cl_0 = 25
p_area = 350
v_tmv <- 19.39
# this method bypass drawned factor Sf
wt[, c("Sf", 'Cw', 'B', 'const') := list(1, 1, 16, 2*sqrt(2*9.81/3)/3)]
wt[, g_open_1 := 0]
wt[, g_open_2 := 0]
wt[, row_id := .I]
t_peak <- wt[lohrwardt_in_1 == max(lohrwardt_in_1), ts]
i_peak <- which(wt$ts == t_peak)[1]
re_curve <- function(x){
  # xx <- x
  y = -1.2433 * x^4 + 1.9618 * x^3 - 1.0288 * x^2 + 0.1291 * x + 1
  if (x <= 0) y <- 1
  if (x >= 1) y <- 0.82
  y
}
i_lag = 100
i_d_max <-  1
d_max <-  0
for (i_lag in seq.int(1, 30, by = 1)){
  for (i in seq.int(i_peak - 72, i_peak, 1)){
    # init condtions
    wt[row_id < i, g_open_2 := 0]
    wt[row_id >= i, g_open_2 := 1]
    wt[row_id < i + i_lag, g_open_1 := 0]
    wt[row_id >= i + i_lag, g_open_1 := 1]
    wt[, Q_1 := 0]
    wt[, Q_2 := 0]
    wt[, Volume := 0]
    wt[, Vol_cs := 0]
    wt[, Sf1 := 1]
    wt[, Sf2 := 1]
    wt[, rH_1 := 1]
    wt[, rH_2 := 1]
    # calculating discharge with drawned factor = 1
    wt[lohrwardt_in_1 >= cl_1, Q_1 := g_open_1*Sf1*Cw*B*const*(lohrwardt_in_1 - cl_1)^1.5]
    wt[lohrwardt_in_2 >= cl_2, Q_2 := g_open_2*Sf2*Cw*B*const*(lohrwardt_in_2 - cl_2)^1.5]
    wt[, Q := Q_1 + Q_2]
    wt[, Volume := Q * 3600 / 10^6]
    wt[, Vol_cs := cumsum(Volume)]
    i_full <- wt[Vol_cs >= v_tmv, min(row_id) - 1]
    v_max <- wt[i_full, Vol_cs]
    wt[row_id > i_full, c('Q', 'Vol_cs') := list(0, v_max)]
    # recalculate discharge with drawned factor = f(H2, H1)
    wt[, H2 := Vol_cs * 10^2 / p_area + z_bottom]
    wt[, rH_1 := H2/lohrwardt_in_1]
    wt[, rH_2 := H2/lohrwardt_in_2]
    wt$Sf1 <- sapply(wt$rH_1, re_curve)
    wt$Sf2 <- sapply(wt$rH_2, re_curve)
    wt[lohrwardt_in_1 >= cl_1, Q_1 := g_open_1*Sf1*Cw*B*const*(lohrwardt_in_1 - cl_1)^1.5]
    wt[lohrwardt_in_2 >= cl_2, Q_2 := g_open_2*Sf2*Cw*B*const*(lohrwardt_in_2 - cl_2)^1.5]
    wt[, Q := Q_1 + Q_2]
    # wt[H2 >= lohrwardt_in_1 & H2 >= cl_1,
    #    Q_1 := - g_open_1*Sf1*Cw*B*const*(H2 - cl_1)^1.5]
    wt[, Volume := Q * 3600 / 10^6]
    wt[, Vol_cs := cumsum(Volume)]
    i_full <- wt[Vol_cs >= v_tmv, min(row_id) - 1]
    v_max <- wt[i_full, Vol_cs]
    wt[row_id > i_full,
       Q_2 := - Q_1]
    wt[, Q := Q_1 + Q_2]
    wt[, Q_pz := p_lohrwardt_out - Q]
    d_peak <- max(wt$Q_pz) - max(wt$p_lohrwardt_out)
    if (d_peak < d_max){
      d_max <- d_peak
      i_d_max <- i
      i_lag_soll <- i_lag
    }
  }
}

i <- i_choose <- i_d_max
wt[i_choose, ts]
wt[i_choose + i_lag_soll, ts]
wt[i_full, ts]
wt[, max(Q_1)]
wt[, max(Q_2)]

qt2 <- wt[, c('ts', 'Q_pz', 'p_lohrwardt_out')] %>%
  melt(id.vars = c('ts'))
g <- ggplot(data = qt2, aes(x = ts, y = value,
                                color = variable )) +
  scale_x_datetime() + theme(legend.position = 'bottom') +
  geom_line(size = 1)
g

#----HW 1995 S----
case.list = 'Planzustand_ZPK_HW1995_Selten_Eich_Wor_Zeit_Orsoy_CL2469'
wt <- his_from_case(case.list = case.list,
                    sobek.project = so_prj,
                    param = 'waterlevel',
                    wID = c('lohrwardt_in_1', 'lohrwardt_in_2')
)
qt <- his_from_case(
  case.list = case.list,
  sobek.project = so_prj,
  param = 'discharge',
  mID = 'p_lohrwardt_out'
)
wt <- merge(wt, qt, by = c('ts', 'case'))
z_bottom = 15.5
cl_1 = 18.39
cl_2 = 17.37
cl_0 = 25
p_area = 350
v_tmv <- 18.75
# this method bypass drawned factor Sf
wt[, c("Sf", 'Cw', 'B', 'const') := list(1, 1, 16, 2*sqrt(2*9.81/3)/3)]
wt[, g_open_1 := 0]
wt[, g_open_2 := 0]
wt[, row_id := .I]
t_peak <- wt[lohrwardt_in_1 == max(lohrwardt_in_1), ts]
i_peak <- which(wt$ts == t_peak)[1]
re_curve <- function(x){
  # xx <- x
  y = -1.2433 * x^4 + 1.9618 * x^3 - 1.0288 * x^2 + 0.1291 * x + 1
  if (x <= 0) y <- 1
  if (x >= 1) y <- 0.82
  y
}
cl_t0 = 25 # Crest level at the time of openning
o_time = 20 # Openning duration
dv_dt_1 = round((cl_t0 - cl_1)/3600/o_time, 5)
dv_dt_2 = round((cl_t0 - cl_2)/3600/o_time, 5)
i_d_max <-  1
d_max <-  0
wt[, CL1 := 25]
for (i in seq.int(i_peak - 48, i_peak, 1)) {
  for (i_duration in seq.int(1, 24, by = 1)) {
    dv_dt_1 = round((cl_t0 - cl_1)/3600/i_duration, 5)
    dv_dt_2 = round((cl_t0 - cl_2)/3600/i_duration, 5)
    # caculate Crest level for each starting time
    wt[row_id <= i, CL1 := 25]
    wt[row_id <= i, CL1 := 25]
    wt[row_id <= i + i_duration]
    # init condtions
    wt[row_id < i, g_open_2 := 0]
    wt[row_id >= i, g_open_2 := 1]
    wt[row_id < i + i_lag, g_open_1 := 0]
    wt[row_id >= i + i_lag, g_open_1 := 1]
    wt[, Q_1 := 0]
    wt[, Q_2 := 0]
    wt[, Volume := 0]
    wt[, Vol_cs := 0]
    wt[, Sf1 := 1]
    wt[, Sf2 := 1]
    wt[, rH_1 := 1]
    wt[, rH_2 := 1]
    # calculating discharge with drawned factor = 1
    wt[lohrwardt_in_1 >= cl_1, Q_1 := g_open_1 * Sf1 * Cw * B * const *
         (lohrwardt_in_1 - cl_1) ^ 1.5]
    wt[lohrwardt_in_2 >= cl_2, Q_2 := g_open_2 * Sf2 * Cw * B * const *
         (lohrwardt_in_2 - cl_2) ^ 1.5]
    wt[, Q := Q_1 + Q_2]
    wt[, Volume := Q * 3600 / 10 ^ 6]
    wt[, Vol_cs := cumsum(Volume)]
    i_full <- wt[Vol_cs >= v_tmv, min(row_id) - 1]
    v_max <- wt[i_full, Vol_cs]
    wt[row_id > i_full, c('Q', 'Vol_cs') := list(0, v_max)]
    # recalculate discharge with drawned factor = f(H2, H1)
    wt[, H2 := Vol_cs * 10 ^ 2 / p_area + z_bottom]
    wt[, rH_1 := H2 / lohrwardt_in_1]
    wt[, rH_2 := H2 / lohrwardt_in_2]
    wt$Sf1 <- sapply(wt$rH_1, re_curve)
    wt$Sf2 <- sapply(wt$rH_2, re_curve)
    wt[lohrwardt_in_1 >= cl_1, Q_1 := g_open_1 * Sf1 * Cw * B * const *
         (lohrwardt_in_1 - cl_1) ^ 1.5]
    wt[lohrwardt_in_2 >= cl_2, Q_2 := g_open_2 * Sf2 * Cw * B * const *
         (lohrwardt_in_2 - cl_2) ^ 1.5]
    wt[, Q := Q_1 + Q_2]
    # wt[H2 >= lohrwardt_in_1 & H2 >= cl_1,
    #    Q_1 := - g_open_1*Sf1*Cw*B*const*(H2 - cl_1)^1.5]
    wt[, Volume := Q * 3600 / 10 ^ 6]
    wt[, Vol_cs := cumsum(Volume)]
    i_full <- wt[Vol_cs >= v_tmv, min(row_id) - 1]
    v_max <- wt[i_full, Vol_cs]
    wt[row_id > i_full,
       Q_2 := -Q_1]
    wt[, Q := Q_1 + Q_2]
    wt[, Q_pz := p_lohrwardt_out - Q]
    d_peak <- max(wt$Q_pz) - max(wt$p_lohrwardt_out)
    if (d_peak < d_max) {
      d_max <- d_peak
      i_d_max <- i
      i_lag_soll <- i_lag
      print(paste(
        'delta max: ',
        d_peak,
        '. i_d_max: ',
        i_d_max,
        '. i_lag_soll: ',
        i_lag_soll
      ))
    }
  }
}

i_lag <- i_lag_soll
i <- i_choose <- i_d_max
wt[i_choose, ts]
wt[i_choose + i_lag_soll, ts]
wt[i_full, ts]
wt[, max(Q_1)]
wt[, max(Q_2)]

qt2 <- wt[, c('ts', 'Q_pz', 'p_lohrwardt_out')] %>%
  melt(id.vars = c('ts'))
g <- ggplot(data = qt2, aes(x = ts, y = value,
                            color = variable )) +
  scale_x_datetime() + theme(legend.position = 'bottom') +
  geom_line(size = 1)
g
