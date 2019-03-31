# this script is for processing multiple input for a graph
library(sobekio)
library(data.table)
library(tidyverse)

# reading
rhein_tbl <- fread("D:/rhein_table.txt", sep = "\t", dec = ",")
orsoy <- rhein_tbl[grepl('orsoy', besonderheit, ignore.case = TRUE)]
cases <- "BFG_BZ18_Gesamt"
id_type = 'mID'
id_vor <- 'P_Mainz'
args_list <- list(case.list = cases, sobek.project = 'd:/so21302/rhein29a.lit',
              id_type = id_vor)
names(args_list) <- c("case.list", "sobek.project", id_type)
qt <- do.call(his_from_case, args = args_list)
his_from_case(cases, sobek.project = 'd:/so21302/rhein29a.lit',
              reformulate(paste(id_type, "=", "'",id_vor,"'", sep = "")))
eval()

plot_polder <- function(
  name = 'Guntersblum',
  case.name = "Bezugszustand_ZPK_HW1988_Selten_1830_newReg",
  sobek.project = "D:/rheinGesamt.lit",
  delta = TRUE,
  master.tbl = rhein_tbl
){
  # get ID table of the polder
  id_tbl <- rhein_tbl[grepl(name, besonderheit, fixed = TRUE)]
  # get qt for Einlass
  id_in <- id_tbl[grepl('Einlass', besonderheit), ID_FROM]
  id_in_type <- id_tbl[grepl('Einlass', besonderheit), ID_TYPE]
  id_in_args <- list(case.list = case.name, sobek.project = sobek.project,
                       id_in_type = id_in, param = "discharge")
  names(id_in_args)[3] <- id_in_type
  qt_id_in <- do.call(his_from_case, id_in_args)
  # get qt for Auslass
  id_out <- id_tbl[grepl('Auslass', besonderheit), ID_FROM]
  id_out_type <- id_tbl[grepl('Einlass', besonderheit), ID_TYPE]
  id_out_args <- list(case.list = case.name, sobek.project = sobek.project,
                      id_out_type = id_out, param = "discharge")
  names(id_out_args)[3] <- id_out_type
  qt_id_out <- do.call(his_from_case, id_out_args)
  # get Waterlevel
  id_mitte <- id_tbl[grepl('Innen', besonderheit), ID_FROM]
  id_mitte_type <- id_tbl[grepl('Innen', besonderheit), ID_TYPE]
  id_mitte_args <- list(case.list = case.name, sobek.project = sobek.project,
                        id_mitte_type = id_mitte, param = "water level")
  names(id_mitte_args)[3] <- id_mitte_type
  wt_id_mitte <- do.call(his_from_case, id_mitte_args)
  id_data <- merge(qt_id_in, qt_id_out, by = c('ts', 'case'))
  id_data <- merge(id_data, wt_id_mitte, by = c('ts', 'case'))
  id_data_colname <- colnames(id_data) %>%
    str_replace(id_mitte, 'W_innen') %>%
    str_replace(id_in, 'Q_vor') %>%
    str_replace(id_out, 'Q_nach')
  colnames(id_data) <- id_data_colname
  id_data[, Q_diff := Q_vor - Q_nach]
  g <- ggplot(data = id_data,
              mapping = aes(x = ts)) +
    theme_bw() +
    theme(legend.position = 'bottom')+
    scale_x_datetime() +
    geom_line(aes(y = Q_vor, color = 'Q_vor', linetype = 'Q_vor'),
              size = 1) +
    geom_line(aes(y = Q_nach,  color = 'Q_nach', linetype = 'Q_nach'),
              size = 1)
  return(g)
}

plot_polder(name = 'Bodenheim')
