#' Get list of IDs
library(sobekio)
library(data.table)
cases <- c('Bezug18_Maßn_Bret_Spon_Gunter_QP_REC_Orsoy_Wor_Lohr_Muen')
soprj <- 'd:/so21302/rhein29a.lit'
so_folder <- 'd:/so21302'

bnd.dat <- get_file_path(cases[1], soprj, 'measstation')
bnd <- fread(bnd.dat, header = F, sep = "\n")
id_tbl <- bnd[grep("id '.*'", V1)] # get table of IDs

