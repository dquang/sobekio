# case.name = 'Planzustand_ZPK_HW1988_Selten_EreigOpt_fric29A_2'
# sobek.project = so_prj
# fric.dat <- get_file_path(case.name, sobek.project, 'friction.dat')
# 
# .get_fric_dat <- function(fric.dat) {
#   fric_tbl <- fread(file = fric.dat, sep = '\n', header = FALSE, quote = FALSE)
#   fric_tbl[, orig_id := .I]
#   fric_tbl[grepl('GLFR', V1)]
#   fric_global <- fric_tbl[grepl('GLFR', V1), V1]
#   fric_tbl <- fric_tbl[!grepl('GLFR', V1)]
#   fric_id_tbl <- fric_tbl[grepl('^BDFR|^CRFR|^STFR|^XRST|^D2FR|^WNDS', V1)]
# }