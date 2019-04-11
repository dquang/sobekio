# reading River Weir Information
ct_def_f <- get_file_path('Planzustand_Eich_TEST_fixCL', so_prj, type = 'control.def')
ct_dat_f <- get_file_path('Planzustand_Eich_TEST_fixCL', so_prj, type = 'control.dat')
str_def_f <- get_file_path('Planzustand_Eich_TEST_fixCL', so_prj, type = 'struct.def')
str_dat_f <- get_file_path('Planzustand_Eich_TEST_fixCL', so_prj, type = 'struct.dat')

ct_def <- fread(ct_def_f, sep = "\n", header = FALSE)
str_def <- fread(str_def_f , sep = "\n", header = FALSE)
str_tbl <- fread(str_dat_f , sep = "\n", header = FALSE)
ct_def[grepl('CNTL id', V1), CNTL := .N]
ct_tbl <- ct_def[grepl('CNTL id .* cntl', V1)|grepl('CNTL id .* pdin', V1)]
ct_tbl[, id := str_match(V1, "CNTL id '([^']*)'")[,2]]
ct_tbl[, name := str_match(V1, "CNTL id .* nm '([^']*)'")[,2]]

#----reading structure-----
str_mt <- str_match(
  str_tbl$V1,
  "STRU id '([^']*)' nm '([^']*)' dd '([^']*)'.*stru")
str_mt2 <- str_match(
  str_tbl$V1,
  "STRU id.*ca (\\d \\d \\d \\d) cj '([^']*)' '([^']*)' '([^']*)' '([^']*)'.* stru")
str_tbl$id <- str_mt[, 2]
str_tbl$name <- str_mt[, 3]
str_tbl$def <- str_mt[, 4]
str_tbl$ca <- str_mt2[, 2]
str_tbl$cj1 <- str_mt2[, 3]
str_tbl$cj2 <- str_mt2[, 4]
str_tbl$cj3 <- str_mt2[, 5]
str_tbl$cj4 <- str_mt2[, 6]
#----reading struc.def----
# cl (.*) cw (.*) cs (\\d) po (.*) ps (.*)
str_mt3 <- str_match(
  str_def_tbl$V1,
  "STDS id '([^']*)' nm '([^']*)' ty (\\d).*")
str_def_tbl <- str_def[grepl("STDS id", V1)]
str_def_tbl$id <- str_mt3[, 2]
str_def_tbl$name <- str_mt3[, 3]
str_def_tbl$ty <- str_mt3[, 4]
# reading crest level, crest width
str_mt4 <- str_match(
  str_def_tbl$V1,
  "STDS id.* cl (.*) cw (\\d*\\.*\\d*)\\ .*")
str_def_tbl$cl <- str_mt4[, 2]
str_def_tbl$cw <- str_mt4[, 3]
# reading possible flow direction
str_mt5 <- str_match(
  str_def_tbl$V1,
  "STDS.*rt (\\d*\\.*\\d*)\\ .*")
str_def_tbl$rt <- str_mt5[, 2]
#----reading control.def----
