#' Change profile definitions for DRV
#' @param case.name: Name of case
#' @param sobek.project: Path to Sobek Project
#' @param drv.name: Name of DRV
#' @param f.tbl Path to profile definition table
#' @return Result will be written directly to the profile.dat
#' @export
switch_DRV <- function(case.name, sobek.project, drv.name = NULL,
                       status = "mit", f.tbl){
  def.tbl <- fread(f.tbl, sep = "\t", header = T, quote = "")
  case_folder <- dirname(get_file_path(case.name = case.name,
                                       sobek.project = sobek.project,
                                       type = 'node'))
  pf.dat <- fread(file = paste(case_folder, 'profile.dat', sep = "/"),
                  sep = "\n",
                  header = F)
  for (i in def.tbl[DRV == drv.name, id]){
    if(status == "mit"){
      id_line <- def.tbl[id == as.character(i), DRV_in_qp]
    } else{
      id_line <- def.tbl[id == as.character(i), DRV_nicht_in_qp]
    }
    pf.dat[grepl(paste("CRSN id '", i, "'", sep = ""), V1, fixed = TRUE),
           V1 := id_line]
  }
  fwrite(pf.dat, file =  paste(case_folder, 'profile.dat', sep = "/"),
         col.names = FALSE, row.names = FALSE, quote = FALSE)
}


#' Set possible flow  direction. Use it to close or open a simple weir
#' @param weir.id ID of the weir
#' @param f.struct Path toe control.def file, get it by function get_file_path
#' @param direction Possible flow direction (0-both, 1-positive, 2-negative, 3-no flow)
#' @export
set_weir_pfd <- function(weir.id, f.struct, direction = 1L){
  if (!direction %in% c(0L, 1L, 2L, 3L)) stop('wrong direction value')
  control.def <- fread(file = f.struct,
                       header = F,
                       sep = "\n")
  id_pattern = paste("STDS id '", weir.id, "' ", sep = '')
  check <- grep(id_pattern, control.def$V1)
  if (length(check) == 1){
    control.def[grepl(id_pattern, V1),
                V1:=sub("rt [0-9]{1}",
                        paste("rt ", direction, sep = " "),
                        V1)]
    fwrite(control.def, f.struct, col.names = F, row.names = F, quote = FALSE)
    return(TRUE)
  } else{
    return(FALSE)
  }
}


pf_extract <- function(pf.str, ptype = 'id'){
  pattern <-  "CRSN id '(.*)' di '(.*)' rl (\\S*) (.*)"
  pattern_rs <-  "CRSN id '(.*)' di '(.*)' rl (\\S*) rs (\\S*) (.*)"
  str.matrix <- str_match(pf.str, pattern)
  str.matrix_rs <- str_match(pf.str, pattern_rs)
  # tcol <- ncol(str.matrix)
  rel <- NA
  # if (tcol == 1) rel <- NA
  if (ptype == 'id') rel <- str.matrix[, 2]
  if (ptype == 'di') rel <- str.matrix[, 3]
  if (ptype == 'rl') rel <- str.matrix[, 4]
  if (ptype == 'rs'){
    rel <- str.matrix_rs[, 5]
  }
  if (ptype == 'rest'){
    if (is.na(str.matrix_rs[1,1])) {
      rel <- str.matrix[1,5]
    } else{
      rel <- str.matrix_rs[1, 6]
    }
  }
  return(rel)
}

#' Change reference level of a cross section
#' @param crsn.id ID of cross section
#' @pf.df data.frame or path to the table of cross sections (profile.dat)
#' @param rl.new New value of bed level left
#' @param rs.new New value of surface level right, default = rl.new + 10
#' @export
#' @import stringr
change_rl_rs <- function(crsn.id, pf.df, rl.new = 0, rs.new = rl.new + 10){
  write_output <- FALSE
  if(is.data.frame(pf.df)){
    pf <- data.table(pf.df)
  } else{
    write_output <- TRUE
    pf <- fread(file = pf.df, header = F, sep = "\n")
  }
  pf$id <- lapply(pf[, 1], pf_extract, ptype = "id")
  pf.str <- pf$V1[pf$id == crsn.id]
  # pattern <-  "CRSN id '(.*)' di '(.*)' rl (\\S*) (.*)"
  pattern_rs <-  "CRSN id '(.*)' di '(.*)' rl (\\S*) rs (\\S*) (.*)"
  # str.matrix <- str_match(pf.str, pattern)
  str.matrix_rs <- str_match(pf.str, pattern_rs)
  rs_old <- str.matrix_rs[1, 5]
  if (is.na(rs_old)){
    cout <- str_replace(pf.str,
                        "(CRSN id '.*rl )(\\S*)(.*)",
                        paste("\\1", rl.new, "\\3", sep = ""))
  } else{
    cout <- str_replace(pf.str,
                        "(CRSN id '.*rl )(\\S*) rs (\\S*)(.*)",
                        paste("\\1", rl.new, " rs ", rs.new, "\\4", sep = "")
    )
  }
  pf[pf$id == crsn.id, 1] <- cout
  if (write_output) {
    write.table(pf[, V1],
                file = pf.df,
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE
                  )
  }
  return(cout)
}
