#' Change profile definitions for DRV
#' @param case.name: Name of case
#' @param sobek.project: Path to Sobek Project
#' @param drv.name: Name of DRV
#' @param f.tbl Path to profile definition table
#' @return Result will be written directly to the profile.dat
#' @export
switch_DRV <- function(case.name, sobek.project, drv.name = NULL,
                       status = "mit", f.tbl){
  if (!is.data.table(f.tbl)){
    def.tbl <- fread(f.tbl, sep = "\t", header = T, quote = "")
  } else{
    def.tbl <- copy(f.tbl)
  }
  stopifnot(drv.name %in% def.tbl$DRV)
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
#' @param w.id ID of the weir
#' @param def.file Path to struct.def file, get it by function get_file_path
#' @param w.rt Possible flow direction (0-both, 1-positive, 2-negative, 3-no flow)
#' @param w.cl Crest level
#' @param w.cw Crest width
#' @export
set_weir_info <- function(w.id, struct.def,
                          w.rt = NULL, w.cl = NULL, w.cw = NULL){

  stopifnot(is.numeric(w.cl) & is.numeric(w.cw))
  st_def <- fread(file = struct.def,
                       header = F,
                       sep = "\n")
  id_pattern = paste("STDS id '", w.id, "' ", sep = '')
  st_line <- grep(id_pattern, st_def$V1)
  f_changed <- FALSE
  if (length(st_line) == 1){
    if (!is.null(w.rt)){
      if (!w.rt %in% c(0L, 1L, 2L, 3L)) stop('wrong direction value')
      f_changed <- TRUE
      st_def[st_line,
             V1 := str_replace(V1,
                               "rt [0-9]{1}",
                               paste("rt ", w.rt, sep = ""))]
    }
    if (!is.null(w.cl)){
      f_changed <- TRUE
      stopifnot(is.numeric(w.cl))
      st_def[st_line,
             V1 := str_replace(V1,
                               "cl [^ ]+ ",
                               paste("cl ", w.cl, " ", sep = ""))]
    }
    if (!is.null(w.cw)){
      f_changed <- TRUE
      st_def[st_line,
             V1 := str_replace(V1,
                               "cw [^ ]+ ",
                               paste("cw ", w.cw, " ", sep = ""))]
    }
  }
  if (f_changed){
    fwrite(st_def, struct.def, col.names = F, row.names = F, quote = FALSE)
  } else{
    warning('Weir ID not found or no information provided. File was not written!')
  }
  return(f_changed)
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
#' @param pf.df data.frame or path to the table of cross sections (profile.dat)
#' @param rl.new New value of bed level left
#' @param rs.new New value of surface level right, default = rl.new + 10
#' @export
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



#' This function is only for open Sponsheim and Bretzenheim in Rhein model
#'
#' FUNCTION_DESCRIPTION
#'
#' @param case.list DESCRIPTION.
#' @param sobek.project DESCRIPTION.
#' @param w.in.rt DESCRIPTION.
#' @param w.in.cl DESCRIPTION.
#' @param w.in.cw DESCRIPTION.
#' @param w.out.rt DESCRIPTION.
#' @param w.out.cl DESCRIPTION.
#' @param w.out.cw DESCRIPTION.
#'
#' @export
set_nahe_on <- function(
  case.list = NULL,
  sobek.project = so_prj,
  w.in.rt = 0,
  w.in.cl = 87,
  w.in.cw = 50,
  w.out.rt = 0,
  w.out.cl = 86.17,
  w.out.cw = 50
){
  for (case in case.list){
    p_def_f <- get_file_path(case.name = case,
                             sobek.project = sobek.project,
                             type = 'profile.def')
    p_dat_f <- get_file_path(case.name = case,
                             sobek.project = sobek.project,
                             type = 'profile.dat')
    ct_def_f <- get_file_path(case.name = case,
                              sobek.project = sobek.project,
                              type = 'control.def')
    st_def_f <- get_file_path(case.name = case,
                              sobek.project = sobek.project,
                              type = 'struct.def')
    # changing STRUCT.DEF for opening sponsheim_wehr_in and sponsheim_wehr_out
    set_weir_info(
      w.id = 'sponsheim_wehr_in',
      struct.def = st_def_f,
      w.rt = w.in.rt,
      w.cl = w.in.cl,
      w.cw = w.in.cw
    )
    set_weir_info(
      w.id = 'sponsheim_wehr_out',
      struct.def = st_def_f,
      w.rt = w.out.rt,
      w.cl = w.out.cl,
      w.cw = w.out.cw
    )
    p_def <- get_profile_tbl(
      case,
      so_prj,
      def.tbl = TRUE
    )
    dat_tbl <- p_def$dat
    def_tbl <- p_def$def
    # changing PROFILE.DAT to open Bretzenheim
    dat_tbl[grepl('bretz_\\d+', dat_id) ,
            dat_file := str_replace(dat_file, " rl [^ ]* rs ", " rl 0 rs ")]
    # change Z bottom of sponsheim_pr*
    dat_tbl[grepl("sponsheim_.r", dat_id),
            dat_file := str_replace(dat_file, ' rl [^ ]+ rs ', ' rl 86.17 rs ')
            ]
    # change PROFILE.DEF to adjust Sponsheim CRs
    def_tbl[def_nm == "r_sponsheim_profile",
            def_file := str_replace(def_file,
                                    ' wm [^ ]+ rw [^ ]+ ',
                                    ' wm 546.6 rw 546.6 ')]
    def_tbl[def_id == 9477,
            def_file := str_replace(def_file, "([^ ]+) ([^ ]+) ([^ ]+) <",
            "\\1 546.6 546.6 <")]
    # write profile.dat and profile.def
    fwrite(dat_tbl[, .(dat_file)],
           file = p_dat_f, quote = FALSE, col.names = FALSE)
    fwrite(def_tbl[, .(def_file)],
           file = p_def_f, quote = FALSE, col.names = FALSE)
  }
}


#' This function read the profile.def to a data.table
#'
#' @param case Sobek case name
#' @param sobek.project Path to sobek project
#' @param all.line Get all data with TBLE or only information table? Default is TRUE
#' @return a data.table
#' @export
get_profile_tbl <- function(
  case,
  sobek.project = so_prj,
  dat.tbl = TRUE,
  def.tbl = FALSE
  ){
  stopifnot(TRUE %in% c(dat.tbl, def.tbl))
  p_dat_f <- get_file_path(case.name = case,
                           sobek.project = sobek.project,
                           type = 'profile.dat')
  p_def_f <- get_file_path(case.name = case,
                           sobek.project = sobek.project,
                           type = 'profile.def')
  p_dat <- fread(p_dat_f, sep = "\n", header = FALSE)
  p_def <- fread(p_def_f, sep = "\n", header = FALSE)
  # parse DAT file
  # id of cross-section
  p_dat[, dat_id := str_match(V1, "CRSN id '([^']+)' ")[, 2]]
  # p_dat[, dat_row_id := .I]
  # id of the cross-section DEFINITION
  p_dat[, def_id := str_match(V1, " di '([^']+)' ")[, 2]]
  # reference level 1
  p_dat[, rl := as.numeric(str_match(V1, " rl (\\d*\\.*\\d*) ")[, 2])]
  # surface level right
  p_dat[, rs := as.numeric(str_match(V1, " rs (\\d*\\.*\\d*) ")[, 2])]
  # parse DEF File
  p_def[, def_row_id := .I]
  p_def[, def_id := str_match(V1, "CRDS id '([^']+)' ")[, 2]]
  p_def[!is.na(def_id), def_nm := str_match(V1, " nm '([^']+)' ")[, 2]]
  p_def[!is.na(def_id), ty := str_match(V1, " ty (\\d{1,2}) ")[, 2]]
  p_def[, V2 := shift(V1, n = 2, type = 'lead')]
  p_def[!is.na(def_id), zb := as.numeric(str_match(V2, "^(\\d*\\.*\\d*) ")[, 2])]
  p_def[, V2 := NULL ]
  setnames(p_def, 'V1', 'def_file')
  setnames(p_dat, 'V1', 'dat_file')
  # avoid duplicated records of Rhe_596.30_3901 in rhein model
  p_def[def_id == 'Rhe_596.30_3901' &
          def_row_id > 18 & !is.na(def_id),
        def_id := 'Rhe_596.30_3901_BK']
  p_dat <- merge(p_dat, p_def, by = 'def_id', sort = FALSE)
  p_dat[, zb := zb + rl]
  p_dat[, c("def_file", "def_row_id") := list(NULL, NULL)]
  p_def[, def_id := def_id[1], .(cumsum(!is.na(def_id)))]

  result <- list(dat = p_dat, def = p_def)[c(dat.tbl, def.tbl)]
  if (length(result) == 1) result <- result[[1]]

  return(result)
}
