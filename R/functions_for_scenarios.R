#' Change NHWSP measures on Rhein OFF, on Nahe & Main ON (Zustand: Nur Neben Fluss)
#'
#' This function change structures of Guntersblum, Worringer, Orsoy, Lohrwadt OFF.
#' Change DRV Mündelheim OFF
#' open Sponsheim, Bretzenheim. Change DRV Hattersheim to ON
#' There is still things to do before running the case: Input from Worms and Input from Main Modell (with Bergrheinfeld)
#'
#' @param case.list List of cases to change
#' @param sobek.project Path to sobek project
#'
#' @return Nothing
#' @export
set_nur_neben_fluss <- function(
  case.list = NULL,
  sobek.project = NULL
){
  # set Nahe on
  set_nahe_on(case.list = case.list, sobek.project = sobek.project)
  nhwsp_rhein_river_weirs <- c(
    'guntersblum_zu', 'worringer_zu', 'orsoy_zu',
    'lohrwardt_zu_1', 'lohrwardt_zu_2'
  )
  nhwsp_rhein_weirs <- c(
    'guntersblum_ab', 'worringer_ab', 'orsoy_ab',
    'lohrwardt_ab'
  )

  for (case in case.list){
    # set Main on
    switch_DRV(
      drv.name = 'Hattersheim_hq25',
      case.name = case,
      sobek.project = sobek.project,
      status = 'mit',
      f.tbl = drv_def
    )
    # set Mündelheim off
    switch_DRV(
      drv.name = 'Mündelheim',
      case.name = case,
      sobek.project = sobek.project,
      status = 'ohne',
      f.tbl = drv_def
    )
    # reading struct.dat & struct.def
    st_dat_f <- get_file_path(
      case.name = case,
      sobek.project = sobek.project,
      type = "struct.dat"
    )
    st_def_f <- get_file_path(
      case.name = case,
      sobek.project = sobek.project,
      type = "struct.def"
    )
    st_dat <- .get_struct_dat(st_dat_f)
    st_def <- .get_struct_def(st_def_f)
    # get def_ID direct from st_dat, to avoid errors
    nhwsp_rhein_river_weirs_dd <- st_dat[id %in% nhwsp_rhein_river_weirs]$def_ID
    # set River Weir Off (control active 0 0 0 0, cw = 0)
    st_dat[id %in% nhwsp_rhein_river_weirs,
           V1 := str_replace(V1, ' ca \\d \\d \\d \\d ', ' ca 0 0 0 0 ')
           ]
    st_def[grepl("^STDS", V1) & def_ID %in% nhwsp_rhein_river_weirs_dd,
           V1 := str_replace(V1, ' cw \\d*\\.*\\d* ', ' cw 0 ')]
    # set possible flow direction of the weirs to 3 (no flow)
    nhwsp_rhein_weirs_dd <- st_dat[id %in% nhwsp_rhein_weirs]$def_ID
    st_def[grepl("^STDS", V1) & def_ID %in% nhwsp_rhein_weirs_dd,
           V1 := str_replace(V1, " rt \\d ", " rt 3 ")]
    # writing DEF & DAT file back
    file.copy(from = c(st_dat_f, st_def_f),
              to = paste(c(st_dat_f, st_def_f), ".BAK", sep = ""),
              overwrite = TRUE)
    fwrite(st_dat[, .(V1)], file = st_dat_f, quote = FALSE, col.names = FALSE)
    fwrite(st_def[, .(V1)], file = st_def_f, quote = FALSE, col.names = FALSE)
  }

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
  sobek.project = NULL,
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
      sobek.project,
      def.tbl = TRUE
    )
    dat_tbl <- p_def$dat
    def_tbl <- p_def$def
    # changing PROFILE.DAT to open Bretzenheim
    dat_tbl[dat_id == 'bretz_50',
            dat_file := str_replace(dat_file, " rl [^ ]* rs [^ ]* ", " rl 0 rs 99.96 ")]
    dat_tbl[dat_id == 'bretz_1450',
            dat_file := str_replace(dat_file, " rl [^ ]* rs [^ ]* ", " rl 0 rs 94.11 ")]
    dat_tbl[dat_id == 'sponsheim_cr_in'|dat_id == 'sponsheim_cr_out',
            dat_file := str_replace(dat_file, "di '9476'", "di '9477'")]
    # change Z bottom of sponsheim_pr*
    dat_tbl[grepl("sponsheim_.r", dat_id),
            dat_file := str_replace(dat_file, ' rl [^ ]+ rs ', ' rl 86.17 rs ')
            ]
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
    def_tbl[def_id == 9477,
            def_file := str_replace(def_file, "\\ *crds",
                                    "\\ crds")]
    # write profile.dat and profile.def
    fwrite(dat_tbl[, .(dat_file)],
           file = p_dat_f, quote = FALSE, col.names = FALSE)
    fwrite(def_tbl[, .(def_file)],
           file = p_def_f, quote = FALSE, col.names = FALSE)
  }
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
set_nahe_off <- function(
  case.list = NULL,
  sobek.project = NULL,
  w.in.rt = 3,
  w.in.cl = 87,
  w.in.cw = 50,
  w.out.rt = 3,
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
    # ct_def_f <- get_file_path(case.name = case,
    #                           sobek.project = sobek.project,
    #                           type = 'control.def')
    st_def_f <- get_file_path(case.name = case,
                              sobek.project = sobek.project,
                              type = 'struct.def')
    # changing STRUCT.DEF for closing sponsheim_wehr_in and sponsheim_wehr_out
    set_weir_info(
      w.id = 'sponsheim_wehr_in',
      struct.def = st_def_f,
      w.rt = 3,
      w.cl = w.in.cl,
      w.cw = w.in.cw
    )
    set_weir_info(
      w.id = 'sponsheim_wehr_out',
      struct.def = st_def_f,
      w.rt = 3,
      w.cl = w.out.cl,
      w.cw = w.out.cw
    )
    p_def <- get_profile_tbl(
      case,
      sobek.project,
      def.tbl = TRUE
    )
    dat_tbl <- p_def$dat
    def_tbl <- p_def$def
    # changing PROFILE.DAT to close Bretzenheim
    dat_tbl[dat_id == 'bretz_50',
            dat_file := str_replace(dat_file, " rl [^ ]* rs [^ ]* ", " rl 200 rs 299.96 ")]
    dat_tbl[dat_id == 'bretz_1450',
            dat_file := str_replace(dat_file, " rl [^ ]* rs [^ ]* ", " rl 200 rs 294.11 ")]
    dat_tbl[dat_id == 'sponsheim_cr_in'|dat_id == 'sponsheim_cr_out',
            dat_file := str_replace(dat_file, "di '9476'", "di '9477'")]
    # change Z bottom of sponsheim_pr*
    dat_tbl[grepl("sponsheim_.r", dat_id),
            dat_file := str_replace(dat_file, ' rl [^ ]+ rs ', ' rl 86.17 rs ')
            ]
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
    def_tbl[def_id == 9477,
            def_file := str_replace(def_file, "\\ *crds",
                                    "\\ crds")]
    # write profile.dat and profile.def
    fwrite(dat_tbl[, .(dat_file)],
           file = p_dat_f, quote = FALSE, col.names = FALSE)
    fwrite(def_tbl[, .(def_file)],
           file = p_def_f, quote = FALSE, col.names = FALSE)
  }
}

#' @export
add_zpw_main <- function(tbl) {
  case_tbl <- parse_case(tbl[, unique(case)])
  case_tbl[, zustand := str_replace(zustand, "Nur *", "Nur ")]
  case_tbl[grepl("Planz", zustand), zustand := "Alle Maßnahmen"]
  tbl <- merge(tbl, case_tbl, by = "case")
  # add ZPW
  tbl_zpw <- tbl[vgf == "VGF1" & hwe %in% c("HW1988", "HW1995",  "HW2003")]
  tbl_zpw[, zielpegel := "ZPW"]
  tbl_zpw[, vgf := "Mittel"]
  m_rows <- nrow(tbl_zpw)
  tbl_zpw <- rbind(tbl_zpw, tbl_zpw)
  tbl_zpw[1:m_rows, vgf := "Selten"]
  tbl <- rbind(tbl, tbl_zpw)
  tbl_nf <- tbl[grepl("Alle", zustand)]
  tbl_nf[, zustand := "Nur NF"]
  tbl_nr <- tbl[zustand == "Bezugszustand"]
  tbl_nr[, zustand := "Nur Rhein"]
  tbl <- rbind(tbl, tbl_nf, tbl_nr)
  tbl[, case := paste(zustand, zielpegel, hwe, vgf, sep = "_")]
  tbl[, c("zustand", "zielpegel", "hwe", "vgf", "notiz", "case_desc") := rep(NULL, 6)]
  tbl
}
