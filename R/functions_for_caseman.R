init_sbk_work <- function(case.folder, tmp.folder, no.his = TRUE) {
  wk_files <- dir(case.folder, full.names = TRUE)
  wk_files <- wk_files[!grepl("/[0-9]{1,}$", wk_files)]
  # exclude old .HIS files to save HDD space
  if (no.his) {
    wk_files <- grep(pattern = '\\.his$', wk_files,
                     invert = TRUE,
                     ignore.case = TRUE, value = TRUE)
  }
  cmt_folder <- file.path(tmp.folder, "CMTWORK")
  if (!dir.exists(cmt_folder)) dir.create(cmt_folder)
  wk_folder <- file.path(tmp.folder, "WORK")
  if (!dir.exists(wk_folder)) dir.create(wk_folder)
  all(file.copy(from = wk_files, to = wk_folder, overwrite = TRUE))
}


#' Prepare casedesc.cmt and appropriate ini file for SOBEK task
#'
init_sbk_cmt <- function(
  case.folder,
  tmp.folder,
  sobek.path,
  fix.data = NULL,
  copy.others = TRUE,
  type = c("view", "edit", "simulate")
) {
  type <- match.arg(tolower(type), c("view", "edit", "simulate"))
  cmt_folder <- paste0(tmp.folder, "\\CMTWORK")
  wk_folder <- paste0(tmp.folder, "\\WORK")
  stopifnot(dir.exists(cmt_folder))
  wkd <- getwd()
  setwd(cmt_folder)
  desc_cmt_f <- paste0(wk_folder, "\\casedesc.cmt")
  desc_cmt <- fread(desc_cmt_f,
                    sep = "\n", encoding = "UTF-8", header = FALSE)
  # change folder to WORK
  desc_cmt[, V1 := stri_replace_all_regex(
    V1, (" .*\\\\\\d{1,}\\\\"),
    " \\.\\.\\\\WORK\\\\")
  ]
  desc_cmt[, V1 := stri_replace_all_regex(
    V1, paste0(" .*\\\\#\\\\"),
    " \\.\\.\\\\#\\\\")
  ]
  # backup original fixed data in casedesc.cmt
  orig_fix <- desc_cmt[grep("[IO]{1,2} .*\\FIXED", V1, ignore.case = TRUE), V1]
  if (!is.null(fix.data)) {
    fix_pat <- paste0("$1 ", fix.data)
    fix_pat <- stri_replace_all_fixed(fix_pat, "\\", "\\\\")
    fix_pat <- stri_replace_all_fixed(fix_pat, "/", "\\\\")
    fix_pat <- stri_replace_all_fixed(fix_pat, ".", "\\.")
    # change path to fixed folder
    desc_cmt[grep("[IO]{1,2} .*\\\\FIXED", V1, ignore.case = TRUE),
             V1 := stri_replace_first_regex(
               V1,
               "([IO]{1,2}) .*\\\\FIXED", fix_pat,
               opts_regex = stri_opts_regex(case_insensitive = TRUE))]
  } else {
    # change related path of fixed to project folder
    fix_pat <- paste0(dirname(case.folder), "\\FIXED")
    fix_pat <- stri_replace_all_fixed(fix_pat, "\\", "\\\\")
    fix_pat <- stri_replace_all_fixed(fix_pat, ".", "\\.")
    desc_cmt[grep("[IO]{1,2} \\.\\.\\\\FIXED", V1, ignore.case = TRUE),
             V1 := stri_replace_first_regex(
               V1,
               "([IO]{1,2}) \\.\\.\\\\FIXED", fix_pat,
               opts_regex = stri_opts_regex(case_insensitive = TRUE))]
  }
  fwrite(desc_cmt[, c("V1")], file = "casedesc.cmt",
         sep = "\n", col.names = FALSE, quote = FALSE)
  des_cmt_f <- file.path(dirname(case.folder), "DESCPROT.CMT")
  if (!file.exists(des_cmt_f)) {
    cat("File: ", des_cmt_f, " not found. Sobek can behave unexpectedly!\n")
    des_cmt_f <- paste0(sobek.path, "\\New\\Project\\Default.flw\\DESCPROT.CMT")
  }
  stopifnot(file.exists(des_cmt_f))
  file.copy(des_cmt_f, tmp.folder, overwrite = TRUE)
  if (type == "view") {
    reg_cmd <- paste0(sobek.path, "\\programs\\vervng32.exe ",
                      sobek.path, "\\programs\\ini\\vv_map.ini")
    system(reg_cmd, wait = TRUE, invisible = TRUE)
    cmt_files <- list.files(cmt_folder, all.files = TRUE, no.. = TRUE)
    sapply(cmt_files, replace_ini_fixed, fix.data = fix.data)
    reg_cmd <- paste0(sobek.path, "\\programs\\vervng32.exe ",
                      sobek.path, "\\programs\\ini\\Vv_schem.ini")
    system(reg_cmd, wait = TRUE, invisible = TRUE)
    cmt_files <- list.files(cmt_folder, all.files = TRUE, no.. = TRUE)
    sapply(cmt_files, replace_ini_fixed, fix.data = fix.data)
    prep_ini <- fread(file = file.path(sobek.path, "programs\\prepmapp.ini"),
                      sep = "\n", header = FALSE)
    # remove Register command, because we have no parent thread
    prep_ini[grep("NrOfCalls", V1, ignore.case = TRUE), V1 := "NrOfCalls=0"]
    prep_ini[grep("NrOfCalls", V1, ignore.case = TRUE), V1 := "NrOfCalls=0"]
    prep_ini[grep("@", V1, fixed = TRUE),
             V1 := stri_replace_all_fixed(V1, "@", "..\\WORK\\")]
    fwrite(prep_ini, "prepmapp.ini", col.names = FALSE, sep = "\n", quote = FALSE)
    # manually give path to HIS files, write them to "nettmp.dlf"
    nettmp <- fread(file.path(cmt_folder, "nettmp.dlf"), sep = "\n", header = FALSE)
    nettmp[, V1 := stri_replace_first_fixed(
      V1, '"..\\work', paste0('"', case.folder),
      opts_fixed = stri_opts_fixed(case_insensitive = TRUE))]
    fwrite(nettmp, file = file.path(cmt_folder, "nettmp.dlf"), sep = "\n",
           col.names = FALSE, quote = FALSE)
  } else if (type == "edit") {
    reg_cmd <- paste0(sobek.path, "\\programs\\vervng32.exe ",
                      sobek.path, "\\programs\\ini\\Vv_schem.ini")
    system(reg_cmd, wait = TRUE, invisible = TRUE)
    cmt_files <- list.files(cmt_folder, all.files = TRUE, no.. = TRUE)
    sapply(cmt_files, replace_ini_fixed, fix.data = fix.data)
    schemat_ini <- fread(file = paste0(sobek.path, "\\programs\\schemat.ini"),
                         sep = "\n", header = FALSE)
    schemat_ini[grep("NrOfCalls", V1, ignore.case = TRUE), V1 := "NrOfCalls=0"]
    ntc_files <- schemat_ini[grepl("NtcFile=", V1, ignore.case = TRUE) &
                               !grepl("DestNtcFile=", V1, ignore.case = TRUE)]
    ntc_files[, c("V2", "V3") := tstrsplit(V1, "=")]
    ntc_files[, V4 := paste0(".\\", basename(V3))]
    ntc_files[, V1 := paste0(V2, "=", V4)]
    # redirect NtcFile to temp CMTWORK
    schemat_ini[grepl("NtcFile=", V1, ignore.case = TRUE) &
                  !grepl("DestNtcFile=", V1, ignore.case = TRUE),
                V1 := ntc_files$V1]
    prj_folder <- gsub("/", "\\\\", dirname(case.folder))
    for (i in seq_along(ntc_files$V3)) {
      ntc_tbl <- fread(file = ntc_files$V3[[i]], sep = "\n", header = FALSE)
      ntc_tbl[!grepl("\\.\\.\\\\work", V1, ignore.case = TRUE),
              V1 := stri_replace_first_fixed(V1, "..", prj_folder)]
      fwrite(ntc_tbl, file = file.path(cmt_folder, basename(ntc_files$V3[[i]])),
             col.names = FALSE, sep = "\n", quote = FALSE)
    }
    fwrite(schemat_ini, "schemat.ini", col.names = FALSE, sep = "\n", quote = FALSE)
    expand_netter(case.folder = case.folder, tmp.folder = tmp.folder,
                  sobek.path = sobek.path)
  } else {
    # init simulate.ini
    reg_cmd <- paste0(sobek.path, "\\programs\\vervng32.exe casedesc.cmt PLUVIUS1 ",
                   sobek.path, "\\programs\\simulate.ini simulate.ini ..\\descprot.cmt")
    system(command = reg_cmd, wait = TRUE)
  }
  if (copy.others) {
    prj_folder <- dirname(case.folder)
    dirs_2_cp <- list.dirs(prj_folder, recursive = FALSE, full.names = TRUE)
    dirs_2_cp <- grep("/\\d{1,}$|WORK$|NEWSTART$", dirs_2_cp,
                      ignore.case = TRUE, value = TRUE, invert = TRUE)
    if (length(dirs_2_cp) > 0)
      file.copy(dirs_2_cp, tmp.folder, recursive = TRUE, overwrite = FALSE)
  }
  setwd(wkd)
  invisible(orig_fix)
}

replace_ini_fixed <- function(f_ini, fix.data) {
  f_chk <- FALSE
  if (!is.data.table(f_ini)) {
    f_path <- f_ini
    f_info <- file.info(f_ini)
    if (f_info$size == 0) return(TRUE)
    f_ini <- fread(file = f_ini, sep = "\n", header = FALSE)
    f_chk <- TRUE
  }
  fix.data <- normalizePath(fix.data)
  fix.data <- stri_replace_all_fixed(fix.data, "\\", "\\\\")
  fix.data <- paste0("=", fix.data)
  n_row <- nrow(f_ini[grepl("\\\\fixed", V1, ignore.case = TRUE)])
  f_ini[grepl("\\\\fixed", V1, ignore.case = TRUE),
        V1 := stri_replace_first_regex(
          V1, "=.*\\\\fixed",
          fix.data,
          opts_regex = stri_opts_regex(case_insensitive = TRUE))]
  if (n_row > 0 & f_chk) {
    fwrite(f_ini, file = f_path, quote = FALSE, sep = "\n", col.names = FALSE)
  } else {
    invisible(f_ini)
  }
}

#' remove double quote at begining and at the end of a string
remove_dbl_quote <- function(x) {
  x <- stri_replace_first_fixed(x, '"', '')
  x <- stri_replace_last_fixed(x, '"', '')
  x
}


expand_netter <- function(case.folder, tmp.folder, sobek.path) {
  # if .HIS files are not in the work folder the try to assign them from SobekErgebnisse
  netter_f <- file.path(tmp.folder, "work\\netter.dlf")
  if (file.exists(netter_f)) {
    netter <- fread(file = netter_f,
                    sep = "\n", header = FALSE)
    his_files <- netter[, stri_match_first_regex(V1, '"@(.+his)"',
                                                 opts_regex = stri_opts_regex(TRUE))[, 2]]
    his_files <- his_files[!is.na(his_files)]
    wk_his_files <- paste0(case.folder, "\\", his_files)
    for (i in seq_along(his_files)) {
      netter[grep(paste0("@", his_files[i]), V1), V1 := stri_replace_first_fixed(
        V1,
        paste0("@", his_files[i]),
        wk_his_files[i]
      )]
    }
    fwrite(netter, file = file.path(tmp.folder, "cmtwork\\netter2.dlf"),
           sep = "\n", col.names = FALSE, quote = FALSE)
  }
  netter1_ntc <- fread(file = file.path(sobek.path, "programs\\netter1.ntc"),
                       header = FALSE, sep = "\n")
  sbk_map <- netter1_ntc[grepl("\\.\\..*map$", V1, ignore.case = TRUE), V1]
  sbk_prj_path <- paste0(gsub("/", "\\\\", dirname(case.folder)), "\\")
  sbk_map <- stri_replace_all_fixed(sbk_map, "..\\", sbk_prj_path)
  netter2_ntc <- c(
    "NTC1.00", sbk_map,
    "..\\WORK\\NETWORK.NTW",
    "netter2.dlf")
  fwrite(data.table(V1 = netter2_ntc),
    file = file.path(tmp.folder, "cmtwork\\netter2.ntc"),
    sep = "\n", col.names = FALSE, quote = FALSE)
}

copy_bui_sbdef <- function(sobek.path, fix.data) {
  f_files <- list.files(path = file.path(sobek.path, "fixed"),
                        recursive = FALSE, full.names = TRUE, no.. = TRUE)
  f_files <- tolower(f_files)
  f_files <- grep("default|sbkedit", f_files, value = TRUE)
  dest_files <- file.path(fix.data, basename(f_files))
  f_files <- f_files[file.exists(dest_files)]
  dest_files <- dest_files[file.exists(dest_files)]
  if (length(dest_files) > 0) {
    file.copy(f_files, fix.data)
    on.exit(unlink(dest_files))
  }
}


#' Check if a list of files or a folder has been changed
#'
#' @param before Path to folder or list of files before
#' @param after Path to folder or list of files after
#' @return a character vector of files that are new or changed
md5_changed <- function(before, after) {
  before <- unlist(before)
  after <- unlist(after)
  stopifnot(all.equal(dirname(before), dirname(before)))
  stopifnot(all.equal(dirname(after), dirname(after)))
  before_dir <- normalizePath(before)
  after_dir <- normalizePath(after)
  if (length(before) == 1 | length(after) == 1) {
    stopifnot(length(before) == length(after))
    if (file.info(before)$isdir & file.info(after)$isdir) {
      before_fnames <- list_only_files(before)
      after_fnames <- list_only_files(after)
    } else {
      stopifnot(!file.info(before)$isdir, !file.info(after)$isdir)
      before_fnames <- before
      after_fnames <- after
      before_dir <- normalizePath(dirname(before))
      after_dir <- normalizePath(dirname(after))
    }
  } else {
    before_fnames <- before
    after_fnames <- after
  }
  before_bnames <- tolower(basename(before_fnames))
  after_bnames <- tolower(basename(after_fnames))
  new_files <- setdiff(after_bnames, before_bnames)
  if (length(new_files) > 0) {
    new_files <- paste(after_dir, new_files, sep = "\\")
  } else {
    new_files <- NA
  }
  intersect_files <- intersect(before_bnames, after_bnames)
  if (length(intersect_files) > 0) {
    before_int_files <- paste(before_dir, intersect_files, sep = "\\")
    after_int_files <- paste(after_dir, intersect_files, sep = "\\")
    before_md5 <- suppressWarnings(tools::md5sum(before_int_files))
    after_md5 <- suppressWarnings(tools::md5sum(after_int_files))
    changed_files <- after_int_files[before_md5 != after_md5]
  } else {
    changed_files <- NA
  }
  return(list(new = new_files, changed = changed_files))
}


list_only_files <- function(path) {
  ret <- setdiff(
    list.files(path, full.names = TRUE, all.files = TRUE,
               recursive = FALSE, no.. = TRUE),
    list.dirs(path, recursive = FALSE, full.names = TRUE)
  )
  normalizePath(ret)
}


#' Compare and overwrite changed files from a working folder back to its original
#'
#' @param orig Path to original folder
#' @param work Path to working folder
#' @param except.dirs Pattern to exclude directories
#' @param except.files Pattern to exclude files
#' @export
save_changed_files <- function(
  orig, work, except.dirs = NULL, except.files = NULL
){
  changes <- vector(mode = "character")
  orig <- normalizePath(orig)
  work <- normalizePath(work)
  work_dirs <- normalizePath(list.dirs(work))
  if (!is.null(except.dirs))
    work_dirs <- grep(except.dirs, work_dirs,
                      ignore.case = TRUE, invert = TRUE, value = TRUE)
  orig_dirs <- stri_replace_all_fixed(work_dirs, work, orig)
  for (i in seq_along(work_dirs)) {
    work_d <- work_dirs[i]
    orig_d <- orig_dirs[i]
    changed_files <- md5_changed(before = orig_d, after = work_d)
    changed_files <- unlist(changed_files)
    changed_files <- changed_files[!is.na(changed_files)]
    if (!is.null(except.files))
      changed_files <- grep(except.files, changed_files,
                          ignore.case = TRUE, invert = TRUE, value = TRUE)
    if (length(changed_files) > 0)
      file.copy(from = changed_files, to = orig_d, overwrite = TRUE)
    changes <- c(changes, changed_files)
  }
  changes <- stri_replace_all_fixed(changes, work, orig)
  invisible(changes)
}


parse_sobek_ini <- function(path) {
  stopifnot(file.exists(path))
  tbl <- data.table::fread(file = path, sep = "\n", header = FALSE,
                           blank.lines.skip = TRUE)
  tbl[, org_ln := .I]
  tbl[, grp := stri_match_first_regex(V1, "\\[(.+)]")[, 2]]
  tbl[, grp := grp[1], .(cumsum(!is.na(grp)))]
  tbl[, c("var", "val") := data.table::tstrsplit(V1, "=", fixed = TRUE)]
  return(tbl)
}


time_to_list <- function(input) {
  # assume that input is given as character or POSIXct
  if (class(input) == "character") {
    ret <- as.POSIXct(
      input,
      tryFormats = c(
        # US
        "%Y-%m-%d %H:%M:%S",
        "%Y/%m/%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y/%m/%d %H:%M",
        "%Y-%m-%d",
        "%Y/%m/%d",
        # DE with .
        "%d.%m.%Y %H:%M:%S",
        "%d.%m.%Y %H:%M:%S",
        "%d.%m.%Y %H:%M",
        "%d.%m.%Y %H:%M",
        "%d.%m.%Y",
        # DE with -
        "%d-%m-%Y %H:%M:%S",
        "%d-%m-%Y %H:%M:%S",
        "%d-%m-%Y %H:%M",
        "%d-%m-%Y %H:%M",
        "%d-%m-%Y"
      )
    )
  } else {
    ret <- input
  }
  ret <- format(ret, format = "%d-%m-%Y-%H-%M-%S")
  ret <- as.numeric(strsplit(ret, "-")[[1]])
  ret <- list(
    hh = ret[4],
    mm = ret[5],
    ss = ret[6],
    d = ret[1],
    m = ret[2],
    y = ret[3]
  )
  return(ret)
}

change_settings <- function(begin = NULL, end = NULL, dat) {
  time_chk <- any(!is.null(begin), !is.null(end))
  if (time_chk) {
    set_dat <- parse_sobek_ini(dat)
    # store original settings.dat
    org_setting <- set_dat[, c("V1")]
    if (!is.null(begin)) {
      begin <- time_to_list(begin)
      begin_ln <-  set_dat[grepl("^Begin", var) &
                             grepl("Simulation", grp, ignore.case = TRUE), org_ln]
      begin_v1 <- c(
        paste0("BeginSecond=", begin$ss),
        paste0("BeginMinute=", begin$mm),
        paste0("BeginHour=", begin$hh),
        paste0("BeginDay=", begin$d),
        paste0("BeginMonth=", begin$m),
        paste0("BeginYear=", begin$y)
      )
      set_dat[org_ln %in% begin_ln, V1 := begin_v1]
    }
    if (!is.null(end)) {
      end <- time_to_list(end)
      end_ln <-  set_dat[grepl("^End", var) &
                           grepl("Simulation", grp, ignore.case = TRUE), org_ln]
      end_v1 <- c(
        paste0("EndSecond=", end$ss),
        paste0("EndMinute=", end$mm),
        paste0("EndHour=", end$hh),
        paste0("EndDay=", end$d),
        paste0("EndMonth=", end$m),
        paste0("EndYear=", end$y)
      )
      set_dat[org_ln %in% end_ln, V1 := end_v1]
    }
    fwrite(
      set_dat[, c("V1")],
      file = dat,
      sep = "\n", col.names = FALSE, row.names = FALSE, quote = FALSE
    )
    invisible(org_setting)
  } else {
    FALSE
  }
}

expand_sobek_path <- function(
  path,
  tmp.folder,
  case.folder,
  expand = c("work", "case"),
  sobek.path,
  fix.data = NULL,
  dest = file.path(tmp.folder, "CMTWORK")
) {
  if (!file.exists(path)) return(FALSE)
  f_ini <- fread(file = path, sep = "\n", header = FALSE)
  expand <- match.arg(expand, c("work", "case"))
  if (expand == "work")
    pat <- "..\\WORK\\"
  else
    pat <- paste0(case.folder, "\\")
  f_ini[grepl("@", V1),
        V1 := stri_replace_first_fixed(V1, "@", pat)]
  f_ini[grepl("^NetDir", V1), V1 := paste0("NetDir=", sobek.path)]
  f_ini[grepl("^LocalDir", V1),
        V1 := paste0("LocalDir=",
                     stri_replace_first_regex(sobek.path, "^.{1}:", ""))]
  f_ini[grepl("@event", V1), V1 := "EventFile=..\\WORK\\settings.dat"]
  if (is.null(fix.data)) {
    fix.data <- paste0(dirname(case.folder), "\\fixed")
  }
  fix.data <- normalizePath(fix.data)
  fix.data <- stri_replace_all_fixed(fix.data, "\\", "\\\\")
  fix.data <- paste0("=", fix.data)
  f_ini[grepl("\\\\fixed", V1, ignore.case = TRUE),
        V1 := stri_replace_first_regex(
          V1, "=.*\\\\fixed",
          fix.data,
          opts_regex = stri_opts_regex(case_insensitive = TRUE))]
  fwrite(f_ini, file.path(dest, basename(path)), sep = "\n", quote = FALSE,
         col.names = FALSE)
}

get_case_folder <- function(case.list, sobek.project) {
  sobek.project <- gsub("/", "\\\\", sobek.project)
  case.list <- unlist(case.list)
  case_tbl <- fread(file = paste0(sobek.project, "\\caselist.cmt"),
                    sep = " ", quote = "'", header = FALSE,
                    col.names = c("case_number", "case_name"))
  case_tbl[, case_name := remove_dbl_quote(case_name)]
  ret <- vector(mode = "integer")
  for (i in seq_along(case.list)) {
    # support reading case currently managed by SOBEK GUI
    c_name <- case.list[[i]]
    if (tolower(c_name) == "work") {
      ret[i] <- paste0(sobek.project, "\\work")
      next
    }
    c_number <- case_tbl[case_name == c_name, case_number]
    if (length(c_number) == 0) {
      c_number <- case.list[[i]]
      c_name <- case_tbl[case_number == c_name, case_name]
    }
    if (length(c_name) > 0 & length(c_number) > 0) {
      ret[i] <- paste0(sobek.project, "\\", c_number)
      if (!file.exists(ret[i]))
        ret[i] <- NA
    } else {
      ret[i] <- NA
    }
  }
  ret
}


get_case_name <- function(case.folder, sobek.project) {
  sobek.project <- gsub("/", "\\\\", sobek.project)
  case.folder <- unlist(case.folder)
  case_tbl <- fread(file = paste0(sobek.project, "\\caselist.cmt"),
                    sep = " ", quote = "'", header = FALSE,
                    col.names = c("case_number", "case_name"))
  case_tbl[, case_name := remove_dbl_quote(case_name)]
  ret <- vector(mode = "character")
  for (i in seq_along(case.folder)) {
    # support reading case currently managed by SOBEK GUI
    c_number <- case.folder[[i]]
    c_name <- case_tbl[case_number == c_number, case_name]
    ret[i] <- ifelse(length(c_name) == 1, c_name, NA)
  }
  ret
}
