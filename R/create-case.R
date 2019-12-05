# This function changes the simulation start and ending time in a sobek setting
# The input time is an POSIXCt or an object that is coerceable to POSIXCt
.set_time <- function(sfile = "", begin.time = NULL, end.time = NULL){
  s_dat <- utils::read.table(sfile,
                             header = FALSE,
                             sep = "\n",
                             quote = "",
                             comment.char = "",
                             stringsAsFactors = FALSE,
                             blank.lines.skip = FALSE,
                             col.names = c("dat")
  )
  if(!is.null(begin.time)){
    # convert string to POSIXCt, tz = "GMT" to avoid time zone effect
    start_t <- try(as.POSIXct(x = begin.time,
                              tz = "GMT",
                              tryFormats = c(
                                "%Y-%m-%d %H:%M:%S",
                                "%Y/%m/%d %H:%M:%S",
                                "%d.%m.%Y %H:%M:%S",
                                "%Y-%m-%d %H:%M",
                                "%Y/%m/%d %H:%M",
                                "%d.%m.%Y %H:%M",
                                "%Y-%m-%d",
                                "%Y/%m/%d",
                                "%d.%m.%Y")
    ),
    silent = FALSE
    )
    if (class(start_t)[[1]] == "try-error") {
      stop("Cannot regconize time string. Simulation time was not set!")
    }
    start_Y <- paste("BeginYear=",
                     strftime(start_t,format = "%Y", tz = "GMT"),
                     sep = "")
    start_m <- paste("BeginMonth=",
                     strftime(start_t, format = "%m", tz = "GMT"),
                     sep = "")
    start_d <- paste("BeginDay=",
                     strftime(start_t, format = "%d", tz = "GMT"),
                     sep = "")
    start_H <- paste("BeginHour=",
                     strftime(start_t, format = "%H", tz = "GMT"),
                     sep = "")
    start_M <- paste("BeginMinute=",
                     strftime(start_t, format = "%M", tz = "GMT"),
                     sep = "")
    start_S <- paste("BeginSecond=",
                     strftime(start_t, format = "%S", tz = "GMT"),
                     sep = "")
    s_dat$dat <- sub("^BeginYear=[0-9]{1,4}", start_Y, s_dat$dat)
    s_dat$dat <- sub("^BeginMonth=[0-9]{1,2}", start_m, s_dat$dat)
    s_dat$dat <- sub("^BeginDay=[0-9]{1,2}", start_d, s_dat$dat)
    s_dat$dat <- sub("^BeginHour=[0-9]{1,2}", start_H, s_dat$dat)
    s_dat$dat <- sub("^BeginMinute=[0-9]{1,2}", start_M, s_dat$dat)
    s_dat$dat <- sub("^BeginSecond=[0-9]{1,2}", start_S, s_dat$dat)
  }
 
  if(!is.null(begin.time)){
    end_t <- try(as.POSIXct(x = end.time,
                            tz = "GMT",
                            tryFormats = c(
                              "%Y-%m-%d %H:%M:%S",
                              "%Y/%m/%d %H:%M:%S",
                              "%d.%m.%Y %H:%M:%S",
                              "%Y-%m-%d %H:%M",
                              "%Y/%m/%d %H:%M",
                              "%d.%m.%Y %H:%M",
                              "%Y-%m-%d",
                              "%Y/%m/%d",
                              "%d.%m.%Y")
    ),
    silent = FALSE
    )
    if (class(end_t)[[1]] == "try-error") {
      stop("Cannot regconize time string. Simulation time was not set!")
    }
    if (!is.null(begin.time)){
      check_time <- grep(" [-0][0-9]*", difftime(end_t, start_t, tz = "GMT"))
      if (length(check_time) > 0){
        stop("new.end must be a moment after new.begin")
      }
    }
    end_Y <- paste("EndYear=",
                   strftime(end_t, format = "%Y", tz = "GMT"),
                   sep = "")
    end_m <- paste("EndMonth=",
                   strftime(end_t, format = "%m", tz = "GMT"),
                   sep = "")
    end_d <- paste("EndDay=",
                   strftime(end_t, format = "%d", tz = "GMT"),
                   sep = "")
    end_H <- paste("EndHour=",
                   strftime(end_t, format = "%H", tz = "GMT"),
                   sep = "")
    end_M <- paste("EndMinute=",
                   strftime(end_t, format = "%M", tz = "GMT"),
                   sep = "")
    end_S <- paste("EndSecond=",
                   strftime(end_t, format = "%S", tz = "GMT"),
                   sep = "")
    s_dat$dat <- sub("^EndYear=[0-9]{1,4}", end_Y, s_dat$dat)
    s_dat$dat <- sub("^EndMonth=[0-9]{1,2}", end_m, s_dat$dat)
    s_dat$dat <- sub("^EndDay=[0-9]{1,2}", end_d, s_dat$dat)
    s_dat$dat <- sub("^EndHour=[0-9]{1,2}", end_H, s_dat$dat)
    s_dat$dat <- sub("^EndMinute=[0-9]{1,2}", end_M, s_dat$dat)
    s_dat$dat <- sub("^EndSecond=[0-9]{1,2}", end_S, s_dat$dat)
  }
  write.table(s_dat$dat,
    file = sfile,
    sep = "\n",
    quote = FALSE,
    col.names = FALSE,
    row.names = FALSE
  )
}


#' Create new Sobek Case from existing Case
#' @param old.name Name of the old case, case-sensitive
#' @param new.name Name of the new case
#' @param sobek.project Sobek Project folder
#' @param new.begin New simulation starting time (in format dd.mm.yyyy hh:mm:ss)
#' @param new.end New simulation starting time (in format dd.mm.yyyy hh:mm:ss)
#' @param new.desc Character vector, for new case description, one element for each line
#' @return A Sobek Case
#' @export
create_case <- function(
                        old.name = "",
                        new.name = "",
                        sobek.project = NULL,
                        new.begin = NULL,
                        new.end = NULL,
                        new.desc = NULL) {

  # check SOBEK project
  sobek_cmt <- paste(sobek.project, "caselist.cmt", sep = "/")
  if (!dir.exists(sobek.project)) {
    stop("Sobek Folder ", sobek.project, " does not exist!")
  }
  # reading SOBEK caselist.cmt
  sobek_clist <- fread(
    file = sobek_cmt,
    header = FALSE,
    sep = " ",
    quote = "'",
    strip.white = FALSE,
    encoding = 'Latin-1',
    stringsAsFactors = FALSE,
    blank.lines.skip = TRUE,
    col.names = c("case_number", "case_name")
  )
  sobek_clist[, case_name := gsub('"', '', case_name, fixed = TRUE)]
  if (old.name %in% sobek_clist$case_name) {
    if (new.name %in% sobek_clist$case_name) stop("Case: ", new.name, " existed!")

    old_folder <- sobek_clist$case_number[sobek_clist$case_name == old.name]
    new_folder <- max(sobek_clist$case_number) + 1

    while (TRUE) {
      if (dir.exists(paste(sobek.project, new_folder, sep = "/"))) {
        new_folder <- new_folder + 1
      } else {
        break
      }
    }
    # make a copy of old case folder
    dir.create(paste(sobek.project, new_folder, sep = "/"))
    file.copy(
      from = list.files(paste(sobek.project, "/", old_folder, "/", sep = ""),
                        full.names = TRUE),
      to = paste(sobek.project, "/", new_folder, "/", sep = ""),
      recursive = TRUE,
      overwrite = FALSE
    )
    # update settings.dat
    if (!is.null(new.begin) && !is.null(new.end)) {
      file.copy(from = paste(sobek.project, old_folder, "settings.dat", sep = "/"),
                to = paste(sobek.project, "/", new_folder, "/", sep = ""))
      setting_dat <- paste(sobek.project, new_folder, "settings.dat", sep = "/")
      try_settime <- try(.set_time(sfile = setting_dat, begin = new.begin, end = new.end),
                         silent = FALSE)
      if (class(try_settime) == "try-error"){
        unlink(paste(sobek.project, new_folder, sep = "/", force = TRUE))
        stop('wrong time input')
      }
    }
    # update casedesc.cmt
    cdesc <- utils::read.table(
      file = paste(sobek.project, old_folder, "casedesc.cmt", sep = "/"),
      header = FALSE,
      sep = "\n",
      quote = "",
      comment.char = "",
      stringsAsFactors = FALSE,
      blank.lines.skip = FALSE,
      col.names = c("old")
    )
    # "SUFHYD 1" is the line of casedesc.cmt right after the case description
    cdesc_begin <- which(cdesc$old == "SUFHYD 1")
    new_cdesc <- paste(sobek.project, "/", new_folder,
    									 "/", "casedesc.cmt", sep = "")

    write.table(
      paste("#", new.name, sep = ""),
      file = new_cdesc,
      quote = FALSE,
      col.names = FALSE,
      row.names = FALSE,
      sep = ""
    )
    if (class(new.desc) == "character") {
      write.table(
        "#Case description:",
        file = new_cdesc,
        append = TRUE,
        quote = FALSE,
        col.names = FALSE,
        row.names = FALSE,
        sep = ""
      )
      write.table(
        paste("#", new.desc, sep = ""),
        file = new_cdesc,
        append = TRUE,
        quote = FALSE,
        col.names = FALSE,
        row.names = FALSE,
        sep = ""
      )
    } else {
      if (!is.null(new.desc)) {
        warning(
          "For case: ",
          new.name,
          " Case description was not written,
          new.desc must be a character vector"
        )
      }
      write.table(cdesc[2:(cdesc_begin-1),],
        file = new_cdesc,
        append = TRUE,
        quote = FALSE, col.names = FALSE,
        row.names = FALSE, sep = ""
      )
    }
    cdesc$old <- gsub(
      paste("\\\\", old_folder, "\\\\", sep = ""),
      paste("\\\\", new_folder, "\\\\", sep = ""),
      cdesc$old
    )
    write.table(
      cdesc$old[cdesc_begin:length(cdesc$old)],
      file = new_cdesc,
      append = TRUE,
      quote = FALSE,
      col.names = FALSE,
      row.names = FALSE,
      sep = ""
    )
    # update caselist.cmt
    write.table(
      paste(new_folder, " ", "\'", new.name, "\'", sep = ""),
      file = sobek_cmt,
      append = TRUE,
      quote = FALSE,
      col.names = FALSE,
      row.names = FALSE,
      sep = ""
    )
    # update register.cmt
    fc_reg <- paste(sobek.project, "register.cmt", sep = "/")
    file.copy(
      from = fc_reg,
      to = paste(fc_reg, ".bk", sep = ""),
      overwrite = TRUE
    )
    c_reg <- data.table::fread(file = fc_reg,
                               header = FALSE,
                               strip.white = FALSE,
                               encoding = 'Latin-1',
                               sep = "\n")
    n_files <- as.integer(c_reg$V1[1])
    case_flist <- list.files(path = paste(sobek.project, new_folder, sep = "/"),
    												 full.names = FALSE)
    case_flist <- case_flist[tolower(case_flist) != "casedesc.cmt"]
    case_flist <- paste("1 ..\\", new_folder, "\\", case_flist, sep = "")
    n_files <- n_files + length(case_flist)
    data.table::fwrite(as.list(n_files), file = fc_reg,
    									 sep = "\n",
    									 append = FALSE,
    									 row.names = FALSE,
    									 col.names = FALSE,
    									 quote = FALSE)
    data.table::fwrite(c_reg[2:.N, ], file = fc_reg,
    									 sep = "\n",
    									 append = TRUE,
    									 row.names = FALSE,
    									 col.names = FALSE,
    									 quote = FALSE)
    data.table::fwrite(as.list(case_flist), file = fc_reg,
    									 sep = "\n",
    									 append = TRUE,
    									 row.names = FALSE,
    									 col.names = FALSE,
    									 quote = FALSE)
  } else {
    stop("Case ", old.name, " does not found in caselist.cmt")
  }
}


#' This function change the output time steps
#' @param case.list List of cases to change
#' @param sobek.project Path to Sobek project
#' @param n.tstep An integer - Number of time step to export result
#' @export
change_output_ntstep <- function(
  case.list = NULL,
  sobek.project = NULL,
  n.tstep = NULL)
{
  n.tstep <- as.integer(n.tstep)
  if (!is.integer(n.tstep)) stop("n.tstep must be an integer")
  if (!is.vector(case.list)) stop('case.list must be a vector/list')
  for (i in unlist(case.list)) {
    sfile <- get_file_path(case.name = i,
                           sobek.project = sobek.project,
                           type = 'setting')
    s_dat <- utils::read.table(sfile,
                               header = FALSE,
                               sep = "\n",
                               quote = "",
                               comment.char = "",
                               stringsAsFactors = FALSE,
                               blank.lines.skip = FALSE,
                               col.names = c("dat")
    )
    s_dat$dat <- sub("^NrOfTimesteps=[0-9]{1,}",
                     paste("NrOfTimesteps=", n.tstep, sep = ""), s_dat$dat)
    try_set <- try(write.table(s_dat$dat,
                                           file = sfile,
                                           sep = "\n",
                                           quote = FALSE,
                                           col.names = FALSE,
                                           row.names = FALSE
    ))
    if (class(try_set) == "try-error"){
      warning(paste('writing output for case:', i, 'not successful!'))
    }
  }
}


#' Rename a Sobek Case
#' @param old.name Name of the old case, case-sensitive
#' @param new.name Name of the new case
#' @param new.begin New simulation starting time (in format dd.mm.yyyy hh:mm:ss)
#' @param new.end New simulation starting time (in format dd.mm.yyyy hh:mm:ss)
#' @param new.desc Character vector, for new case description, one element for each line
#' @param sobek.project Sobek Project folder
#' @return A Sobek Case
#' @export
change_case_info <- function(
  old.name = NULL,
  new.name = NULL,
  new.begin = NULL,
  new.end = NULL,
  new.desc = NULL,
  sobek.project = NULL) {
  stopifnot(!is.null(old.name) & !is.null(sobek.project))
  # check SOBEK project
  sobek_cmt <- paste(sobek.project, "caselist.cmt", sep = "/")
  if (!dir.exists(sobek.project)) {
    stop("Sobek Folder ", sobek.project, " does not exist!")
  }
  # reading SOBEK caselist.cmt
  sobek_clist <- fread(
    file = sobek_cmt,
    header = FALSE,
    sep = " ",
    quote = "'",
    strip.white = FALSE,
    encoding = 'Latin-1',
    stringsAsFactors = FALSE,
    blank.lines.skip = TRUE,
    col.names = c("case_number", "case_name")
  )
  sobek_clist[, case_name := gsub('"|\'', '', case_name)]
  if (!old.name %in% sobek_clist$case_name) stop('case name ', old.name, 'not found' )
  case_folder <- paste(sobek.project, 
                      sobek_clist[case_name == old.name, case_number],
                      sep = "/") 
    # changing time
  if (!is.null(new.begin) | !is.null(new.end)) {
    setting_dat <-
      paste(case_folder, "settings.dat", sep = "/")
    .set_time(sfile = setting_dat,
              begin.time = new.begin,
              end.time = new.end)
    
  }
  # changing case description
  if (!is.null(new.name) | !is.null(new.desc)) {
    cdesc <- utils::read.table(
      file = paste(case_folder, "casedesc.cmt", sep = "/"),
      header = FALSE,
      sep = "\n",
      quote = "",
      comment.char = "",
      stringsAsFactors = FALSE,
      blank.lines.skip = FALSE,
      col.names = c("old")
    )
    # "SUFHYD 1" is the line of casedesc.cmt right after the case description
    cdesc_begin <- which(cdesc$old == "SUFHYD 1")
    new_cdesc <- paste(case_folder,
                       "/", "casedesc.cmt", sep = "")
    if (!is.null(new.name)){
      if (new.name %in% sobek_clist$case_name) stop("Case: ", new.name, " existed!")
      # update caselist.cmt
      sobek_clist[case_name == old.name, case_name := new.name]
      sobek_clist[, clist_out := paste(case_number, " ", "'", case_name, "'",
                                       sep = "")]
      write.table(
        sobek_clist[, c('clist_out')],
        file = sobek_cmt,
        append = FALSE,
        quote = FALSE,
        col.names = FALSE,
        row.names = FALSE,
        sep = ""
      )
      write.table(
        paste("#", new.name, sep = ""),
        file = new_cdesc,
        quote = FALSE,
        append = FALSE,
        col.names = FALSE,
        row.names = FALSE,
        sep = ""
      )
    } else{
      write.table(
        paste("#", old.name, sep = ""),
        file = new_cdesc,
        quote = FALSE,
        append = FALSE,
        col.names = FALSE,
        row.names = FALSE,
        sep = ""
      )
    }
    if (class(new.desc) == "character") {
      write.table(
        "#Case description:",
        file = new_cdesc,
        append = TRUE,
        quote = FALSE,
        col.names = FALSE,
        row.names = FALSE,
        sep = ""
      )
      write.table(
        paste("#", new.desc, sep = ""),
        file = new_cdesc,
        append = TRUE,
        quote = FALSE,
        col.names = FALSE,
        row.names = FALSE,
        sep = ""
      )
    } else {
      if (!is.null(new.desc)) {
        warning(
          "For case: ",
          new.name,
          " Case description was not written,
          new.desc must be a character vector"
        )
      }
      write.table(
        cdesc[2:(cdesc_begin - 1), ],
        file = new_cdesc,
        append = TRUE,
        quote = FALSE,
        col.names = FALSE,
        row.names = FALSE,
        sep = ""
      )
    }
    write.table(
      cdesc$old[cdesc_begin:length(cdesc$old)],
      file = new_cdesc,
      append = TRUE,
      quote = FALSE,
      col.names = FALSE,
      row.names = FALSE,
      sep = ""
    )
  }
}


buil_case_list <- function(
  sobek.project
) {
  cmt_f <- file.path(sobek.project, 'caselist.cmt')
  case_folder_list <- list.files(sobek.project, full.names = TRUE, 
                                 pattern = "\\d$", recursive = FALSE)[-1]
  case_names <- vector()
  for (i in seq_along(case_folder_list)) {
    cmt_f <- file.path(case_folder_list[[i]], 'casedesc.cmt')
    if (file.exists(cmt_f)) {
      cmt <- fread(file = cmt_f, 
                   strip.white = FALSE,
                   encoding = 'Latin-1',
                   header = FALSE, sep = '\n', nrows = 1)[1]
      cmt <- sub('^#', '', cmt)
    } else {
      cmt <- paste0('Unknown_case_name', i)
    }
    case_names[i] <- cmt
  }
}


#' Create new cases with new boundary.dat and lateral.dat from one old case
#' 
#' This function tries to find VGF and HWE in new case names. An error with  be raised if there is no correct information found.
#' 
#' @param old.name Name of the template case
#' @param new.name Names of the new cases
#' @param hwe.tbl Table of HWE (hwe, begin, end)
#' @param sobek.project Path to sobek.project
#' @param new.desc New case description, must be same length as new.name
#' @param id.file Path to ID file
#' @param data.file Path to data file
#' @param bnd.header Path to boundary header
#' @param lat.header Path to lateral header
#' @export
create_case_vgf <- function(
  old.name,
  new.name,
  hwe.tbl = hwe_tbl,
  sobek.project,
  new.desc = new.name,
  id.file,
  data.file,
  bnd.header,
  lat.header
) {
  for (i in seq_along(new.name)) {
    vgf <- str_match_all(new.name[i], '_(\\d{4})_')
    if (length(vgf) > 1 | is.na(vgf[[1]][2])) {
      stop('Wrong format with VGF for case: ', new.name[i])
    }
    vgf <- as.numeric(vgf[[1]][2]) / 1000
    hwe_year <- str_match(new.name[i], '_HW(\\d{4})_')[, 2]
    if (is.na(hwe_year)) {
      stop('Wrong format with HWE for case: ', new.name[i])
    }
    new_begin <- hwe.tbl[hwe == hwe_year, begin]
    new_end <- hwe.tbl[hwe == hwe_year, end]
    create_case(
      old.name = old.name,
      new.name = new.name[i],
      sobek.project = sobek.project,
      new.desc = new.desc[i],
      new.begin = new_begin,
      new.end = new_end
    )
    # create dat
    create_dat(
      id.file = id.file,
      data.file = data.file,
      bnd.header = bnd.header,
      lat.header = lat.header,
      factor = vgf,
      output.case = c(new.name[i], sobek.project)
    )
  }
}