#' This function changes the simulation start and ending time in a sobek setting
#' The input time is an POSIXCt or an object that is coercible to POSIXCt
set_time <- function(sfile = "", begin.time = NULL, end.time = NULL){
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
    start_Y <- paste0("BeginYear=",
                     strftime(start_t,format = "%Y", tz = "GMT"))
    start_m <- paste0("BeginMonth=",
                     strftime(start_t, format = "%m", tz = "GMT"))
    start_d <- paste0("BeginDay=",
                     strftime(start_t, format = "%d", tz = "GMT"))
    start_H <- paste0("BeginHour=",
                     strftime(start_t, format = "%H", tz = "GMT"))
    start_M <- paste0("BeginMinute=",
                     strftime(start_t, format = "%M", tz = "GMT"))
    start_S <- paste0("BeginSecond=",
                     strftime(start_t, format = "%S", tz = "GMT"))
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
    end_Y <- paste0("EndYear=",
                   strftime(end_t, format = "%Y", tz = "GMT"))
    end_m <- paste0("EndMonth=",
                   strftime(end_t, format = "%m", tz = "GMT"))
    end_d <- paste0("EndDay=",
                   strftime(end_t, format = "%d", tz = "GMT"))
    end_H <- paste0("EndHour=",
                   strftime(end_t, format = "%H", tz = "GMT"))
    end_M <- paste0("EndMinute=",
                   strftime(end_t, format = "%M", tz = "GMT"))
    end_S <- paste0("EndSecond=",
                   strftime(end_t, format = "%S", tz = "GMT"))
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
#'
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
  sobek_clist[, case_name := remove_dbl_quote(case_name)]
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
      from = list.files(paste0(sobek.project, "/", old_folder),
                        full.names = TRUE),
      to = paste0(sobek.project, "/", new_folder, "/"),
      recursive = TRUE,
      overwrite = FALSE
    )
    # update settings.dat
    if (!is.null(new.begin) && !is.null(new.end)) {
      file.copy(from = paste(sobek.project, old_folder, "settings.dat", sep = "/"),
                to = paste0(sobek.project, "/", new_folder, "/"))
      setting_dat <- paste(sobek.project, new_folder, "settings.dat", sep = "/")
      try_settime <- try(set_time(sfile = setting_dat, begin = new.begin, end = new.end),
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
    new_cdesc <- paste0(sobek.project, "/", new_folder, "/casedesc.cmt")

    write.table(
      paste0("#", new.name),
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
        paste0("#", new.desc),
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
      paste0("\\\\", old_folder, "\\\\"),
      paste0("\\\\", new_folder, "\\\\"),
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
      paste0(new_folder, " ", "\'", new.name, "\'"),
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
      to = paste0(fc_reg, ".bk"),
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
    case_flist <- paste0("1 ..\\", new_folder, "\\", case_flist)
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
                     paste0("NrOfTimesteps=", n.tstep), s_dat$dat)
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


#' Change Sobek case information
#'
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
    set_time(sfile = setting_dat,
              begin.time = new.begin,
              end.time = new.end)

  }
  # changing case description
  if (!is.null(new.name) | !is.null(new.desc)) {
    cdesc <- utils::read.table(
      file = paste0(case_folder, "/casedesc.cmt"),
      header = FALSE,
      sep = "\n",
      quote = "",
      comment.char = "",
      stringsAsFactors = FALSE,
      blank.lines.skip = FALSE,
      col.names = c("old")
    )
    # "SUFHYD 1" is the line of casedesc.cmt right after the case description
    cdesc_begin <- grep("^SUFHYD \\d{1}", cdesc$old, ignore.case = TRUE)
    new_cdesc <- paste0(case_folder, "/casedesc.cmt")
    if (!is.null(new.name)){
      if (new.name %in% sobek_clist$case_name) stop("Case: ", new.name, " existed!")
      # update caselist.cmt
      sobek_clist[case_name == old.name, case_name := new.name]
      sobek_clist[, clist_out := paste0(case_number, " '", case_name, "'",)]
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
        paste0("#", new.name),
        file = new_cdesc,
        quote = FALSE,
        append = FALSE,
        col.names = FALSE,
        row.names = FALSE,
        sep = ""
      )
    } else{
      write.table(
        paste0("#", old.name),
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
        paste0("#", new.desc),
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


build_case_list <- function(
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


