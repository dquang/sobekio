#' Export list of cases from a sobek project to another location
#' @param case.list Path to file with case list
#' @param sobek.project Path to current sobek project
#' @param dest Destination folder
#' @param copy.his Should .HIS file be copied? Default TRUE
#' @param his.only Copy only HIS/HIA files for saving time?
#' @export
sobek_export <- function(case.list,
                         sobek.project,
                         dest,
                         copy.his = TRUE,
                         his.only = FALSE
                         ){
  if (!dir.exists(dest)) dir.create(dest)
  if (is.character(case.list[[1]]) & file.exists(case.list[[1]])) {
    case.list <-  data.table(read.table(case.list,
                                        header = FALSE, sep = "\n",
                                        encoding = "windows-1252",
                                        stringsAsFactors = FALSE
    )
    )
    case.list <- case.list$V1
  }
  sobek_cmt <- file_path(name = 'caselist.cmt', path = sobek.project)
  sobek_clist <- fread(file = sobek_cmt,
                       header = FALSE,
                       sep = " ",
                       quote = "'",
                       col.names = c("case_number", "case_name")
  )
  sobek_clist[, case_name := gsub('"', '', case_name, fixed = TRUE)]
  setkey(sobek_clist, case_name)
  all_files <- dir(path = sobek.project,
                   pattern = "^[^0-9]{1,}$",
                   full.names = TRUE, no.. = TRUE)
  file.copy(from = all_files, to = dest, recursive = TRUE)
  for (i in case.list) {
    if (substr(i, 1, 3) == "---") next
    cat('copying case:', i, "...\n")
    from_folder <- dirname(get_file_path(
      case.name = i,
      sobek.project = sobek.project,
      type = 'bnd.dat'
    ))
    if (isTRUE(his.only)) {
      # export only HIS, HIA for reading result
      case_folder <- basename(from_folder)
      dest_folder <- paste(dest, case_folder, sep = "/")
      dir.create(dest_folder)
      case_files <- dir(path = from_folder,
                        full.names = TRUE,
                        no.. = TRUE)
      case_files <- case_files[grepl("\\.his$|\\.hia$", case_files,
                                     # fixed = TRUE,
                                     ignore.case = TRUE)]
      file.copy(case_files, dest_folder)
    } else{
      if (!isTRUE(copy.his)) {
        # do not copy .HIS file, saving disk space and time
        case_folder <- basename(from_folder)
        dest_folder <- paste(dest, case_folder, sep = "/")
        dir.create(dest_folder)
        case_files <- dir(path = from_folder,
                          full.names = TRUE,
                          no.. = TRUE)
        case_files <- case_files[!grepl("\\.his$", case_files,
                                        # fixed = TRUE,
                                        ignore.case = TRUE)]
        file.copy(case_files, dest_folder)
      } else{
        # copy all files - full export
        file.copy(from = from_folder,
                  to = dest,
                  recursive = TRUE)
      }
    }
  }
  # update register.cmt
  sobek_reg <- fread(file = paste(sobek.project, "register.cmt", sep = "\\"),
                     header = FALSE, sep = "\n", fill = FALSE,
                     stringsAsFactors = FALSE
                     )
  case_folders <- sobek_clist[case.list, case_number]
  for (i in case_folders) {
    sobek_reg <- sobek_reg[!grepl(paste("\\\\", i, "\\\\", sep = ""),
                                  V1)]
  }
  sobek_reg[1,1] <- as.character(length(sobek_reg$V1) - 1)
  case_cmt <- data.table(case_folders, case.list)
  cat('updating register...\n')
  case_cmt[, case.list := paste("'", case.list, "'", sep = "")]
  write.table(case_cmt,
         file = paste(dest, "caselist.cmt", sep = "\\"),
         quote = FALSE,
         col.names = FALSE,
         row.names = FALSE,
         fileEncoding = "windows-1252",
         sep = " ")
  fwrite(sobek_reg,
         file = paste(dest, "register.cmt", sep = "\\"),
         col.names = FALSE,
         quote = FALSE
         )
}


new_case_number <- function(old, n = 1){
  old <- unique(old)
  tsd_nr <- seq.int(1, max(old) + n + 1, 1)
  tsd_nr[!tsd_nr %in% old][1:n]
}

#' Copy and merge sobek cases from different projects
#'
#' This function copies a list of cases from different sobek projects
#' and merge them into one destination project. The destination can be a new or an existing project
#'
#' @param cases List of cases. This parameter must be given as a list
#' @param projects Character vector of path to projects
#' @param prefix Character vector of prefix, that will be prefixed to the case names, in order to avoid naming conflict. This parameter is optional, however, if naming conflict occurs the function will stop
#' @param dest Path to destination project.
#' @param do.par Parallel copy files for cases
#' @param as.suffix Using prefix as suffix
#' @examples
#' \dontrun{
#' sobek_copy(
#'   cases = list(
#'     from_prj_1 = c('case 1', 'case 2'),
#'     from_prj_2 = c('case 2', 'case 3')
#'   ),
#'   projects = c('path_to_prj_1', 'path_to_prj_2'),
#'   dest = 'd:/so21503/new.lit'
#' )
#' }
#' @export
sobek_copy <- function(
  cases,
  projects,
  prefix = NULL,
  dest,
  do.par = FALSE,
  as.suffix = FALSE
) {
  do.par <- isTRUE(do.par)
  as.suffix <- isTRUE(as.suffix)
  stopifnot(is.vector('projects', mode = 'character'))
  # checking input folders
  for (a_p in projects) {
    stopifnot(dir.exists(a_p))
    stopifnot(file.exists(file.path(a_p, 'caselist.cmt')))
  }
  stopifnot(typeof(cases) == 'list')
  if (length(cases) != length(projects)) {
    if (length(cases) == 1) {
      cases <- rep(cases, length(projects))
    } else {
      stop('cases must have length 1 or same length as projects')
    }
  }
  if (!is.null(prefix)) {
    if (length(prefix) == 1) prefix <- rep(prefix, length(cases))
    if (length(prefix) != 1 & (length(cases) != length(prefix))) stop('wrong prefix')
  }
  # check destination
  is_new <- TRUE
  if (dir.exists(dest)) {
    if (file.exists(file.path(dest, 'caselist.cmt'))) {
      is_new <- FALSE
    } else {
      stop('A name for destination project should be given. Your dest = ', dest,
           ' is already exists but is not a sobek project folder')
    }
  }
  # building input table for case
  cmt_tbl_list <- list()
  for (i in seq_along(projects)) {
    from_cmt <- file.path(projects[[i]], 'caselist.cmt')
    this_cmt <- fread(
      from_cmt,
      sep = ' ',
      quote = "'",
      header = FALSE,
      col.names = c('case_number', 'case_name'),
      blank.lines.skip = TRUE,
      encoding = 'Latin-1',
      key = 'case_name',
      colClasses = c('integer', 'character')
    )
    this_cmt <- this_cmt[case_name %in% cases[[i]]]
    this_cmt$prj_path <- projects[[i]]
    this_cmt$prj_id <- i
    this_cmt[, case_folder := file.path(prj_path, case_number)]
    prefix_i <- prefix[[i]]
    if (!is.null(prefix_i)) {
      if (as.suffix) {
        this_cmt[, case_name_new := paste0(case_name, prefix_i)]
      } else {
        this_cmt[, case_name_new := paste0(prefix_i, case_name)]
      }
    } else {
      this_cmt[, case_name_new := case_name]
    }
    cmt_tbl_list[[i]] <- this_cmt
  }
  cmt_tbl <- rbindlist(cmt_tbl_list)
  n_case <- nrow(cmt_tbl)
  if (n_case != length(unique(cmt_tbl$case_name_new))) stop('Naming conflict, check prefix')
  # checking uniqueness of case.name
  if (length(cmt_tbl$case_name_new) != length(cmt_tbl$case_name_new)) {
    stop('List of case.names for destination is not unique. Check prefix')
  }
  if (!is_new) {
    cmt_new <- fread(
      file.path(dest, 'caselist.cmt'),
      sep = ' ',
      quote = "'",
      header = FALSE,
      col.names = c('case_number_new', 'case_name_new'),
      blank.lines.skip = TRUE,
      encoding = 'Latin-1',
      key = 'case_name_new',
      colClasses = c('integer', 'character')
    )
    case_conflict_list <- vector(mode = 'integer')
    for (i in seq_along(cmt_tbl$case_name_new)) {
      if (cmt_tbl$case_name_new[[i]] %in% cmt_new$case_name_new) {
        case_conflict_list[i] <- cmt_tbl$case_name_new[[i]]
      }
    }
    if (length(case_conflict_list) > 0) {
      cat(case_conflict_list, sep = '\n')
      stop('cases listed above are already in the destination project. Please check prefix!\n')
    }
    # list all case folders in destination sobek project
    ext_folders <- as.integer(
      list.files(dest, full.names = FALSE, pattern = '^\\d+'))
    cmt_tbl$case_number_new <-
      new_case_number(c(cmt_new$case_number, ext_folders), n_case)
    cmt_new <- rbind(cmt_new,
                     cmt_tbl[, .(case_number_new, case_name_new)],
                     use.names = TRUE)
  } else {
    cmt_tbl$case_number_new <- seq.int(1, n_case, 1)
    cmt_new <- cmt_tbl[, .(case_number_new, case_name_new)]
  }
  cmt_new[, case_name_new := paste0("'", case_name_new, "'")]
  cmt_tbl[, case_folder_new := file.path(dest, case_number_new)]
  reg_list <- list()
  # now we update register
  for (i in seq_along(projects)) {
    reg_file <- file.path(projects[[i]], 'register.cmt')
    this_reg <- fread(file = reg_file,
                      header = FALSE, sep = "\n", fill = FALSE,
                      stringsAsFactors = FALSE
    )
    this_reg$prj_id <- i
    this_reg[, case_number := stri_match_first_regex(V1, '\\.\\\\(\\d+)\\\\')[, 2]]
    this_old_nr <- cmt_tbl[prj_id == i, case_number]
    this_new_nr <- cmt_tbl[prj_id == i, case_number_new]
    this_reg[, to_keep := 0]
    this_reg[case_number %in% this_old_nr, to_keep := 1]
    this_reg[is.na(case_number), to_keep := 1]
    this_reg <- this_reg[to_keep == 1]
    for (j in seq_along(this_old_nr)) {
      old_pat <- paste0("..\\", this_old_nr[j], "\\")
      new_pat <- paste0("..\\", this_new_nr[j], "\\")
      this_reg[, V1 := stri_replace_first_fixed(V1, pattern = old_pat,
                                                replacement = new_pat)]
    }
    # remove the first line that stores total data lines, we make it later
    reg_list[[i]] <- this_reg[-1]
  }
  new_reg <- rbindlist(reg_list)[, .(V1, case_number)]
  if (!is_new) {
    reg_file <- file.path(dest, 'register.cmt')
    if (file.exists(reg_file)) {
      file.copy(reg_file, paste0(reg_file, '.bk'))
      old_reg <- fread(file = reg_file,
                       header = FALSE, sep = "\n", fill = FALSE,
                       stringsAsFactors = FALSE
      )
      old_reg[, case_number := stri_match_first_regex(V1, '\\.\\\\(\\d+)\\\\')[, 2]]
      new_reg <- rbind(old_reg[-1], new_reg[-1])
    }
  } else {
    stopifnot(dir.create(dest))
  }
  setorder(new_reg, case_number)
  new_reg_count <- nrow(new_reg)
  new_reg[, case_number := NULL]
  new_reg <- unique(new_reg)
  new_reg <- rbind(data.table(V1 = new_reg_count), new_reg)
  # now writing results
  # writing caselist.cmt
  cmt_file <- file.path(dest, 'caselist.cmt')
  if (file.exists(cmt_file)) file.copy(cmt_file, paste0(cmt_file, '.bk'), overwrite = TRUE)
  reg_file <- file.path(dest, 'register.cmt')
  if (file.exists(reg_file)) file.copy(reg_file, paste0(reg_file, '.bk'), overwrite = TRUE)
  # changing case description
  cat('Copying files for cases, it will take probably a long time...\n')
  if (do.par) {
    doParallel::registerDoParallel(parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    foreach::foreach(i = 1:length(cmt_tbl$case_name_new)) %dopar% {
      old_cmt <- file.path(cmt_tbl[i, case_folder], 'casedesc.cmt')
      new_cmt <-  file.path(cmt_tbl[i, case_folder_new], 'casedesc.cmt')
      case_cmt <- fread(
        file = old_cmt,
        header = FALSE,
        quote = '',
        sep = '\n'
      )
      # change case name
      new_name <- paste0('#', cmt_tbl[i, case_name_new])
      case_cmt[1, V1 := new_name]
      # change case number
      old_pat <- paste0("..\\", cmt_tbl[i, case_number], "\\")
      new_pat <- paste0("..\\", cmt_tbl[i, case_number_new], "\\")
      case_cmt[, V1 := stri_replace_first_fixed(V1, pattern = old_pat,
                                                replacement = new_pat)]
      old_files <- list.files(path = cmt_tbl[i, case_folder])
      new_files <- file.path(cmt_tbl[i, case_folder_new], old_files)
      old_files <- file.path(cmt_tbl[i, case_folder], old_files)
      dir.create(cmt_tbl[i, case_folder_new])
      file.copy(old_files, new_files)
      fwrite(x = case_cmt, file = new_cmt,
             col.names = FALSE, quote = FALSE,
             append = FALSE)
    }
    doParallel::stopImplicitCluster()
  } else {
    n_case <- nrow(cmt_tbl)
    for (i in seq_along(cmt_tbl$case_name_new)) {
      old_cmt <- file.path(cmt_tbl[i, case_folder], 'casedesc.cmt')
      new_cmt <-  file.path(cmt_tbl[i, case_folder_new], 'casedesc.cmt')
      case_cmt <- fread(
        file = old_cmt,
        header = FALSE,
        quote = '',
        sep = '\n'
      )
      # change case name
      new_name <- paste0('#', cmt_tbl[i, case_name_new])
      case_cmt[1, V1 := new_name]
      # change case number
      old_pat <- paste0("..\\", cmt_tbl[i, case_number], "\\")
      new_pat <- paste0("..\\", cmt_tbl[i, case_number_new], "\\")
      case_cmt[, V1 := stri_replace_first_fixed(V1, pattern = old_pat,
                                                replacement = new_pat)]
      old_files <- list.files(path = cmt_tbl[i, case_folder])
      new_files <- file.path(cmt_tbl[i, case_folder_new], old_files)
      old_files <- file.path(cmt_tbl[i, case_folder], old_files)
      dir.create(cmt_tbl[i, case_folder_new])
      tot_file <- length(old_files)
      fwrite(x = case_cmt, file = new_cmt,
             col.names = FALSE, quote = FALSE,
             append = FALSE)
      for (j in seq_along(old_files)) {
        file.copy(from = old_files[j], to = new_files[j], overwrite = TRUE)
        cat('Copying files....................',
            str_pad(round(i * j * 100 / tot_file / n_case), 5, 'left'), '%\r'
            )
        flush.console()
      }
    }
  }
  cat('\nCopy all other files of the projects...\n')
  fwrite(cmt_new,
         file = cmt_file,
         quote = FALSE,
         append = FALSE,
         sep = ' ',
         col.names = FALSE
  )
  fwrite(new_reg,
         file = reg_file,
         quote = FALSE,
         append = FALSE,
         col.names = FALSE
  )
  for (p in projects) {
    other_files <- list.files(path = p)
    other_files <- other_files[!grepl('^\\d+', other_files)]
    for (a_f in other_files) {
      file.copy(from = file.path(p, a_f), to = dest, recursive = TRUE, overwrite = FALSE)
    }
  }
  cat('Done.\n')
}


#' Copy cases from one project and overwrite those with same name in another
#'
#' @param cases Character vector of cases
#' @param from Path to source project
#' @param to Path to destination project
#' @export
sobek_overwrite <- function(
  cases,
  from,
  to
) {
  stopifnot(is.vector(cases, mode = 'character'))
  if (length(cases) != length(unique(cases))) warning('cases is not unique')
  cases <- unique(cases)
  # check destination
  from_cmt_f <- file.path(from, 'caselist.cmt')
  to_cmt_f <- file.path(to, 'caselist.cmt')
  stopifnot(file.exists(from_cmt_f, to_cmt_f))
  # building input table for case
  from_cmt <- fread(
    from_cmt_f,
    sep = ' ',
    quote = "'",
    header = FALSE,
    col.names = c('case_number', 'case_name'),
    blank.lines.skip = TRUE,
    encoding = 'Latin-1',
    key = 'case_name',
    colClasses = c('integer', 'character')
  )
  to_cmt <- fread(
    to_cmt_f,
    sep = ' ',
    quote = "'",
    header = FALSE,
    col.names = c('case_number', 'case_name'),
    blank.lines.skip = TRUE,
    encoding = 'Latin-1',
    key = 'case_name',
    colClasses = c('integer', 'character')
  )
  from_cmt <- from_cmt[case_name %in% cases]
  if (length(from_cmt$case_name) != length(cases)) {
    cat('Following cases are not in the source project: \n')
    for (a_c in cases) if (!a_c %in% from_cmt$case_name) cat(a_c, '\n')
  }
  stopifnot(nrow(from_cmt) > 0)
  from_cmt[, from_folder := file.path(from, case_number)]
  to_cmt <- to_cmt[case_name %in% from_cmt$case_name]
  stopifnot(nrow(to_cmt) > 0)
  if (nrow(to_cmt) != nrow(from_cmt)) {
    cat('Following cases are not in the destination project: \n')
    for (a_c in to_cmt$case_name) if (!a_c %in% from_cmt$case_name) cat(a_c, '\n')
  }
  to_cmt[, to_folder := file.path(to, case_number)]
  cmt_tbl <- merge(to_cmt[, .(case_name, to_folder)],
                   from_cmt[, .(case_name, from_folder)],
                   by = 'case_name')
  n_case <- nrow(cmt_tbl)
  cat('Copying files, it will take probably a long time...\n')
  for (i in seq.int(1, n_case, 1)) {
    from_files <- list.files(cmt_tbl[i, from_folder], full.names = TRUE)
    tot_file <- length(from_files)
    for (j in seq_along(from_files)) {
      file.copy(from = from_files[j], to = cmt_tbl[i, to_folder], overwrite = TRUE)
      cat('Copying files....................',
          str_pad(round(i * j * 100 / tot_file / n_case), 5, 'left'), '%\r'
      )
      flush.console()
    }
  }
  cat('\nDone.\n')
}

