#----reading Trigger file, generating trig_def----
# trigger_tbl should be one table with V1 is  the ogriginal file
# orig_line_nr is original line number, this serves any changing later
# this function get definition table of triggers (trigger.def)
.get_trigger_def <- function(trigger.def.f = NULL){
  trig_def <- fread(trigger.def.f, sep = "\n", header = FALSE)
  trig_def[, orig_line_nr := .I]
  str_mt <-  str_match(trig_def$V1,
                       "TRGR id '([^']*)' nm '([^']*)'")
  trig_def$id <- str_mt[, 2]
  trig_def$nm <- str_mt[, 3]
  trig_def$ty <- str_match(trig_def$V1, "ty ([0-9]{1}) ")[, 2]
  trig_def$tp <- str_match(trig_def$V1, "tp ([0-9]{1}) ")[, 2]
  trig_def$ml <- str_match(trig_def$V1, " ml '([^']*)' ")[, 2]
  # cumulative sum of the id, i.e. 
  # id takes the value of the first element, grouping by none-NA
  trig_def[, id := id[1], by = .(cumsum(!is.na(id)))]

  return(trig_def)
}


#' Get information table for one trigger
#'
#' This function read important information of a trigger from the project case.
#'
#' @param t.id Id of the trigger
#' @param case.name Name of the case
#' @param sobek.project Path to Sobek project
#' @param html If TRUE, return a html table
#'
#' @return a data.table or a html.table
#' @export
get_trigger_info <- function(t.id, case.name, sobek.project, html = TRUE,
                             tble = TRUE) {
  t_file <- get_file_path(case.name, sobek.project, type = 'trigger')
  trig_def <- .get_trigger_def(t_file)[id == t.id]
  t_type <- switch(trig_def[1, ty],
                   '0' = 'time',
                   '1' = 'hydraulic',
                   '2' = 'combined')
  t_par <- switch(trig_def[1, tp],
                  '0' = 'waterlevel at branch location',
                  '1' = 'head difference over structure',
                  '2' = 'discharge at branch location',
                  '3' = 'gate lower edge level',
                  '4' = 'crest level',
                  '6' = 'crest width',
                  '6' = 'waterlevel in retention area',
                  '7' = 'pressure difference over structure'
                  )
  trig_tbl <- data.table(
    Parameter = c(
      'Trigger ID', 'Trigger name', 'Trigger type', 'Trigger parameter', 
      'Trigger measurement'
    ),
    Value = c(
      trig_def[1, id], trig_def[1, nm], t_type, t_par, trig_def[1, ml]
    )
  )
  if (isTRUE(tble)) {
    trig_tble <- trig_def[grepl(" <$", V1), c('V1')]
    trig_tble[, c('V2', 'V3', 'V4', 'V5', 'V6', 'V7') := 
                tstrsplit(str_trim(V1), split = ' ')]
    trig_tble[V3 == 0, V3 := 'OFF'][V3 == 1, V3 := 'ON']
    trig_tble[V4 == 0, V4 := 'OR'][V4 == 1, V4 := 'AND']
    trig_tble[V5 == 0, V5 := '<'][V5 == 1, V5 := '>']
    trig_tble[, V2 := str_replace_all(V2, "'", "")]
    trig_tble[, V2 := str_replace(V2, ";", " ")]
    trig_tble[, V1 := paste(V2, V3, V4, V5, V6)]
    trig_tble <- trig_tble[, c('V1')]
    colnames(trig_tble) <- 'Value'
    nrow_tble <- nrow(trig_tble)
    trig_tble[, Parameter := c('Trigger table', rep(NA, nrow_tble - 1))]
    trig_tbl <- rbind(trig_tbl, trig_tble)
  }
  if (isTRUE(html)) {
    trig_tbl <- htmlTable::htmlTable(
      trig_tbl,
        align = 'l',
        caption = paste(
          "Information table of the Trigger:", t.id),
        tfoot = paste('Case:', case.name)
    )
  }
  return(trig_tbl)
}


get_trigger_popover <- function(t.id, case.name, sobek.project, html = TRUE,
                             tble = TRUE) {
  t_file <- get_file_path(case.name, sobek.project, type = 'trigger')
  trig_def <- .get_trigger_def(t_file)[id == t.id]
  t_type <- switch(trig_def[1, ty],
                   '0' = 'time',
                   '1' = 'hydraulic',
                   '2' = 'combined')
  t_par <- switch(trig_def[1, tp],
                  '0' = 'waterlevel at branch location',
                  '1' = 'head difference over structure',
                  '2' = 'discharge at branch location',
                  '3' = 'gate lower edge level',
                  '4' = 'crest level',
                  '6' = 'crest width',
                  '6' = 'waterlevel in retention area',
                  '7' = 'pressure difference over structure'
  )
  trig_tbl <- data.table(
    Parameter = c(
      'ID', 'Name', 'Type', 'Parameter', 
      'Measurement'
    ),
    Value = c(
      trig_def[1, id], trig_def[1, nm], t_type, t_par, trig_def[1, ml]
    )
  )
  if (isTRUE(tble)) {
    trig_tble <- trig_def[grepl(" <$", V1), c('V1')]
    trig_tble[, c('V2', 'V3', 'V4', 'V5', 'V6', 'V7') := 
                tstrsplit(str_trim(V1), split = ' ')]
    trig_tble[V3 == 0, V3 := 'OFF'][V3 == 1, V3 := 'ON']
    trig_tble[V4 == 0, V4 := 'OR'][V4 == 1, V4 := 'AND']
    trig_tble[V5 == 0, V5 := '<'][V5 == 1, V5 := '>']
    trig_tble[, V2 := str_replace_all(V2, "'", "")]
    trig_tble[, V2 := str_replace(V2, ";", " ")]
    trig_tble[, V1 := paste(V2, V3, V4, V5, V6)]
    trig_tble <- trig_tble[, c('V1')]
    colnames(trig_tble) <- 'Value'
    nrow_tble <- nrow(trig_tble)
    trig_tble[, Parameter := c('Trigger table', rep(NA, nrow_tble - 1))]
    trig_tbl <- rbind(trig_tbl, trig_tble)
  }
  if (isTRUE(html)) {
    trig_tbl <- htmlTable::htmlTable(
      trig_tbl,
      align = 'l',
      caption = paste(
        "Information table of the Trigger:", t.id),
      tfoot = paste('Case:', case.name)
    )
  }
  return(trig_tbl)
}


#----reading structure data table-----
# str_tbl should be one table with V1 is the ogriginal file
# orig_line_nr is original line number, this serves any changing later
# this function get the table of structures (struct.dat)
.get_struct_dat <- function(struct.dat.f = NULL){
  str_tbl <- fread(struct.dat.f , sep = "\n", header = FALSE)
  str_tbl[, orig_line_nr := .I]
  # get id, name, definitionID
  str_mt <- str_match(
    str_tbl$V1,
    "STRU id '([^']*)' nm '([^']*)' dd '([^']*)'.*stru")
  str_tbl$id <- str_mt[, 2]
  str_tbl$name <- str_mt[, 3]
  str_tbl$def_ID <- str_mt[, 4]
  # get controllers
  str_mt <- str_match(
    str_tbl$V1,
    "STRU id.*ca (\\d \\d \\d \\d) cj ('[^']*' '[^']*' '[^']*' '[^']*').* stru")
  str_tbl$ca <- str_mt[, 2]
  str_tbl$cj <- str_mt[, 3]
  str_tbl[is.na(ca), ca := str_match(V1, " ca (\\d) ")[, 2]]
  str_tbl[is.na(cj), cj := str_match(V1, " cj ('[^']*') ")[, 2]]

  return(str_tbl)
}


#----reading table of structure difinitions (with def_ID)-----
# this function get definition table of structures (struct.def)
.get_struct_def <- function(struct.def.f = NULL){
  str_def <- fread(struct.def.f , sep = "\n", header = FALSE)
  str_def[, orig_line_nr := .I]
  # get the description lines only
  str_def_tbl <- str_def[grepl("^STDS id", V1)]
  # get def_ID, name, type
  str_mt <- str_match(
    str_def_tbl$V1,
    "STDS id '([^']*)' nm '([^']*)' ty (\\d{1,2}).*")
  str_def_tbl$def_ID <- str_mt[, 2]
  str_def_tbl$def_name <- str_mt[, 3]
  str_def_tbl$def_ty <- str_mt[, 4]
  # get crest level, crest/sill width
  str_def_tbl[, cl := as.double(str_match(V1, ' cl (\\d*\\.*\\d*) ')[, 2])]
  str_def_tbl[, cw := as.double(str_match(V1, ' [cs]w (\\d*\\.*\\d*) ')[, 2])]
  # get possible flow direction
  str_mt <- str_match(
    str_def_tbl$V1,
    ".* rt (\\d*\\.*\\d*)\\ .*")
  str_def_tbl$rt <- str_mt[, 2]
  str_def_tbl$V1 <- NULL
  str_def <- merge(str_def, str_def_tbl, by = 'orig_line_nr', all.x = TRUE)
  str_def[, def_ID := def_ID[1], .(cumsum(!is.na(def_ID)))]
  return(str_def)
}


#----reading control.def----
# this function get definition table of controllers (control.def)
.get_control_def <- function(control.def.f = NULL){
  ct_def <- fread(control.def.f, sep = "\n", header = FALSE)
  ct_def[, org_line_nr := .I]
  ct_tbl <- ct_def[grepl('^CNTL id .*', V1)]
  ct_tbl[, id := str_match(V1, "CNTL id '([^']*)'")[,2]]
  ct_tbl[, name := str_match(V1, " nm '([^']*)'")[,2]]
  ct_tbl[, ct := str_match(V1, " ct (\\d) ")[,2]]
  ct_tbl[, ca := str_match(V1, " ca (\\d) ")[,2]]
  ct_tbl[, ac := str_match(V1, " ac (\\d) ")[,2]]
  ct_tbl[, cf := str_match(V1, " cf (\\d{1,}) ")[,2]]
  ct_tbl[, ta := str_match(V1, " ta (\\d \\d \\d \\d) ")[,2]]
  ct_tbl[, gi := str_match(V1, " gi ('.+') ao \\d")[,2]]
  ct_tbl[, ao := str_match(V1, " ao (\\d \\d \\d \\d)")[,2]]
  ct_tbl[, mc := str_match(V1, " mc ([^\\ ]*) ")[,2]]
  ct_tbl[, bl := str_match(V1, " bl (\\d) ")[,2]]
  ct_tbl[, cp := str_match(V1, " cp (\\d) ")[,2]]
  ct_tbl[, mp := str_match(V1, " mp (\\d) ")[,2]]
  ct_tbl[, ml := str_match(V1, " ml '([^']*)' ")[,2]]
  ct_tbl$V1 <- NULL
  ct_def <- merge(ct_def, ct_tbl, by = 'org_line_nr', all.x = TRUE)
  ct_def[, id := id[1], .(cumsum(!is.na(id)))]

  return(ct_def)
}


#' Get information of a controller
#' @param ct.id ID of the controller
#' @param def.file Path to control.def file
#' @param case.name Name of the case (considered if def.file == NULL)
#' @param sobek.project Path to sobek.project (considered if def.file == NULL)
#' @param trigger If TRUE, information about triggers will be given
#' @export
#' @return a list
get_control_info <- function(ct.id = NULL,
                             def.file = NULL,
                             case.name = NULL,
                             sobek.project = NULL,
                             trigger = FALSE,
                             html = TRUE
                             ) {
  
  if (is.null(def.file)) {
    def.file <- get_file_path(case.name, sobek.project, type = 'control.def')
  } else {
    if (isTRUE(trigger)) {
      stopifnot(!is.null(case.name) & !is.null(sobek.project))
    }
  }
  ct_def <- .get_control_def(control.def.f = def.file)
  ct_id_tbl <- ct_def[id == ct.id][1, ]
  ct_info_list <- c(
    'Controller ID' = ct_id_tbl$id,
    'Controller name' = ct_id_tbl$name,
    'Controller type' = .get_ct_type(ct_id_tbl$ct),
    'Controller parameter' = .get_ct_param_type(ct_id_tbl$ca),
    'Controller activated' = ct_id_tbl$ac,
    'Controller measurement' = ct_id_tbl$ml,
    'Measured parameter' = .get_cp_type(ct_id_tbl$cp),
    'Time lag' = ct_id_tbl$mp,
    'Update frequency' = ct_id_tbl$cf,
    'Trigger activated' = ct_id_tbl$ta,
    'Trigger IDs' = ct_id_tbl$gi,
    'dValue/dt' = ct_id_tbl$mc
  )
  ct_tble <- ct_def[id == ct.id & grepl(' < {0,1}$', V1), c('V1')]
  ct_tble[, V1 := str_replace(V1, "< {0,1}$", "")]
  ct_tble[, V1 := str_replace_all(V1, "'", "")]
  ct_tble[, V1 := str_replace(V1, ";", " ")]
  colnames(ct_tble) <- 'Value'
  nrow_tble <- nrow(ct_tble)
  ct_tble[, Parameter := c('Controller table', rep(NA, nrow_tble - 1))]
  ct_info_tbl <- data.table(Parameter = names(ct_info_list),
                            Value = ct_info_list)
  ct_info_tbl <- rbind(ct_info_tbl, ct_tble)
  r_group <- c("Structure Information")
  n_group <- nrow(ct_info_tbl) # Number of rows for "Structure information"
  if (isTRUE(trigger)) {
    trig_all <- str_match(
      ct_info_tbl[Parameter == 'Trigger IDs', Value], 
      "'([^']+)' '([^']+)' '([^']+)' '([^']+)'"
    )[, 2:5]
    trig_all <- trig_all[trig_all != '-1']
    trig_all <- trig_all[!is.na(trig_all)]
    if (length(trig_all) > 0) {
      trig_tbl <- rbindlist(lapply(trig_all, get_trigger_info,
                                   case.name = case.name, 
                                   sobek.project = sobek.project,
                                   html = FALSE)
                            )
      ct_info_tbl <- rbind(ct_info_tbl, trig_tbl)
      ct_info_tbl[, orig_line := .I]
      r_group <- c("Controller Information")
      n_group <- c(ct_info_tbl[Parameter == 'Trigger ID', orig_line], nrow(ct_info_tbl))
      r_group <- unlist(c("Structure Information", 
                          ct_info_tbl[n_group[-length(n_group)], 
                                      paste0('Infos for ', Parameter, ': ', Value)]
      ))
      n_group <- n_group - 1
      n_group <- n_group - shift(n_group, 1, fill = 0)
      ct_info_tbl[, orig_line := NULL]
    }
  }
  if (isTRUE(html)) {
    ct_info_tbl <- htmlTable::htmlTable(
      ct_info_tbl,
      align = 'l',
      rgroup = r_group,
      n.rgroup = n_group,
      caption = paste(
        "Information table of the Controller:", ct.id),
      tfoot = paste('Case:', case.name)
    )
  }
  return(ct_info_tbl)
}


#' Get information of a controller for hovering
#' @param ct.id ID of the controller
#' @param def.file Path to control.def file
#' @param case.name Name of the case (considered if def.file == NULL)
#' @param sobek.project Path to sobek.project (considered if def.file == NULL)
#' @param trigger If TRUE, information about triggers will be given
#' @export
#' @return a list
get_control_popover <- function(ct.id = NULL,
                              def.file = NULL,
                              case.name = NULL,
                              sobek.project = NULL,
                              trigger = FALSE,
                              html = TRUE,
                              tble = FALSE
) {
  if (is.na(ct.id)) return('')
  if (is.null(ct.id)) return('')
  if (is.null(def.file)) {
    def.file <- get_file_path(case.name, sobek.project, type = 'control.def')
  } else {
    if (isTRUE(trigger)) {
      stopifnot(!is.null(case.name) & !is.null(sobek.project))
    }
  }
  ct_def <- .get_control_def(control.def.f = def.file)
  ct_id_tbl <- ct_def[id == ct.id][1, ]
  # activated triggers
  ct_tg_ta <- ct_id_tbl$ta %>% str_replace_all('0','') %>% str_squish()
  # ids of activated triggers
  ct_tg_gi <- ct_id_tbl$gi %>% str_replace_all("'-1'", '') %>% 
    str_replace_all("'", "") %>% str_squish()
  ct_info_list <- c(
    '<strong>Name: </strong>' = ct_id_tbl$name,
    '<strong>Type: </strong>' = .get_ct_type(ct_id_tbl$ct),
    '<strong>Parameter: </strong>' = .get_ct_param_type(ct_id_tbl$ca),
    '<strong>Measurement: </strong> ' = ct_id_tbl$ml,
    '<strong>Measured parameter: </strong>' = .get_cp_type(ct_id_tbl$cp),
    '<strong>Time lag: </strong>' = ct_id_tbl$mp,
    '<strong>Update frequency: </strong>' = ct_id_tbl$cf,
    '<strong>Trigger activated: </strong>' = ct_tg_ta, 
    '<strong>Trigger IDs: </strong>' = ct_tg_gi,
    '<strong>dValue/dt: </strong>' = ct_id_tbl$mc
  )
  ct_info_tbl <- data.table(Parameter = names(ct_info_list),
                            Value = ct_info_list)
  if (isTRUE(tble)) {
    ct_tble <- ct_def[id == ct.id & grepl(' < {0,1}$', V1), c('V1')]
    ct_tble[, V1 := str_replace(V1, "< {0,1}$", "")]
    ct_tble[, V1 := str_replace_all(V1, "'", "")]
    ct_tble[, V1 := str_replace(V1, ";", " ")]
    colnames(ct_tble) <- 'Value'
    nrow_tble <- nrow(ct_tble)
    if (nrow_tble > 0) {
      max_row <- 10 # diplay only max. 10 rows
      if (nrow_tble > max_row) {
        ct_info_list[['<strong>Controller table (first 10 rows):</strong>']] <- ''
      } else {
        ct_info_list[['<strong>Controller table:</strong>']] <- ''
      }
      max_row <- min(max_row, nrow_tble)
      ct_tble <- ct_tble[1:max_row, ]
      ct_tble[, Parameter := c(rep('', max_row))]
      ct_info_tbl <- data.table(Parameter = names(ct_info_list),
                                Value = ct_info_list)
      ct_info_tbl <- rbind(ct_info_tbl, ct_tble)
    }
  }
  r_group <- c("Structure Information")
  n_group <- nrow(ct_info_tbl) # Number of rows for "Structure information"
  if (isTRUE(trigger)) {
    trig_all <- str_match(
      ct_info_tbl[Parameter == 'Trigger IDs', Value], 
      "'([^']+)' '([^']+)' '([^']+)' '([^']+)'"
    )[, 2:5]
    trig_all <- trig_all[trig_all != '-1']
    trig_all <- trig_all[!is.na(trig_all)]
    if (length(trig_all) > 0) {
      trig_tbl <- rbindlist(lapply(trig_all, get_trigger_info,
                                   case.name = case.name, 
                                   sobek.project = sobek.project,
                                   tble = tble,
                                   html = FALSE)
      )
      ct_info_tbl <- rbind(ct_info_tbl, trig_tbl)
      ct_info_tbl[, orig_line := .I]
      r_group <- c("Controller Information")
      n_group <- c(ct_info_tbl[Parameter == 'Trigger ID', orig_line], 
                   nrow(ct_info_tbl))
      r_group <- unlist(c("Structure Information", 
                          ct_info_tbl[n_group[-length(n_group)], 
                                      paste0('Infos for ', Parameter, ': ', Value)]
      ))
      n_group <- n_group - 1
      n_group <- n_group - shift(n_group, 1, fill = 0)
      ct_info_tbl[, orig_line := NULL]
    }
  }
  ct_info_tbl <- ct_info_tbl[!is.na(Value)]
  ret <- ct_info_tbl[, paste0(Parameter, Value)]
  ret <- paste(ret, collapse = '<br>')
  return(ret)
}


#' get controlling of controllers for a structure
#'
#' This function finds all controllers used by a structure and return a list of
#' data.table that are controlling table of the controllers. If there is only one
#' controller it will return a data.tble, if more then a list, if none NA
#'
#' @param s.id ID of the structure
#' @param ct.id ID of the controller to read directly
#' @param case.name Name of sobek case
#' @param sobek.project Path to sobek project
#'
#' @return data.table or list of data.table
#' @export
get_control_tbl <- function(
  s.id = NULL,
  ct.id = NULL,
  case.name = NULL,
  sobek.project = NULL
) {
  
  if (!is.null(s.id)) {
    # get path to files
    str_def_f <- get_file_path(case.name = case.name,
                               sobek.project = sobek.project,
                               type = 'struct.def')
    str_dat_f <- get_file_path(case.name = case.name,
                               sobek.project = sobek.project,
                               type = 'struct.dat')
    
    str_dat_tbl <- .get_struct_dat(str_dat_f)
    if (s.id %in% str_dat_tbl$id) {
      str_id_tbl <- str_dat_tbl[id == s.id][1,]
      cj_list <- str_split(str_id_tbl$cj, ' ', simplify = TRUE)[1, ]
      ct_id_list <- gsub("'", "", cj_list[!grepl("'-1'", cj_list)])
    } else {
      stop('structure with ID: ', s.id, 
           " not found. Maybe you want to use ct.id = '", s.id, "' instead?")
    }
  } else {
    ct_id_list <- ct.id
  }
  if (length(ct_id_list) > 0) {
      ct_def_f <- get_file_path(case.name = case.name, 
                                sobek.project = sobek.project,
                                'control.def')
      ct_def <- .get_control_def(ct_def_f)
      ct_tble_list <- list()
      for (i in ct_id_list) {
        ct_name_tbl <- ct_def[id == i, c('V1')][grepl(" < {0,1}$", V1)]
        if (nrow(ct_name_tbl) > 0) {
          ct_name_tbl[, c('V2', 'V3', 'V4') := 
                        tstrsplit(str_trim(V1), split = ' ')]
          ts_chk <- grepl("^'\\d{4}/\\d{2}/\\d{2}", ct_name_tbl$V2[1])[1]
          if (ts_chk) {
            ct_name_tbl[, ts := as.POSIXct(V2, tz = 'GMT',
                                         format = "'%Y/%m/%d;%H:%M:%S'")]
          } else {
            ct_name_tbl[, ts := as.double(V2)]
          }
          ct_name_tbl[, value := as.double(V3)]
          ct_name_tbl <- ct_name_tbl[, c('ts', 'value')]
        }
        ct_tble_list[[i]] <- ct_name_tbl
      }
    if (length(ct_tble_list) == 1) ct_tble_list <- ct_tble_list[[1]]
  } else {
    ct_tble_list <- NA
  }

  return(ct_tble_list)
}

#' Change controlling table for a controller
#' 
#' @param tble New control table
#' @param ct.id Controller ID
#' @param ct.def Path to control.def file (or given by case.name and sobek.project)
#' @param case.name Sobek case name
#' @param sobek.project Path to sobek project
#' @export
#' @examples
#' \dontrun{
#' guns_ctl_tbl <- get_control_tbl(
#'   ct.id = 'guntersblum_ab',
#'   case.name = 'NurRhein_ZPK_HW1988_Mittel_Nur_Eich_EreigOpt',
#'   sobek.project = 'd:/so21302/rhein.lit'
#' )
#' # change value
#' guns_ctl_tbl[, value := value + 0.0001]
#' change_control_tbl <- function(
#'   tble = guns_ctl_tbl,
#'   ct.id = 'guntersblum_ab',
#'   case.name = 'NurRhein_ZPK_HW1988_Mittel_Nur_Eich_EreigOpt',
#'   sobek.project = 'd:/so21302/rhein.lit'
#' )
#' }
change_control_tbl <- function(
  tble,
  ct.id,
  ct.def = NULL,
  case.name = NULL,
  sobek.project = NULL
) {
  tble <- tble[, 1:2]
  colnames(tble) <- c('ts', 'value')
  nrow_tble <- nrow(tble)
  tble[, value := as.numeric(value)]
  tble <- tble[!is.na(value)]
  if (nrow_tble != nrow(tble)) stop('tble has wrong values/format')
  ts_class <- class(tble$ts)[1]
  if (grepl('numeric', ts_class)) {
    tble[, tble_line := paste(ts, value, '<')]
  } else if (grepl('POSIX', ts_class)) {
    tble[, tble_line := paste0(
      format(ts, format = "'%Y/%m/%d;%H:%M:%S' "),
      value,
      ' <'
      )
      ]
  } else {
    stop('tble has wrong format')
  }
  if (is.null(ct.def)) {
    stopifnot(!is.null(case.name) || !is.null(sobek.project))
    ct.def <- get_file_path(
      case.name = case.name,
      sobek.project = sobek.project,
      type = 'control.def'
    )
  }
  ct_def_tbl <- .get_control_def(ct.def)
  nrow_def <- nrow(ct_def_tbl)
  ct_tbl <- ct_def_tbl[id == ct.id]
  if (nrow(ct_tbl) == 0) {
    stop('control ID not found: ', ct.id)
  }
  tble_begin <- ct_tbl[grepl('TBLE', V1), org_line_nr]
  tble_end <- ct_tbl[grepl('tble', V1), org_line_nr]
  stopifnot(length(tble_begin) == 1 & length(tble_end) == 1)
  fwrite(ct_def_tbl[1:tble_begin, c('V1')], file = ct.def,
         sep = '\n', col.names = FALSE)
  fwrite(tble[, c('tble_line')], file = ct.def,
         append = TRUE,
         sep = '\n', col.names = FALSE)
  fwrite(ct_def_tbl[tble_end:nrow_def, c('V1')], file = ct.def,
         append = TRUE,
         sep = '\n', col.names = FALSE)
}


#' Get information of a structure
#' @param s.id ID of the structure
#' @param case.name Name of the case
#' @param sobek.project Path to sobek project
#' @param html Output to HTML table? Default TRUE
#' @param trigger If TRUE, information about triggers will be given
#' @param control If TRUE, information about controllers will be given
#' @export
#' @return a data.table or a HTML object
get_struct_info <- function(
  s.id = NULL,
  case.name = NULL,
  sobek.project = NULL,
  html = TRUE,
  trigger = TRUE,
  control = TRUE
){

  # get path to files
  str_def_f <- get_file_path(case.name = case.name,
                             sobek.project = sobek.project,
                             type = 'struct.def')
  str_dat_f <- get_file_path(case.name = case.name,
                             sobek.project = sobek.project,
                             type = 'struct.dat')

  str_dat_tbl <- .get_struct_dat(str_dat_f)
  if (!s.id %in% str_dat_tbl$id) {
    stop(s.id, ' not found in struct.dat. Remember that cases are sensitive')
  }
  str_def_tbl <- .get_struct_def(str_def_f)
  str_id_tbl <- str_dat_tbl[id == s.id][1,]
  str_id_def <- str_def_tbl[def_ID == str_id_tbl$def_ID][1,]
  str_id_list <- c(
    "Structure name" = str_id_tbl$name,
    "Structure ID" = s.id,
    'Structure definition ID' = str_id_tbl$def_ID,
    "Structure type" = .get_str_type(str_id_def$def_ty),
    "Crest level" = str_id_def$cl,
    "Crest width" = str_id_def$cw,
    "Controllers used" = str_id_tbl$ca,
    "Possible flow direction" = .get_rt_type(str_id_def$rt),
    'Total controllers' = 0L
  )
  if (!is.na(str_id_tbl$cj)) {
    cj_list <- str_split(str_id_tbl$cj, ' ', simplify = TRUE)[1, ]
    ct_id_list <- gsub("'", "", cj_list[!grepl("'-1'", cj_list)])
    if (length(ct_id_list) > 0) {
      str_id_list[['Total controllers']] <- length(ct_id_list)
      # ct_id_tbl <- subset(ct_def_tbl, id %in% ct_id_list & !is.na(ct))
      for (i in seq_along(ct_id_list)) {
        ct_name <- paste('Controller', i, 'ID: ')
        str_id_list[[ct_name]] <- ct_id_list[[i]]
      }
    }
  } else {
    ct_id_list <- NULL
  }
  str_info_tbl <- data.table(
    Parameter = names(str_id_list),
    Value = unlist(str_id_list)
  )
  #str_info_tbl <- data.table(unlist(str_id_list))
  r_group <- c("Structure Information")
  n_group <- nrow(str_info_tbl) # Number of rows for "Structure information"
  if (isTRUE(control) & length(ct_id_list) > 0) {
    ct_tbl <- rbindlist(lapply(ct_id_list, get_control_info,
                        def.file = NULL, 
                        case.name = case.name, 
                        sobek.project = sobek.project,
                        html = FALSE,
                        trigger = trigger))
    str_info_tbl <- rbind(str_info_tbl, ct_tbl)
    # calculating number of rows for each Controller group
    str_info_tbl[, orig_line := .I]
    n_group <- c(str_info_tbl[Parameter == 'Controller ID' | Parameter == 'Trigger ID'
                              , orig_line])
    r_group <- unlist(c("Structure Information", 
                 str_info_tbl[n_group, paste0('Infos for ', 
                                              Parameter, ': ', Value)]
                 ))
    n_group <- n_group - 1
    n_group <- n_group - shift(n_group, 1, fill = 0)
    str_info_tbl[, orig_line := NULL]
  }
  if (isTRUE(html)) {
    str_info_tbl <- htmlTable::htmlTable(
      str_info_tbl,
      align = 'l',
      rgroup = r_group,
      n.rgroup = n_group,
      caption = paste(
        "Information table of the structure:", s.id),
      tfoot = paste('Case:', case.name)
      )
  }
  return(str_info_tbl)
}


#' Get list of structure for one case
#' 
#' This functions read information from struct.dat and struct.def then produces
#' a table listing all structures in the case together with their ids, names, definition ids and controllers.
#' 
#' @param case.name Name of the case
#' @param sobek.project Path to sobek project
#' @param html Default TRUE. Export a html table
#' @return a data.table or htmlTable
#' @export
#' @examples
#' \dontrun{
#' case_name <- 'NurRhein_ZPK_HW1988_Mittel'
#' so_prj <- 'd:/so21302/rhein.lit'
#' get_all_struct(
#'   case.name = case_name,
#'   sobek.project = so_prj,
#'   html = FALSE,
#'   output = 'd:/users/YourNameHere/desktop'
#'   ) # output will be file with name struct_info_tbl_xxx.xlsx to desktop
#'}
get_all_struct <- function(
  case.name = NULL,
  sobek.project = NULL,
  html = TRUE,
  tble = TRUE,
  output = NULL
) {
    html <- isTRUE(html)
    tble <- isTRUE(tble)
    sobek.project <- str_replace_all(sobek.project, '\\\\', '/')
    if (!is.null(output)) {
      folder_name <- dirname(output)
      file_name <- basename(output)
      folder_chk <- file_test('-d', output)
      if (folder_chk) {
        # output was given as a path to an existing folder
        file_out <- tempfile(pattern = 'struct_info_tbl_', 
                             tmpdir = output, 
                             fileext = ifelse(html, '.html', '.xlsx')
                             )
      } else {
        # output was given as a path to a file
        folder_chk <- file_test('-d', folder_name)
        if (!folder_chk) stop('output path does not exist')
        file_ext <- str_extract(file_name, '\\..+$')
        if (is.na(file_ext)) {
          file_name <- paste0(file_name, ifelse(html, '.html', '.xlsx'))
        }
        file_out <- file.path(folder_name, file_name)
      }
    }
    str_dat_tbl <-
      .get_all_struct(case.name = case.name, sobek.project = sobek.project, 
                      tble = tble)
    if (!html) {
      if (!is.null(output)) {
        # write output to excel file
        xlsx_wb <- createWorkbook()
        xlsx_sheet <- createSheet(xlsx_wb, sheetName = 'struct_info_tbl')
        addDataFrame(str_dat_tbl, xlsx_sheet)
        saveWorkbook(xlsx_wb, file = file_out)
      }
      return(str_dat_tbl)
    } else {
      rmd_f <- system.file('Rmd/struct_table.Rmd', package = 'sobekio')
      rmd_tmp <- tempfile(pattern = 'struct_info_', fileext = '.Rmd')
      rmd <- read.table(rmd_f, sep = '\n', quote = "", header = FALSE) %>% 
        as.data.table()
      rmd[V1 == 'tble', V1 := 
            paste0("tble <- ", ifelse(tble, 'TRUE', 'FALSE'))]
      rmd[V1 == 'case.name', V1 := paste0("case.name <- '", case.name, "'")]
      rmd[V1 == 'sobek.project', V1 := paste0("sobek.project <- '", sobek.project, "'")]
      fwrite(
        file = rmd_tmp,
        rmd,
        sep = '\n',
        col.names = FALSE,
        append = FALSE,
        quote = FALSE
      )
      html_tmp <- str_replace(rmd_tmp, 'Rmd$', 'html')
      rmarkdown::render(rmd_tmp, output_format = 'html_document', 
                        output_file = html_tmp)
      if (!is.null(output)) {
        file.copy(from = html_tmp, to = file_out)
        print(paste('and copied to:', file_out))
        html_tmp <- file_out
      }
      browseURL(html_tmp)
      invisible(str_dat_tbl)
  }
}


.get_all_struct <- function(
  case.name = NULL,
  sobek.project = NULL,
  html = FALSE,
  tble = TRUE
) {
  
  str_dat_f <- get_file_path(
    case.name = case.name, 
    sobek.project = sobek.project,
    'struct.dat'
  )
  str_def_f <- get_file_path(
    case.name = case.name, 
    sobek.project = sobek.project,
    'struct.def'
  )
  str_def_tbl <- .get_struct_def(str_def_f)
  str_def_tbl <- str_def_tbl[grepl(" id '.*'", V1)]
  str_def_tbl[def_ty == '9', rt := str_match(V1, ' (dn -*\\d) ')[, 2]]
  str_def_tbl[, def_ty := 
                sapply(str_def_tbl$def_ty, .get_str_type)]
  str_def_tbl[, rt := 
                sapply(str_def_tbl$rt, .get_rt_type)]
  str_def_tbl <- str_def_tbl[, c('def_ID', 'def_name', 'def_ty', 'cl', 'cw', 'rt')]
  str_dat_tbl <- .get_struct_dat(str_dat_f)
  str_mtx <- str_match(str_dat_tbl$cj, "'([^']+)' '([^']+)' '([^']+)' '([^']+)'")[, -1] %>% as.data.table()
  str_mtx[V1 == '-1', V1 := NA_character_][V2 == '-1', V2 := NA_character_]
  str_mtx[V3 == '-1', V3 := NA_character_][V4 == '-1', V4 := NA_character_]
  str_dat_tbl[, c('ct1', 'ct2', 'ct3', 'ct4') := str_mtx]
  # get controllers for structure that have only one controller
  str_dat_tbl[!grepl("ca \\d \\d ", V1), ct1 := str_match(V1, " cj '([^']*)' ")[, 2]]
  str_dat_tbl <- str_dat_tbl[, c('id', 'name', 'def_ID',
                                 'ct1', 'ct2', 'ct3', 'ct4')]
  str_dat_tbl <- merge(str_dat_tbl, str_def_tbl, by.x = 'def_ID',
                       no.dups = TRUE,
                       by.y = 'def_ID') %>% setkey(NULL) %>% unique()
  str_cols <- c('id',
                'name',
                'def_ty',
                'def_name',
                'cl',
                'cw',
                'rt',
                'ct1',
                'ct2',
                'ct3',
                'ct4')
  str_cols_names <-
    c(
      'ID',
      'Name',
      'Type',
      'Definition name',
      'Crest level',
      'Crest width',
      'Flow direction',
      'Controller 1',
      'Controller 2',
      'Controller 3',
      'Controller 4'
    )
  str_dat_tbl <- str_dat_tbl[, .SD ,.SDcols = str_cols]
  colnames(str_dat_tbl) <- str_cols_names
  setorder(str_dat_tbl, ID)
  
  if (html) {
    ct_names <- grep("Controller \\d", 
                     colnames(str_dat_tbl), value = TRUE)
    for (i in ct_names) {
      ct_hover <- lapply(
        str_dat_tbl[[i]],
        get_control_popover,
        case.name = case.name,
        sobek.project = sobek.project,
        html = TRUE,
        tble = tble
      )
      str_dat_tbl[[i]] <- cell_spec(
        str_dat_tbl[[i]],
        popover = spec_popover2(
          content = ct_hover,
          title = '<strong>Controller Information</strong>',
          html = TRUE,
          position = 'left'
        )
      )
    }
  }
  return(str_dat_tbl)
}


#' Turn off Weir(s) / Weir(s)
#'
#' Turn off Weir(s) / Weir(s) by deactivate all controllers and set crest width to 0
#'
#' @param struct Name(s) of the (River) Weir(s)
#' @param case Case name
#' @param sobek.project Path to sobek project
#' @export
set_struct_off <- function(
  struct = NULL,
  case.name = NULL,
  sobek.project = NULL) {
  struct.dat.f <- get_file_path(case.name = case.name, 
                                sobek.project = sobek.project,
                                type = 'struct.dat')
  struct.def.f <- get_file_path(case.name = case.name, 
                                sobek.project = sobek.project,
                                type = 'struct.def')
  struct_dat <- .get_struct_dat(struct.dat.f)
  struct_def <- .get_struct_def(struct.def.f)
  for (i in seq_along(struct)) {
    struct_def_id <- struct_dat[id == struct[[i]], def_ID]
    # deactivate all controllers, prevent time controllers open the structure
    struct_dat[id == struct[[i]], 
               V1 := str_replace(V1, 'ca \\d \\d \\d \\d', 'ca 0 0 0 0')]
    struct_dat[id == struct[[i]], 
               V1 := str_replace(V1, 'ca \\d ', 'ca 0 ')]
    # change crest-width to 0, no water coming in
    struct_def[def_ID == struct_def_id, V1 := str_replace(V1, ' cw \\S+ ',
                                                          ' cw 0 ')]
  }
  file.copy(struct.dat.f, paste(struct.dat.f, ".BAK", sep = ""))
  file.copy(struct.def.f, paste(struct.dat.f, ".BAK", sep = ""))
  fwrite(struct_dat[, .SD, .SDcols = c("V1")], struct.dat.f, sep = "\n",
         col.names = FALSE, quote = FALSE)
  fwrite(struct_def[, .SD, .SDcols = c("V1")], struct.def.f, sep = "\n",
         col.names = FALSE, quote = FALSE)
}


#' Turn on one River Weir / Weir
#'
#' Turn on one River Weir / Weir by activate related controllers and set its  characters
#'
#' @param struct Name(s) of the (River) Weir(s)
#' @param cw Struct Crest Width
#' @param ct Struct controller ID(s), ex. c("##114", "##112")
#' @param case Case name
#' @param sobek.project Path to sobek project
#' @export
set_struct_on <- function(
  struct = NULL,
  cw = NULL,
  ct = NULL,
  case.name = NULL,
  sobek.project = NULL) {
  
  struct.dat.f <- get_file_path(case.name = case.name,
                                sobek.project = sobek.project,
                                type = 'struct.dat')
  struct.def.f <- get_file_path(case.name = case.name,
                                sobek.project = sobek.project,
                                type = 'struct.def')
  struct_dat <- .get_struct_dat(struct.dat.f)
  struct_def <- .get_struct_def(struct.def.f)
  struct_def_id <- struct_dat[id == struct, def_ID]
  struct_type <- struct_def[def_ID == struct_def_id, def_ty][[1]]
  # deactivate all controllers, prevent time controllers open the structure
  ct <- unlist(ct)
  if (!is.null(ct)) {
    # number of controllers is between 1 and 4
    stopifnot(length(ct) < 4 & length(ct) > 0)
    if (!struct_type %in% c("0", "6")) stop('Only support Weir or River Weir')
    # struct_type 0 for River Weir with max 4 Controllers
    if (struct_type == "0") {
      ca_match_patt <- " ca \\d \\d \\d \\d "
      cj_match_patt <- " cj '[^']+' '[^']+' '[^']+' '[^']+' "
      ca_rep_patt <- c(" ca", '0', '0', '0', '0', '')
      cj_rep_patt <- c(" cj", "'-1'", "'-1'", "'-1'", "'-1'", "")
      # ca_patt <- "'ca"
      for (s in seq_along(ct)) {
        ca_rep_patt[s + 1] <- '1'
        cj_rep_patt[s + 1] <- paste("'", ct[[s]], "'", sep = "")
      }
      ca_rep_patt <- paste(ca_rep_patt, collapse = " ")
      cj_rep_patt <- paste(cj_rep_patt, collapse = " ")
    }
    # struct_type 6 for simple Weir with max only one controller
    if (struct_type == "6") {
      if (length(ct) > 1) stop("Too many controllers for a Weir")
      ca_match_patt <- " ca \\d "
      cj_match_patt <- " cj '[^']+' "
      ca_rep_patt <- c(" ca 1 ")
      cj_rep_patt <- paste(" cj '", ct[[1]], "' ")
    }
    struct_dat[id == struct, 
               V1 := str_replace(V1, ca_match_patt, ca_rep_patt)]
  }
  # change crest-width to cw
  if (!is.null(cw)) {
    cw_rep <- paste(' cw ', cw, ' ', sep = '')
    struct_def[def_ID == struct_def_id, 
               V1 := str_replace(V1, " cw \\S+ ", cw_rep)
               ]
  }
  file.copy(struct.dat.f, paste(struct.dat.f, ".BAK", sep = ""))
  file.copy(struct.def.f, paste(struct.dat.f, ".BAK", sep = ""))
  fwrite(struct_dat[, .SD, .SDcols = c("V1")], struct.dat.f, sep = "\n",
         col.names = FALSE, quote = FALSE)
  fwrite(struct_def[, .SD, .SDcols = c("V1")], struct.def.f, sep = "\n",
         col.names = FALSE, quote = FALSE)
}



#' Delete triggers OR controllers
#'
#' This function remove triggers OR controllers by IDs from files
#'
#' @param trig_ids IDs of triggers to remove
#' @param cont_ids IDs of controllers to remove
#' @param path path to trigger.def/control.def
#' @param trig File name of trigger.def
#' @param cont File name of control.def
#' @param backup Default TRUE. To make a simple backup of the file
#'
#' @export
delete_trigger_by_ids <- function(
  trig_ids = NULL,
  cont_ids = NULL,
  path = ".",
  trig = NULL,
  cont = NULL,
  backup = TRUE
) {
  
  if (!is.null(trig_ids)) {
    stopifnot(!is.null(trig))
    trig_f <- paste(path, trig, sep = "/")
    trig <- .get_trigger_def(trig_f)
    trig_n <- trig[!id %in% trig_ids, ]
    if (isTRUE(backup)) {
      file.copy(trig_f, paste(trig_f, "_BAK", sep = ""), overwrite = TRUE)
    }
    fwrite(trig_n[, list(V1)], file = trig_f, col.names = FALSE,
           quote = FALSE)
  }
  
  if (!is.null(cont_ids)) {
    stopifnot(!is.null(cont))
    cont_f <- paste(path, cont, sep = "/")
    cont <- .get_control_def(cont_f)
    cont_n <- cont[!id %in% cont_ids, ]
    if (isTRUE(backup)) {
      file.copy(cont_f, paste(cont_f, "_BAK", sep = ""), overwrite = TRUE)
    }
    fwrite(cont_n[, list(V1)], file = cont_f, col.names = FALSE,
           quote = FALSE)
  }
}

# Sobek code-type -------------------------------------------------------
# type of structure
.get_str_type <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.na(x)) return(NA_character_)
  switch(
    x,
    '0' = 'River weir',
    '1' = 'River advanced weir',
    '2' = 'General structure',
    '3' = 'River pump',
    '4' = 'Database structure',
    '5' = 'Unknown',
    '6' = 'Weir',
    '7' = 'Orifice',
    '8' = 'Unknown',
    '9' = 'Pump',
    '10' = 'Culvert/Siphon',
    '11' = 'Universal weir',
    '12' = 'Bridge',
    '13' = 'Branch growth 1D Dam break node',
    '112' = 'Branch growth 2D Dam break node',
    NA_character_
  )
}

# type of flow direction through weir
.get_rt_type <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.na(x)) return(NA_character_)
  switch(
    x,
    '0' = 'both',
    '1' = 'positive',
    '2' = 'negative',
    '3' = 'no flow',
    # for pumps
    'dn 1' = 'upward',
    'dn 2' = 'downward',
    'dn 3' = 'both',
    'dn -1' = 'upward (flow >< branch)',
    'dn -2' = 'downward (flow >< branch)',
    'dn -3' = 'both (flow >< branch)',
    NA_character_
  )
}

# type of controller
.get_ct_type <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.na(x)) return(NA_character_)
  switch(
    x,
    '0' = 'Time controller',
    '1' = 'Hydraulic controller',
    '2' = 'Interval controller',
    '3' = 'PID controller',
    '4' = 'Relative time controller',
    '5' = 'Relative from value controller',
    NA_character_
  )
}

# type of control parameter
.get_ct_param_type <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.na(x)) return(NA_character_)
  switch(
    x,
    '0' = 'Crest level',
    '1' = 'Crest width',
    '2' = 'Gate height',
    '3' = 'Pump capacity',
    '4' = '',
    '5' = 'Bottom level of 2D grid cell',
    NA_character_
  )
}

# type of measured parameters
.get_cp_type <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.na(x)) return(NA_character_)
  switch(
    x,
    '0' = 'Water level',
    '1' = 'Discharge',
    '2' = 'Head difference',
    '3' = 'Velocity',
    '4' = 'Flow direction',
    '5' = 'Pressure difference',
    NA_character_
  )
}


# modified from kableExtra, added html = TRUE
#' @export
spec_popover2 <-
  function(content = NULL,
           title = NULL,
           trigger = "hover",
           html = TRUE,
           position = "right")
  {
    trigger <- match.arg(trigger, c("hover", "click", "focus",
                                    "manual"), several.ok = TRUE)
    html <- ifelse(html, '"true"', '"false"')
    position <- match.arg(position,
                          c("bottom", "top", "left",
                            "right", "auto"),
                          several.ok = TRUE)
    popover_options <-
      paste(
        "data-toggle=\"popover\"",
        paste0("data-trigger=\"",
               trigger, "\""),
        paste0("data-placement=\"", position,
               "\""),
        paste0("data-html=", html),
        ifelse(!is.null(title), paste0("title=\"", title,
                                       "\""), ""),
        paste0("data-content=\"", content, "\"")
      )
    class(popover_options) <- "ke_popover"
    return(popover_options)
  }