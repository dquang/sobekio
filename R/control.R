#----reading Trigger file, generating trig_def----
# trigger_tbl should be one table with V1 is  the ogriginal file
# orig_line_nr is original line number, this serves any changing later
# this function get definition table of triggers (trigger.def)
.get_trigger_def <- function(trigger.file = NULL){
  stopifnot(file.exists(trigger.file))
  trig_def <- fread(trigger.file, sep = "\n", header = FALSE)
  trig_def[, orig_line_nr := .I]
  str_mt <-  str_match(trig_def$V1,
                       "TRGR id '([^']*)' nm '([^']*)'")
  #ty ([0-9]{1}) tp ([0-9]{1})
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


# get_trigger_tbl <- function(
#   case.name, sobek.project
# ){
#   return(trig.def[id == trig.id, c('V1', 'id')])
# }


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
get_trigger_info <- function(t.id, case.name, sobek.project, html = TRUE) {
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
    Parameter = list(
      'Trigger_ID', 'Trigger_name', 'Trigger_type', 'Trigger_parameter', 
      'Trigger_measurement', 'Trigger_tble'
    ),
    Value = list(
      trig_def[1, id], trig_def[1, nm], t_type, t_par, trig_def[1, ml],
      paste(trig_def[grepl(" <$", V1), V1], collapse = "<br>")
    )
  )
  
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
    "STDS id '([^']*)' nm '([^']*)' ty (\\d).*")
  str_def_tbl$def_ID <- str_mt[, 2]
  str_def_tbl$def_name <- str_mt[, 3]
  str_def_tbl$def_ty <- str_mt[, 4]
  # get crest level, crest width
  str_mt <- str_match(
    str_def_tbl$V1,
    "STDS id.* cl (.*) cw (\\d*\\.*\\d*)\\ .*")
  str_def_tbl$cl <- str_mt[, 2]
  str_def_tbl$cw <- str_mt[, 3]
  # get possible flow direction
  str_mt <- str_match(
    str_def_tbl$V1,
    "STDS.*rt (\\d*\\.*\\d*)\\ .*")
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


# this function get controlling table of a controller
.get_control_tbl <- function(
  ct.id, ct.def
){
  ct_id_tbl <- ct.def[id == ct.id, c("V1")]
  ct_id_tbl_nrow <- nrow(ct_id_tbl)
  if (ct_id_tbl_nrow > 3) {
    ct_id_tbl <- ct_id_tbl[3:(ct_id_tbl_nrow - 1)]
    return(paste(ct_id_tbl$V1, collapse = "<br>"))
  } else{
    return(NA)
  }
}


# this function converts 'ty' code to structure type name
.get_struct_type <- function(s.id) {
  id_list <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 112)
  str_type_list <- c(
    "River weir",
    "River advanced weir",
    "General structure",
    "River pump",
    "Database structure",
    NULL,
    "Weir",
    "Orifice",
    NULL,
    "Pump",
    "Culvert/Siphon",
    "Universal weir",
    "Bridge",
    "Branch growth 1D Dam break node",
    "Branch growth 2D Dam break node"
  )
  if (s.id %in% id_list) {
    str_type <- str_type_list[id_list == s.id]
  } else{
    str_type <- NULL
  }
  return(str_type)
}


# this function converts 'ct' code to control type name
.get_control_type <- function(ct.id) {
  id_list <- c(0, 1, 2, 3, 4, 5)
  ct_type_list <- c(
    "Time controller",
    "Hydraulic controller",
    "Interval controller",
    "PID controller",
    "Relative time controller",
    "Relative from value controller"
  )
  if (ct.id %in% id_list){
    ct_type <- ct_type_list[id_list == ct.id]

  } else{
    ct_type <- NULL
  }
  return(ct_type)
}


# this function converts 'ca' code to control parameter name
.get_control_parameter <- function(ca.id){
  id_list <- c(0, 1, 2, 3, 4, 5)
  ca_type_list <- c(
    "Crest level",
    "Crest width",
    "Gate height",
    "Pump capacity",
    "",
    "Bottom level of 2D grid cell"
  )
  if (ca.id %in% id_list){
    ca_type <- ca_type_list[id_list == ca.id]
  } else{
    ca_type <- NULL
  }
  return(ca_type)
}


# this function converts 'cp' code to measured parameter name
.get_type_of_measured_param <- function(cp.id){
  id_list <- c(0, 1, 2, 3, 4, 5)
  cp_type_list <- c(
    "Water level",
    "Discharge",
    "Head difference",
    "Velocity",
    "Flow direction",
    "Pressure difference"
  )
  if (cp.id %in% id_list) {
    cp_type <- cp_type_list[id_list == cp.id]
  } else {
    cp_type <- NULL
  }
  return(cp_type)
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
  ct_info_list <- list(
    'Control_ID' = ct_id_tbl$id,
    'Control_name' = ct_id_tbl$name,
    'Control_type' = .get_control_type(ct_id_tbl$ct),
    'Control_parameter' = .get_control_parameter(ct_id_tbl$ca),
    'Controlled_active' = ct_id_tbl$ac,
    'Control_measurement' = ct_id_tbl$ml,
    'Measured_parameter' = .get_type_of_measured_param(ct_id_tbl$cp),
    'Time_lag' = ct_id_tbl$mp,
    'Update_frequency' = ct_id_tbl$cf,
    'Trigger_active' = ct_id_tbl$ta,
    'Trigger_IDs' = ct_id_tbl$gi,
    'dValue/dt' = ct_id_tbl$mc,
    'Control_tble' = .get_control_tbl(ct.id, ct_def)
  )
  ct_info_tbl <- data.table(Parameter = names(ct_info_list),
                            Value = ct_info_list)
  r.group <- c("Structure Information")
  n.rgroup <- c(11) # Number of rows for "Structure information"
  if (isTRUE(trigger)) {
    trig_all <- str_match(
      ct_info_tbl[Parameter == 'Trigger_IDs', Value], 
      "'([^']+)' '([^']+)' '([^']+)' '([^']+)'"
    )[, 2:5]
    trig_all <- trig_all[trig_all != '-1']
    if (length(trig_all) > 0) {
      trig_tbl <- rbindlist(lapply(trig_all, get_trigger_info,
                                   case.name = case.name, 
                                   sobek.project = sobek.project,
                                   html = FALSE)
                            )
      ct_info_tbl <- rbind(ct_info_tbl, trig_tbl)
      r.group <- c("Controller Information")
      n.rgroup <- c(13) # Number of rows for "Controller information"
      ct_info_tbl[, orig_line := .I - 1]
      r.group <- c("Controller Information", paste('Trigger', trig_all))
      n.rgroup <- c(ct_info_tbl[Parameter == 'Trigger_ID', orig_line], 
                    nrow(ct_info_tbl)) 
      n.rgroup <- n.rgroup - shift(n.rgroup, 1, fill = 0)
      ct_info_tbl[, orig_line := NULL]
    }
  }
  if (isTRUE(html)) {
    ct_info_tbl <- htmlTable::htmlTable(
      ct_info_tbl,
      align = 'l',
      rgroup = r.group,
      n.rgroup = n.rgroup,
      caption = paste(
        "Information table of the Controller:", ct.id),
      tfoot = paste('Case:', case.name)
    )
  }
  return(ct_info_tbl)
}


#' Get information of a structure
#' @param s.id ID of the structure
#' @param case.name Name of the case
#' @param sobek.project Path to sobek project
#' @param html Output to HTML table? Default TRUE
#' @param trigger If TRUE, information about triggers will be given
#' @param control If TRUE, information about controllers will be given
#' @import data.table
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
  str_id_list <- list(
    Struct_ID = s.id,
    Struct_name = str_id_tbl$name,
    Struct_type = .get_struct_type(str_id_def$def_ty),
    "Crest_level" = str_id_def$cl,
    "Crest_width" = str_id_def$cw,
    Controller = str_id_tbl$ca,
    "Possible_flow_direction" = str_id_def$rt,
    'Total_controllers' = 0L,
    'Definition_ID' = str_id_tbl$def_ID
  )
  if (!is.na(str_id_tbl$cj)) {
    cj_list <- str_split(str_id_tbl$cj, ' ', simplify = TRUE)[1, ]
    ct_id_list <- gsub("'", "", cj_list[!grepl("'-1'", cj_list)])
    if (length(ct_id_list) > 0) {
      str_id_list$Total_controllers <- length(ct_id_list)
      # ct_id_tbl <- subset(ct_def_tbl, id %in% ct_id_list & !is.na(ct))
      for (i in seq_along(ct_id_list)){
        ct_name <- paste('Control', i, sep = "_")
        str_id_list[[ct_name]] <- ct_id_list[[i]]
      }
    }
  } else {
    ct_id_list <- NULL
  }
  str_info_tbl <- data.table(
    Parameter = names(str_id_list),
    Value = str_id_list
  )
  r.group <- c("Structure Information")
  n.rgroup <- c(11) # Number of rows for "Structure information"
  if (isTRUE(control) & length(ct_id_list) > 0) {
    ct_tbl <- rbindlist(lapply(ct_id_list, get_control_info,
                        def.file = NULL, 
                        case.name = case.name, 
                        sobek.project = sobek.project,
                        html = FALSE,
                        trigger = trigger))
    str_info_tbl <- rbind(str_info_tbl, ct_tbl)
    # calculating number of rows for each Controller group
    str_info_tbl[, orig_line := .I - 1]
    r.group <- c("Structure Information", paste('Controller', ct_id_list))
    n.rgroup <- c(str_info_tbl[Parameter == 'Control_ID', orig_line], 
                 nrow(str_info_tbl)) 
    n.rgroup <- n.rgroup - shift(n.rgroup, 1, fill = 0)
    str_info_tbl[, orig_line := NULL]
  }
  if (isTRUE(html)) {
    str_info_tbl <- htmlTable::htmlTable(
      str_info_tbl,
      align = 'l',
      rgroup = r.group,
      n.rgroup = n.rgroup,
      caption = paste(
        "Information table of the structure:", s.id),
      tfoot = paste('Case:', case.name)
      )
  }
  return(str_info_tbl)
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
