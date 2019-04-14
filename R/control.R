#----reading Trigger file, generating trig_def----
# trigger_tbl should be one table with V1 is  the ogriginal file
# orig_line_nr is original line number, this serves any changing later
# this function get definition table of triggers (trigger.def)
.get_trigger_def <- function(trigger.file = NULL){
  stopifnot(file.exists(trigger.file))
  trig_def <- fread(trig_def_f, sep = "\n", header = FALSE)
  trig_def[, orig_line_nr := .I]
  str_mt <-  str_match(trig_def$V1, "TRGR id '([^']*)'")
  trig_def$id <- str_mt[, 2]
  trig_def[, id := id[1], .(cumsum(!is.na(id)))]

  return(trig_def)
}


.get_trigger_tbl <- function(
  trig.id, trig.def
){
  return(trig.def[id == trig.id, c('V1', 'id')])
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
  ct_tbl[, id := str_match(V1, "CNTL id '([^']*)'")[,2]]
  ct_tbl[, name := str_match(V1, "CNTL id .* nm '([^']*)'")[,2]]
  ct_tbl[, ct := str_match(V1, "CNTL id .* ct (\\d) ")[,2]]
  ct_tbl[, ca := str_match(V1, "CNTL id .* ca (\\d) ")[,2]]
  ct_tbl[, ac := str_match(V1, "CNTL id .* ac (\\d) ")[,2]]
  ct_tbl[, cf := str_match(V1, "CNTL id .* cf (\\d{1,}) ")[,2]]
  # ct_tbl[, ta := str_match(V1, "CNTL id .* ta (\\d{1,}) ")[,2]]
  ct_tbl[, ta := str_match(V1, "CNTL id .* ta (\\d \\d \\d \\d) ")[,2]]
  ct_tbl[, gi := str_match(V1, "gi ('.+') ao \\d")[,2]]
  ct_tbl[, ao := str_match(V1, "ao (\\d \\d \\d \\d)")[,2]]
  str_mt <- str_match(ct_tbl$V1, "mc (.*) bl (\\d) ti")
  ct_tbl$mc <- str_mt[, 2]
  ct_tbl$bl <- str_mt[, 3]
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
  if (ct_id_tbl_nrow > 3){
    ct_id_tbl <- ct_id_tbl[3:(ct_id_tbl_nrow - 1)]
    return(paste(ct_id_tbl$V1, collapse = "\n\r"))
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
    "5-NA",
    "Weir",
    "Ori?ce",
    "8-NA",
    "Pump",
    "Culvert/Siphon",
    "Universal weir",
    "Bridge",
    "Bbranch growth 1D Dam break node",
    "Bbranch growth 2D Dam break node"
  )
  if (s.id %in% id_list){
    str_type <- str_type_list[id_list == s.id]
  } else{
    str_type <- 'NA'
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
    ct_type <- "NA"
  }
  return(ct_type)
}


# this function converts 'ca' code to control parameter name
.get_control_parameter <- function(ca.id){
  id_list <- c(0, 1, 2, 3, 4, 5)
  ca_type_list <- c(
    "Crest level",
    "Crest Width",
    "Gate height",
    "Pump capacity",
    "",
    "Bottom level of 2D grid cell"
  )
  if (ca.id %in% id_list){
    ca_type <- ca_type_list[id_list == ca.id]
  } else{
    ca_type <- "NA"
  }
  return(ca_type)
}


#' Get information of a controller
#' @param ct.id ID of the controller
#' @param def.file Path to control.def file
#' @import data.table
#' @export
#' @return a list
get_control_info <- function(ct.id = NULL,
                             def.file = NULL){
  ct_def <- .get_control_def(control.def.f = def.file)
  ct_id_tbl <- ct_def[id == ct.id][1,]
  ct_info_list <- list(
    'ID' = ct_id_tbl$id,
    'Name' = ct_id_tbl$nm,
    'Control type' = .get_control_type(ct_id_tbl$ct),
    'Control parameter' = .get_control_parameter(ct_id_tbl$ac),
    'Controlled active' = ct_id_tbl$ca,
    'Update frequency' = ct_id_tbl$cf,
    'Trigger active' = ct_id_tbl$ta,
    'Trigger IDs' = ct_id_tbl$gi,
    'dValue/dt' = ct_id_tbl$mc,
    'tble' = .get_control_tbl(ct.id, ct_def)
  )
  ct_info_tbl <- data.table(Parameter = names(ct_info_list),
                            Value = ct_info_list)
  return(ct_info_tbl)
}


#' Get information of a structure
#' @param s.id ID of the structure
#' @param case.name Name of the case
#' @param sobek.project Path to sobek project
#' @import data.table
#' @export
#' @return a list
get_struct_info <- function(
  s.id = NULL,
  case.name = NULL,
  sobek.project = NULL
){

  # get path to files
  ct_def_f <- get_file_path(case.name = case.name,
                            sobek.project = sobek.project,
                            type = 'control.def')
  str_def_f <- get_file_path(case.name = case.name,
                             sobek.project = sobek.project,
                             type = 'struct.def')
  trig_def_f <- get_file_path(case.name = case.name,
                              sobek.project = sobek.project,
                              type = 'trigger')
  str_dat_f <- get_file_path(case.name = case.name,
                             sobek.project = sobek.project,
                             type = 'struct.dat')

  str_dat_tbl <- .get_struct_dat(str_dat_f)
  str_def_tbl <- .get_struct_def(str_def_f)
  ct_def_tbl <- .get_control_def(ct_def_f)
  str_id_tbl <- str_dat_tbl[id == s.id][1,]
  str_id_def <- str_def_tbl[def_ID == str_id_tbl$def_ID][1,]
  str_id_list <- list(
    ID = s.id,
    Name = str_id_tbl$name,
    Type = .get_struct_type(str_id_def$def_ty),
    Definition = str_id_tbl$def_ID,
    Controller = str_id_tbl$ca,
    "Crest_level" = str_id_def$cl,
    "Crest_width" = str_id_def$cw,
    "Possible_flow_direction" = str_id_def$rt,
    'Total_control' = 0L
  )
  cj_list <- str_split(str_id_tbl$cj, ' ', simplify = TRUE)[1, ]
  ct_id_list <- gsub("'", "", cj_list[!grepl("'-1'", cj_list)])
  if (length(ct_id_list) > 0){
    str_id_list$Total_control <- length(ct_id_list)
    # ct_id_tbl <- subset(ct_def_tbl, id %in% ct_id_list & !is.na(ct))
    for (i in seq_along(ct_id_list)){
      ct_name <- paste('Control', i, sep = "_")
      str_id_list[[ct_name]] <- ct_id_list[[i]]
    }
  }
  str_info_tbl <- data.table(
    Parameter = names(str_id_list),
    Value = str_id_list
  )
  # str_id <- data.table()
  return(str_info_tbl)
}


#' Get controlling information of a structure
#' @param s.id ID of the structure
#' @param case.name Name of the case
#' @param sobek.project Path to sobek project
#' @import data.table
#' @export
#' @return a list
get_struct_ct <- function(
  s.id = NULL,
  case.name = NULL,
  sobek.project = NULL
){
  str_info_tbl <- get_struct_info(
    s.id = s.id,
    case.name = case.name,
    sobek.project = sobek.project
  )
  ct_def_f <- get_file_path(case.name = case.name,
                            sobek.project = sobek.project,
                            type = 'control.def')
  total_ct <- str_info_tbl[Parameter == 'Total_control', Value]
  if (total_ct > 0){
    ct_list <- str_info_tbl[grepl('^Control_', Parameter), Value]
    ct_info_list <- lapply(ct_list, get_control_info, def.file = ct_def_f)
    ct_info_tbl <- ct_info_list[[1]]
    colnames(ct_info_tbl) <- c('Parameter', 'Controller_1')
    if (length(ct_info_list) > 1){
      for (i in 2:length(ct_info_list)){
        ct_info_tbl <- merge(ct_info_tbl, ct_info_list[[i]], by = 'Parameter')
        # colnames(ct_info_tbl) <- c('Parameter',
        #                            paste('Controller', i, sep = '_'))
      }
    }
    colnames(ct_info_tbl) <- c('Parameter',
                               paste('Controller',
                                     1:length(ct_info_list), sep = '_')
                               )
    return(ct_info_tbl)
  }
  return(NA)
}
