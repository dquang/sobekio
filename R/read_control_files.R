#----reading Trigger file, generating trig_def----
# trigger_tbl should be one table with V1 is  the ogriginal file
# orig_line_nr is original line number, this serves any changing later
# this function get definition table of triggers (trigger.def)
.get_trigger_def <- function(trigger.def.f = NULL){
  trig_def <- fread(trigger.def.f, sep = "\n", header = FALSE, encoding = 'Latin-1')
  str_mt <-  str_match(trig_def$V1,
                       "TRGR id '([^']*)' nm '([^']*)'")
  trig_def$id <- str_mt[, 2]
  trig_def$nm <- str_mt[, 3]
  # type of trigger
  trig_def$ty <- str_match(trig_def$V1, "ty ([0-9]{1}) ")[, 2]
  # trigger parameter
  trig_def$tp <- str_match(trig_def$V1, "tp ([0-9]{1}) ")[, 2]
  # measurement id
  trig_def$ml <- str_match(trig_def$V1, " ml '([^']*)' ")[, 2]
  # structure id (for hydraulic/combined triggers only)
  trig_def$struct <- str_match(trig_def$V1, " ts '([^']*)' ")[, 2]
  # check on (only relevant if trigger parameter: 3, 4, 5)
  trig_def$chk <- str_match(trig_def$V1, " ch (\\d) ")[, 2]
  # cumulative sum of the id, i.e. 
  # id takes the value of the first element, grouping by none-NA
  trig_def[, id := id[1], by = .(cumsum(!is.na(id)))]
  
  return(trig_def)
}


#----reading structure data table-----
# str_tbl should be one table with V1 is the ogriginal file
# orig_line_nr is original line number, this serves any changing later
# this function get the table of structures (struct.dat)
.get_struct_dat <- function(struct.dat.f = NULL){
  str_tbl <- fread(struct.dat.f , sep = "\n", header = FALSE, encoding = 'Latin-1')
  str_tbl[, orig_line_nr := .I]
  # get id, name, definitionID
  str_mt <- str_match(
    str_tbl$V1,
    " id '([^']*)' nm '([^']*)' dd '([^']*)' ")
  str_tbl$id <- str_mt[, 2]
  str_tbl$name <- str_mt[, 3]
  str_tbl$def_ID <- str_mt[, 4]
  # get controllers
  str_mt <- str_match(
    str_tbl$V1,
    " id .* ca (\\d \\d \\d \\d) cj ('[^']*' '[^']*' '[^']*' '[^']*') ")
  str_tbl$ca <- str_mt[, 2]
  str_tbl$cj <- str_mt[, 3]
  str_tbl[is.na(ca), ca := str_match(V1, " ca (\\d) ")[, 2]]
  str_tbl[is.na(cj), cj := str_match(V1, " cj ('[^']*') ")[, 2]]
  
  return(str_tbl)
}


#----reading table of structure difinitions (with def_ID)-----
# this function get definition table of structures (struct.def)
.get_struct_def <- function(struct.def.f = NULL){
  str_def <- fread(struct.def.f , sep = "\n", header = FALSE, encoding = 'Latin-1')
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
  str_def_tbl[, rt := str_match(V1, ' rt (\\d*\\.*\\d*) ')[, 2]]
  str_def_tbl[def_ty == '9', rt := str_match(V1, ' (dn -*\\d) ')[, 2]]
  str_def_tbl$V1 <- NULL
  str_def <- merge(str_def, str_def_tbl, by = 'orig_line_nr', all.x = TRUE)
  str_def[, def_ID := def_ID[1], .(cumsum(!is.na(def_ID)))]
  return(str_def)
}


#----reading control.def----
# this function get definition table of controllers (control.def)
.get_control_def <- function(control.def.f = NULL){
  ct_def <- fread(control.def.f, sep = "\n", header = FALSE, encoding = 'Latin-1')
  ct_def[, org_line_nr := .I]
  ct_tbl <- ct_def[grepl('^CNTL id .*', V1)]
  # id of the controller
  ct_tbl[, id := str_match(V1, "CNTL id '([^']*)'")[,2]]
  # name of the controller
  ct_tbl[, name := str_match(V1, " nm '([^']*)'")[,2]]
  # controller type
  ct_tbl[, ct := str_match(V1, " ct (\\d) ")[,2]]
  # controlled parameter
  ct_tbl[, ca := str_match(V1, " ca (\\d) ")[,2]]
  # controlled active yes/no
  ct_tbl[, ac := str_match(V1, " ac (\\d) ")[,2]]
  # update frequency
  ct_tbl[, cf := str_match(V1, " cf (\\d{1,}) ")[,2]]
  # trigger active
  ct_tbl[, ta := str_match(V1, " ta (\\d \\d \\d \\d) ")[,2]]
  ct_tbl[is.na(ta), ta := str_match(V1, " ta (\\d) ")[,2]]
  # id of triggers
  ct_tbl[, gi := str_match(V1, " gi ('.+') ao ")[,2]]
  # and (=1) or (=0) relation when using more triggers
  ct_tbl[, ao := str_match(V1, " ao (\\d \\d \\d \\d)")[,2]]
  # dValue / dt
  ct_tbl[, mc := str_match(V1, " mc ([^\\ ]*) ")[,2]]
  # interpolation method
  ct_tbl[, bl := str_match(V1, " bl (\\d) ")[,2]]
  # type of measured parameter
  ct_tbl[, cp := str_match(V1, " cp (\\d) ")[,2]]
  # time lag between controlling parameter and controller parameter
  ct_tbl[, mp := str_match(V1, " mp (\\d) ")[,2]]
  # id of measurement node
  ct_tbl[, ml := str_match(V1, " ml '([^']*)' ")[,2]]
  ct_tbl$V1 <- NULL
  ct_def <- merge(ct_def, ct_tbl, by = 'org_line_nr', all.x = TRUE)
  ct_def[, id := id[1], .(cumsum(!is.na(id)))]
  
  return(ct_def)
}


# Sobek code-type -------------------------------------------------------
# type of structure
.get_str_type <- function(x) {
  if (is.null(x)) return(NA)
  if (is.na(x)) return(NA)
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
    NA
  )
}

# type of flow direction through weir
.get_rt_type <- function(x) {
  if (is.null(x)) return(NA)
  if (is.na(x)) return(NA)
  switch(
    x,
    '0' = 'Both',
    '1' = 'Positive',
    '2' = 'Negative',
    '3' = 'No flow',
    # for pumps
    'dn 1' = 'Upward',
    'dn 2' = 'Downward',
    'dn 3' = 'Both',
    'dn -1' = 'Upward (flow >< branch)',
    'dn -2' = 'Downward (flow >< branch)',
    'dn -3' = 'Both (flow >< branch)',
    NA
  )
}

# type of controller
.get_ct_type <- function(x) {
  if (is.null(x)) return(NA)
  if (is.na(x)) return(NA)
  switch(
    x,
    '0' = 'Time controller',
    '1' = 'Hydraulic controller',
    '2' = 'Interval controller',
    '3' = 'PID controller',
    '4' = 'Relative time controller',
    '5' = 'Relative from value controller',
    NA
  )
}

# type of control parameter
.get_ct_param_type <- function(x) {
  if (is.null(x)) return(NA)
  if (is.na(x)) return(NA)
  switch(
    x,
    '0' = 'Crest level',
    '1' = 'Crest width',
    '2' = 'Gate height',
    '3' = 'Pump capacity',
    '4' = '',
    '5' = 'Bottom level of 2D grid cell',
    NA
  )
}

# type of measured parameters
.get_cp_type <- function(x) {
  if (is.null(x)) return(NA)
  if (is.na(x)) return(NA)
  switch(
    x,
    '0' = 'Water level',
    '1' = 'Discharge',
    '2' = 'Head difference',
    '3' = 'Velocity',
    '4' = 'Flow direction',
    '5' = 'Pressure difference',
    NA
  )
}


# type of trigger
.get_tg_type <- function(x) {
  if (is.null(x)) return(NA)
  if (is.na(x)) return(NA)
  t_type <- switch(x,
                   '0' = 'Time',
                   '1' = 'Hydraulic',
                   '2' = 'Combined',
                   NA)
  return(t_type)
}

# type of trigger parameter
.get_tg_param <- function(x) {
  if (is.null(x)) return(NA)
  if (is.na(x)) return(NA)
  tg_param <- switch(x,
                     '0' = 'Waterlevel at branch location',
                     '1' = 'Head difference over structure',
                     '2' = 'Discharge at branch location',
                     '3' = 'Gate lower edge level',
                     '4' = 'Crest level',
                     '6' = 'Crest width',
                     '6' = 'Waterlevel in retention area',
                     '7' = 'Pressure difference over structure',
                   NA)
  return(tg_param)
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