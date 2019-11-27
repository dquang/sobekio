.get_all_trigger <- function(
  case.name = NULL,
  sobek.project = NULL,
  tble = TRUE,
  html = FALSE
) {

  tg_def_f <- get_file_path(case.name = case.name, sobek.project = sobek.project,
                            type = 'trigger.def')
  tg_def <- .get_trigger_def(tg_def_f)
  tg_def[, ty := sapply(ty, .get_tg_type)][, tp := sapply(tp, .get_tg_param)]
  if (tble) {
    tg_def[, tble_line := 0]
    tg_def[grepl(' < *$', V1), tble_line := 1]
  }
  tg_def
}


#' Get information table for one trigger
#'
#' This function read important information of a trigger from the project case.
#'
#' @param tg.id Id of the trigger
#' @param case.name Name of the case
#' @param sobek.project Path to Sobek project
#' @param html If TRUE, return a html table
#'
#' @return a data.table or a html.table
#' @export
get_trigger_info <- function(
  tg.id, 
  tg.tbl = NULL,
  case.name = NULL, 
  sobek.project = NULL,
  html = TRUE,
  tble = TRUE,
  nrow.tg = 10
  ) {
  if (nrow.tg < 1) tble <- FALSE
  if (tg.id == '-1' || tg.id == "'-1'") {
    # empty trigger table
    trig_tbl <- data.table(
      Parameter = c(
        'Trigger ID', 'Trigger name', 'Trigger type', 'Trigger parameter', 
        'Trigger measurement'
      ),
      Value = rep(NA, 5)
    )
    if (tble) {
      trig_tbl <- data.table(
        Parameter = c('Trigger ID', 'Trigger name', 'Trigger type', 'Trigger parameter', 
          'Trigger measurement', 'Triggering table', rep(NA, nrow.tg - 1)),
        Value = rep(NA, 5 + nrow.tg)
      )
    }
    return(trig_tbl)
  }
  if (is.null(tg.tbl)) {
    t_file <- get_file_path(case.name, sobek.project, type = 'trigger.def')
    tg.tbl <- .get_trigger_def(t_file)
  }
  tg.tbl <- tg.tbl[id == tg.id]
  # in case there is no trigger found, return empty standard table
  if (nrow(tg.tbl) == 0) return(get_trigger_info('-1'))
  t_type <- switch(tg.tbl[1, ty],
                   '0' = 'Time',
                   '1' = 'Hydraulic',
                   '2' = 'Combined')
  t_par <- switch(tg.tbl[1, tp],
                  '0' = 'Waterlevel at branch location',
                  '1' = 'Head difference over structure',
                  '2' = 'Discharge at branch location',
                  '3' = 'Gate lower edge level',
                  '4' = 'Crest level',
                  '6' = 'Crest width',
                  '6' = 'Waterlevel in retention area',
                  '7' = 'Pressure difference over structure'
  )
  trig_tbl <- data.table(
    Parameter = c(
      'Trigger ID', 'Trigger name', 'Trigger type', 'Trigger parameter', 
      'Trigger measurement'
    ),
    Value = c(
      tg.tbl[1, id], tg.tbl[1, nm], t_type, t_par, tg.tbl[1, ml]
    )
  )
  if (isTRUE(tble)) {
    trig_tble <- tg.tbl[grepl(" <$", V1), c('V1')]
    colnames(trig_tble) <- 'Value'
    trig_tble[, Parameter := NA]
    nrow_tble <- nrow(trig_tble)
    if (nrow_tble > 0) {
      trig_tble[, c('V2', 'V3', 'V4', 'V5', 'V6', 'V7') := 
                  tstrsplit(str_trim(Value), split = ' ')]
      trig_tble[V3 == 0, V3 := 'OFF'][V3 == 1, V3 := 'ON']
      trig_tble[V4 == 0, V4 := 'OR'][V4 == 1, V4 := 'AND']
      trig_tble[V5 == 0, V5 := '<'][V5 == 1, V5 := '>']
      trig_tble[, V2 := str_replace_all(V2, "'", "")]
      trig_tble[, V2 := str_replace(V2, ";", " ")]
      trig_tble[, Value := paste(V2, V3, V4, V5, V6)]
      trig_tble <- trig_tble[, c('Value')]
      trig_tble[, Parameter := c('Triggering table', rep(NA, nrow_tble - 1))]
      # make the triggering table has exact nrow.ct rows
      if (nrow_tble < nrow.tg) {
        tmp <- data.table(Parameter = rep(NA, nrow.tg - nrow_tble),
                          Value = rep(NA, nrow.tg - nrow_tble))
        trig_tble <- rbind(trig_tble, tmp)
      } else {
        trig_tble <- trig_tble[1:nrow.tg, ]
      }
    } else {
      # emtpy Trigger table
      trig_tble <- rbind(trig_tble,
                       data.table(Parameter = c('Triggering table', rep(NA, nrow.tg - 1)),
                                  Value = rep(NA, nrow.tg)))
    }
    trig_tbl <- rbind(trig_tbl, trig_tble)
  }
  if (isTRUE(html)) {
    trig_tbl <- htmlTable::htmlTable(
      trig_tbl,
      align = 'l',
      caption = paste(
        "Information table of the Trigger:", tg.id),
      tfoot = paste('Case:', case.name)
    )
  }
  return(trig_tbl)
}


#' Get information of a trigger for popover
#' 
#' @param tg.id ID of the controller
#' @param def.file Path to control.def file
#' @param case.name Name of the case (considered if def.file == NULL)
#' @param sobek.project Path to sobek.project (considered if def.file == NULL)
#' @param trigger If TRUE, information about triggers will be given
#' @export
#' @return a list
get_trigger_popover <- function(tg.id = NULL,
                                tg.tbl,
                                tble = TRUE
) {
  if (isTRUE(tg.id == '')) return('')
  if (is.na(tg.id)) return('')
  if (is.null(tg.id)) return('')
  tg_tbl <- tg.tbl[id == tg.id]
  tg_info_list <- c(
    '<strong>Name: </strong>' = tg_tbl$nm[1],
    '<strong>Type: </strong>' = tg_tbl$ty[1],
    '<strong>Parameter: </strong>' = tg_tbl$tp[1],
    '<strong>Measurement: </strong> ' = tg_tbl$ml[1],
    '<strong>Structure: </strong>' = tg_tbl$struct[1],
    '<strong>Check: </strong>' = tg_tbl$chk[1]
  )
  tg_info_tbl <- data.table(Parameter = names(tg_info_list),
                            Value = tg_info_list)
  if (isTRUE(tble)) {
    tg_tble <- tg_tbl[tble_line == 1, c('V1')]
    tg_tble[, V1 := str_replace(V1, "< {0,1}$", "")]
    tg_tble[, V1 := str_replace_all(V1, "'", "")]
    tg_tble[, V1 := str_replace(V1, ";", " ")]
    tg_tble[, c('V2', 'V3', 'V4', 'V5', 'V6', 'V7') := 
                tstrsplit(str_trim(V1), split = ' ')]
    tg_tble[V4 == '0', V4 := 'OFF'][V4 != '0', V4 := 'ON']
    tg_tble[V5 == '0', V5 := 'OR'][V5 != '0', V5 := 'AND']
    tg_tble[V6 == '0', V6 := '>'][V6 != '0', V6 := '<']
    tg_tble[, V1 := paste(V2, V3, V4, V5, V6, V7)]
    tg_tble <- tg_tble[, c('V1')]
    colnames(tg_tble) <- 'Value'
    nrow_tble <- nrow(tg_tble)
    if (nrow_tble > 0) {
      max_row <- 10 # diplay only max. 10 rows
      if (nrow_tble > max_row) {
        tg_info_list[['<strong>Trigger table (first 10 rows):</strong>']] <- ''
      } else {
        tg_info_list[['<strong>Trigger table:</strong>']] <- ''
      }
      max_row <- min(max_row, nrow_tble)
      tg_tble <- tg_tble[1:max_row, ]
      tg_tble[, Parameter := c(rep('', max_row))]
      tg_info_tbl <- data.table(Parameter = names(tg_info_list),
                                Value = tg_info_list)
      tg_info_tbl <- rbind(tg_info_tbl, tg_tble)
    }
  }
  tg_info_tbl <- tg_info_tbl[!is.na(Value)]
  ret <- tg_info_tbl[, paste0(Parameter, Value)]
  ret <- paste(ret, collapse = '<br>')
  return(ret)
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

