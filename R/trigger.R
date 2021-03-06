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


#' Transfer a trigger from one case to another
#'
#' This function copies the definition of a trigger in the trigger.def from one case to the other case. Conflict IDs with be changed
#'
#' @param from Name of ogirinal case
#' @param to Name of destination case
#' @param tg.ids IDs of the triggers
#' @param sobek.project Path to sobek project
#' @export
transfer_trigger <- function(
  from,
  to,
  tg.ids,
  sobek.project
) {
  trig_def_from_file <- get_file_path(case.name = from,
                                      sobek.project = sobek.project,
                                      type = 'trigger.def')
  trig_def_to_file <- get_file_path(case.name = to,
                                    sobek.project = sobek.project,
                                    type = 'trigger.def')
  trig_tbl_to_file <- get_file_path(case.name = to,
                                    sobek.project = sobek.project,
                                    type = 'trigger.tbl')
  trig_tbl_to <- fread(trig_tbl_to_file, sep = '\n', header = FALSE,
                    strip.white = FALSE)
  trig_tbl_to[, index := str_match(V1, '^ * (\\d+) ')[,2]]
  trig_tbl_to_maxid <- as.integer(trig_tbl_to[, max(index, na.rm = TRUE)])
  trig_def_from <- .get_trigger_def(trig_def_from_file)
  trig_def_to <- .get_trigger_def(trig_def_to_file)
  trig_ids_to <- unique(trig_def_to$id)
  trig_nms_to <- unique(trig_def_to$nm)
  trig_def_new_list <- list(trig_def_to[, c('V1')])
  tg_ids_list <- vector()
  tg_nms_list <- vector()
  tg_indexes_list <- vector()
  for (i in seq_along(tg.ids)) {
    trig_id_from <- tg.ids[i]
    trig_from <- trig_def_from[id == trig_id_from]
    trig_id_to <- trig_id_from
    trig_nm_from <- trig_from[1, nm]
    trig_nm_to <- trig_nm_from
    if (trig_id_from %in% trig_ids_to) {
      while (trig_id_to %in% trig_ids_to) {
        trig_id_to <- paste0(trig_id_to, '_', basename(tempfile(pattern = '')))
      }
    }
    if (trig_nm_from %in% trig_nms_to) {
      while (trig_nm_to %in% trig_nms_to) {
        trig_nm_to <- paste0(trig_nm_to, '_', basename(tempfile(pattern = '')))
      }
      trig_from[1, V1 := str_replace(V1, paste0("nm '", trig_nm_from),
                                     paste0("nm '", trig_nm_to))
                ]
    }
    # storing this new name list to return
    tg_ids_list[i] <- trig_id_to
    tg_nms_list[i] <- trig_nm_to
    tg_indexes_list[i] <- i + trig_tbl_to_maxid
    trig_ids_to <- c(trig_ids_to, trig_id_to)
    trig_nms_to <- c(trig_nms_to, trig_nm_to)
    trig_def_new_list[[i + 1]] <- trig_from[, c('V1')]

  }
  trig_def_new <- rbindlist(trig_def_new_list)
  trig_tbl_new <- data.table(
    V1 = paste(
      stri_pad_left(tg_indexes_list, width = 19),
      stri_pad_right(tg_ids_list, width = 40),
      stri_pad_right(tg_nms_list, width = 40),
      sep = '  ') # two spaces
  )
  trig_tbl_new <- rbind(trig_tbl_to[, c('V1')], trig_tbl_new)
  # backup
  file.copy(trig_def_to_file, paste0(trig_def_to_file, '.BAK'), overwrite = TRUE)
  file.copy(trig_tbl_to_file, paste0(trig_tbl_to_file, '.BAK'), overwrite = TRUE)
  fwrite(
    trig_def_new,
    file = trig_def_to_file,
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE,
    sep = "\n"
  )
  fwrite(
    trig_tbl_new,
    file = trig_tbl_to_file,
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE,
    sep = "\n"
  )
  return(data.table(id_new = tg_ids_list, nm_new = tg_nms_list))
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
get_trigger_info_old <- function(t.id, case.name, sobek.project, html = TRUE) {
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


#' Find out dependencies for one trigger definition
#'
#' This function searches trigger.def to find out how many controllers are using
#' it as their trigger.
trig_dependency <- function(
  tg.id,
  tg.tbl = NULL,
  ct.tbl = NULL,
  tg.def.f = NULL,
  ct.def.f = NULL,
  case.name = NULL,
  sobek.project = NULL
) {
  if (tg.id == '-1' | tg.id == "'-1'") {
    # return empty data.table with three columns
    ret <- data.table(id = NA, ta = NA, gi = NA)
    ret <- ret[!is.na(id)]
    return(ret)
  }
  if (!is.null(case.name)) {
    if (is.null(sobek.project)) stop('case.name and sobek.project must be given together')
    tg.def.f <- get_file_path(case.name, sobek.project, 'trigger.def')
    ct.def.f <- get_file_path(case.name, sobek.project, 'control.def')
  }
  if (!is.null(tg.def.f)) {
    tg.tbl <- .get_trigger_def(tg.def.f)
  } else {
    if (is.null(tg.tbl)) stop('Not enough information for getting trigger table')
  }
  if (!is.null(ct.def.f)) {
    ct.tbl <- .get_control_def(ct.def.f)
  } else {
    if (is.null(ct.tbl)) stop('Not enough information for getting controller table')
  }
  ct.tbl[grepl(paste0("'", tg.id, "'"), gi), dep := 1]
  ret <- ct.tbl[!is.na(dep), c('id', 'ta', 'gi')]
  return(ret)
}
