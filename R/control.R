#' Get information of a controller
#'
#' @param ct.id ID of the controller
#' @param ct.tbl Controller table
#' @param case.name Name of the case (considered if ct.tbl == NULL)
#' @param sobek.project Path to sobek.project (considered if ct.tbl == NULL)
#' @param trigger If TRUE, information about triggers will be given
#' @export
#' @return a list
get_control_info <- function(ct.id = NULL,
                             ct.tbl = NULL,
                             tg.tbl = NULL,
                             case.name = NULL,
                             sobek.project = NULL,
                             trigger = TRUE,
                             tble = TRUE,
                             html = TRUE,
                             nrow.ct = 10,
                             nrow.tg = 5
                             ) {
  if (nrow.ct < 1) tble <- FALSE
  if (nrow.tg < 1) trigger <- FALSE
  if (ct.id == '-1' || ct.id == "'-1'") {
    ct_info_tbl <- data.table(
      Parameter = c(
        'Controller ID',
        'Controller name',
        'Controller type',
        'Controller parameter',
        'Controller activated',
        'Controller measurement',
        'Measured parameter',
        'Time lag',
        'Update frequency',
        'Trigger activated',
        'Trigger IDs',
        'dValue/dt'
      ),
      Value = rep(NA, 12)
    )
    if (tble) {
      ct_info_tbl <- rbind(
        ct_info_tbl,
        data.table(
          Parameter = c('Controlling table', rep(NA, nrow.ct - 1)),
          Value = rep(NA, nrow.ct)
                           ))
    }
    if (trigger) {
      trig_tbl <- rbindlist(
        lapply(c('-1', '-1', '-1', '-1'),
               get_trigger_info,
               tble = TRUE,
               nrow.tg = nrow.tg))
      ct_info_tbl <- rbind(ct_info_tbl, trig_tbl)
    }
    return(ct_info_tbl)
  }
  if (is.null(ct.tbl)) {
    ct_def_f <- get_file_path(case.name, sobek.project, type = 'control.def')
    ct.tbl <- .get_control_def(ct_def_f)
  }
  if (is.null(tg.tbl)) {
    tg_def_f <- get_file_path(case.name, sobek.project, type = 'trigger.def')
    tg.tbl <- .get_trigger_def(tg_def_f)
  }
  # in case there is no controller found, return standard table
  if (nrow(ct.tbl[id == ct.id]) == 0) return(get_control_info('-1'))
  ct_id_tbl <- ct.tbl[id == ct.id][1, ]
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
  ct_info_tbl <- data.table(Parameter = names(ct_info_list),
                            Value = ct_info_list)
  if (tble) {
    ct_tble <- ct.tbl[id == ct.id & grepl(' < {0,1}$', V1), c('V1')]
    colnames(ct_tble) <- 'Value'
    ct_tble[, Parameter := NA]
    nrow_tble <- nrow(ct_tble)
    if (nrow_tble > 0) {
      ct_tble[, Value := str_replace(Value, "< {0,1}$", "")]
      ct_tble[, Value := str_replace_all(Value, "'", "")]
      ct_tble[, Value := str_replace(Value, ";", " ")]
      ct_tble[, Parameter := c('Controlling table', rep(NA, nrow_tble - 1))]
      # make the controlling table has exact nrow.ct rows
      if (nrow_tble < nrow.ct) {
        tmp <- data.table(Parameter = rep(NA, nrow.ct - nrow_tble),
                          Value = rep(NA, nrow.ct - nrow_tble))
        ct_tble <- rbind(ct_tble, tmp)
      } else {
        ct_tble <- ct_tble[1:nrow.ct, ]
      }
    } else {
      # empty controlling table
      ct_tble <- rbind(ct_tble,
                       data.table(Parameter = c('Controlling table',
                                                rep(NA, nrow.ct - 1)),
                                  Value = rep(NA, nrow.ct)))
    }
    ct_info_tbl <- rbind(ct_info_tbl, ct_tble)
  }
  r_group <- c("Structure Information")
  n_group <- nrow(ct_info_tbl) # Number of rows for "Structure information"
  if (isTRUE(trigger)) {
    # Sobek always configure 4 triggers, not like controllers: 1 or 4
    trig_all <- str_match(
      ct_info_tbl[Parameter == 'Trigger IDs', Value],
      "'([^']+)' '([^']+)' '([^']+)' '([^']+)'"
    )[1, 2:5] %>% sort()
    trig_all[is.na(trig_all)] <- "'-1'"
    if (length(trig_all) < 4) {
      for (i in seq.int(length(trig_all) + 1, 4, 1)) {
        trig_all[i] <- "'-1'"
      }
    }
    trig_tbl <- rbindlist(lapply(trig_all, get_trigger_info,
                                 tg.tbl = tg.tbl,
                                 case.name = case.name,
                                 sobek.project = sobek.project,
                                 html = FALSE,
                                 tble = trigger,
                                 nrow.tg = nrow.tg)
                          )
    ct_info_tbl <- rbind(ct_info_tbl, trig_tbl)
  }
  if (isTRUE(html)) {
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


#' Get information of a controller for popover
#'
#' @param ct.id ID of the controller
#' @param ct.tbl Table of controllers
#' @param tg.tbl Table of triggers
#' @param case.name Name of the case (considered if tg.tbl == NULL)
#' @param sobek.project Path to sobek.project (considered if tg.tbl == NULL)
#' @param trigger If TRUE, information about triggers will be given
#' @export
#' @return a list
get_control_popover <- function(ct.id = NULL,
                                ct.tbl = NULL,
                                tg.tbl = NULL,
                                case.name = NULL,
                                sobek.project = NULL,
                                trigger = FALSE,
                                html = TRUE,
                                tble = FALSE

) {
  if (isTRUE(ct.id == '')) return('')
  if (is.na(ct.id)) return('')
  if (is.null(ct.id)) return('')
  if (is.null(ct.tbl)) {
    ct_def_f <- get_file_path(case.name, sobek.project, type = 'control.def')
    ct.tbl <- .get_control_def(control.def.f = ct_def_f)
  }
  if (is.null(tg.tbl)) {
    tg_def_f <- get_file_path(case.name, sobek.project, type = 'trigger.def')
    tg.tbl <- .get_control_def(tg_def_f)
  }
  ct_id_tbl <- ct.tbl[id == ct.id][1, ]
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
    ct_tble <- ct.tbl[id == ct.id & grepl(' < {0,1}$', V1), c('V1')]
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
                                   tg.tbl = tg.tbl,
                                   case.name = case.name,
                                   sobek.project = sobek.project,
                                   tble = tble,
                                   html = FALSE)
      )
      ct_info_tbl <- rbind(ct_info_tbl, trig_tbl, use.names = TRUE)
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
  st.tbl = NULL,
  tg.tbl = NULL,
  case.name = NULL,
  sobek.project = NULL
) {

  if (!is.null(s.id)) {
    # get path to files
    st_dat_f <- get_file_path(case.name = case.name,
                               sobek.project = sobek.project,
                               type = 'struct.dat')

    st_dat_tbl <- .get_struct_dat(st_dat_f)
    if (s.id %in% st_dat_tbl$id) {
      st_id_tbl <- st_dat_tbl[id == s.id][1,]
      cj_list <- str_split(st_id_tbl$cj, ' ', simplify = TRUE)[1, ]
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
#' @param dvalue.dt Changing speed of the controlled parameter (unit per second)
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
#' change_control_tbl(
#'   tble = guns_ctl_tbl,
#'   ct.id = 'guntersblum_ab',
#'   case.name = 'NurRhein_ZPK_HW1988_Mittel_Nur_Eich_EreigOpt',
#'   sobek.project = 'd:/so21302/rhein.lit'
#' )
#' }
change_control_tbl <- function(
  tble,
  dvalue.dt = NULL,
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
  if (!is.null(dvalue.dt)) {
    stopifnot(is.numeric(as.numeric(dvalue.dt)))
    id_line <- ct_tbl[1, orig_line_nr]
    old_dvalue_dt <- ct_def_tbl[id_line,
                               str_match(V1, ' mc (\\d+\\.*\\d*) ')[, 2]]
    ct_def_tbl[id_line, V1 := str_replace(V1, ' mc \\d+\\.*\\d* ',
                                    paste0(' mc ', dvalue.dt, ' ')
                                    )]
    cat('Old dValue/dt: ', old_dvalue_dt, '. Replaced by: ', dvalue.dt, '\n',
        sep = '')
  }
  tble_begin <- ct_tbl[grepl('TBLE', V1), orig_line_nr]
  tble_end <- ct_tbl[grepl('tble', V1), orig_line_nr]
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


#' Transfer a controller from one case to another
#'
#' This function copies the definition of a controller in the control.def from one case, and paste/replace the controller with same id in the other case. By using this method, all information will be copied (for ex. included dValue/dt)
#'
#' @param from Name of ogirinal case
#' @param to Name of destination case
#' @param ct.id ID of the controller
#' @param sobek.project Path to sobek project
#' @param overwrite Logical, default FASLE, if TRUE the definition of the controller will be overwrite. Be carefull with this option
#' @export
transfer_controller <- function(
  from,
  to,
  ct.ids,
  sobek.project,
  overwrite = FALSE,
  write.def = FALSE
) {
  ct.ids <- unlist(ct.ids)
  if (length(overwrite) == 1) {
    overwrite <- rep(overwrite, length(ct.ids))
  } else {
    if (length(overwrite) != length(ct.ids)) {
      stop('length(overwrite) must be one or same as length(ct.ids)')
    }
  }
  # reading definition files
  ctr_def_from_file <- get_file_path(case.name = from,
                                     sobek.project = sobek.project,
                                     type = 'control.def')
  ctr_def_to_file <- get_file_path(case.name = to,
                                     sobek.project = sobek.project,
                                     type = 'control.def')
  ctr_def_from <- .get_control_def(ctr_def_from_file)
  ctr_def_to <- .get_control_def(ctr_def_to_file)
  ctr_def_to_nms <- ctr_def_to[, unique(name)]
  ctr_def_to_ids <- ctr_def_to[, unique(id)]
  ct_nm_to_list <- vector()
  ct_id_to_list <- vector()
  ct_nm_from_list <- vector()
  i <- 1
  for (ct.id in ct.ids) {
    # table of control difinition for ct.id
    ctrid_from <- ctr_def_from[id == ct.id]
    ct_nm_from <- ctrid_from[1, name]
    ct_id_to <- ct.id
    if (nrow(ctrid_from) == 0) {
      stop('Controller with ID ', ct.id, ' is not found in case: ', from)
    }
    if (is.na(ctrid_from$ta[[1]])) {
      trig_chk <- FALSE
    } else {
      trig_chk <- isTRUE(ctrid_from$ta[[1]] != '0 0 0 0')
    }
    if (trig_chk) stop('This function is not yet supported for controllers with triggers')
    if (isTRUE(overwrite[i])) {
      # logic: we transfer a controller from one case to another
      # we have to make sure that there is no conflict in controller id (ct_id_to)
      # or name (ct_nm_to) in the new destination control.def file
      # we can overwrite with the ct_id_from and ct_nm_from if:
      #   + ct_id_from and ct_nm_from is not already used in the ct_def_to, or
      #   + if they are used, they are used only by the same structure that uses
      #   + ct_id_to and ct_nm_to
      ctrid_to <- ctr_def_to[id == ct.id]
      if (nrow(ctrid_to) > 0) {
        ct_nm_to <- ctrid_to[1, name]
        nm_chk <- ifelse(ct_nm_to == ct_nm_from, FALSE, TRUE)
      } else {
        nm_chk <- TRUE
      }
      # because overwrite == TRUE, we overwrite the ID
      # We want to transfer also the old name to the new def file
      # If there is a naming conflict, we have to solve it
      # using ct_nm_from in the destination control.def
      # check if the ct_nm_from is alread used and if yes, for which controllers
      ct_nm_to <- ct_nm_from
      if (nm_chk) {
        while (ct_nm_to %in% ctr_def_to_nms) {
          ct_nm_to <- paste(ct_nm_from,
                            substr(basename(tempfile(
                              pattern = '', fileext = ''
                            )), 1, 6),
                            sep = '_')
        }
        ctrid_from[, V1 := str_replace(
          V1,
          paste0(" nm '", ct_nm_from),
          paste0(" nm '", ct_nm_to)

        )]
      }
      if (nrow(ctrid_to) > 0) {
        if (ctrid_to$ct[[1]] != ctrid_from$ct[[1]]) {
          warning('Controller with id: ', ct.id,
                  ' has different types in the given cases')
        }
        ctrid_to_begin <- ctrid_to[, min(orig_line_nr)]
        ctrid_to_end <- ctrid_to[, max(orig_line_nr)]
        # it is ok with an empty data.table
        ctr_def_to <- rbind(ctr_def_to[orig_line_nr < ctrid_to_begin],
                            ctrid_from,
                            ctr_def_to[orig_line_nr > ctrid_to_end])
      } else {
        # if nrow(ctrid_to) == 0, there is nothing to remove
        ctr_def_to <- rbind(ctr_def_to, ctrid_from)
      }
    } else {
      while (ct_id_to %in% ctr_def_to_ids) {
        ct_id_to <- paste(ct.id,
                          substr(basename(tempfile(
                            pattern = '', fileext = ''
                          )), 1, 6),
                          sep = '_')
      }
      ct_nm_to <- ct_nm_from
      while (ct_nm_to %in% ctr_def_to_nms) {
        ct_nm_to <- paste(ct_nm_from,
                          substr(basename(tempfile(
                            pattern = '', fileext = ''
                          )), 1, 6),
                          sep = '_')
      }
      # rename id and name of the controller
      ctrid_from[, V1 := str_replace(
        V1,
        paste0(" id '", ct.id),
        paste0(" id '", ct_id_to)

      )]
      ctrid_from[, V1 := str_replace(
        V1,
        paste0(" nm '", ct_nm_from),
        paste0(" nm '", ct_nm_to)

      )]
      ctr_def_to <- rbind(ctr_def_to, ctrid_from)
    }
    ctr_def_to_nms <- c(ctr_def_to_nms, ct_nm_to)
    ctr_def_to_ids <- c(ctr_def_to_ids, ct_id_to)
    ct_id_to_list[i] <- ct_id_to
    ct_nm_to_list[i] <- ct_nm_to
    ct_nm_from_list[i] <- ct_nm_from
    i <- i + 1
  }
  ctr_def_to <- ctr_def_to[, c('V1')]
  if (write.def) {
    fwrite(
      ctr_def_to,
      file = get_file_path(
        case.name = to,
        sobek.project = sobek.project,
        type = 'control.def'
      ),
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE,
      sep = "\n"
    )
    invisible(
      data.table(
        orig_id = ct.ids,
        orig_name = ct_nm_from_list,
        new_id = ct_id_to_list,
        new_name = ct_nm_to_list
      )
    )
  } else {
    return(list(
      def_to = ctr_def_to,
      ct_tbl = data.table(
        orig_id = ct.ids,
        orig_name = ct_nm_from_list,
        new_id = ct_id_to_list,
        new_name = ct_nm_to_list
      )
    ))
  }
}


.get_all_control <- function(
  case.name = NULL,
  sobek.project = NULL,
  ct.tbl = NULL,
  tg.tbl = NULL,
  html = FALSE,
  tble = TRUE
) {
  if (is.null(ct.tbl)) {
    ctr_def_f <- get_file_path(
      case.name = case.name,
      sobek.project = sobek.project,
      'control.def'
    )
    ct.tbl <- .get_control_def(ctr_def_f)
  }
  ctr_tbl <- ct.tbl[grepl(" id '.*'", V1)]
  ctr_tbl[, ct := sapply(ct, .get_ct_type)]
  ctr_tbl[, ca := sapply(ca, .get_ct_param_type)]
  ctr_tbl[, cp := sapply(cp, .get_cp_type)]
  ctr_tbl[, ac := ifelse(ac == '1', 'YES', 'NO')]
  # get table of trigger names
  st_mtx <- str_match(
    ctr_tbl$gi,
    "'([^']+)' '([^']+)' '([^']+)' '([^']+)'")[, -1] %>% as.data.table()
  st_mtx[V1 == '-1', V1 := ''][V2 == '-1', V2 := '']
  st_mtx[V3 == '-1', V3 := ''][V4 == '-1', V4 := '']
  st_mtx[is.na(V1), V1 := ''][is.na(V2), V2 := '']
  st_mtx[is.na(V3), V3 := ''][is.na(V4), V4 := '']
  ctr_tbl[, c('tg1', 'tg2', 'tg3', 'tg4') := st_mtx]
  # get trigger for controller that have only one trigger
  ctr_tbl[!grepl("ta \\d \\d ", V1),
              ct1 := str_match(V1, " gi '([^']*)' ")[, 2]]
  ctr_tbl[is.na(ct1), ct1 := '']
  ctr_tbl <- ctr_tbl[, c(
    # basic
    'id', 'name', 'ct', # type
    'ca', 'ac', # parameter, activated
    'mc', # dValue/dt
    'cp', # type of measurement parameter
    'ml', # reference pegel
    # trigger
    'ta', 'ao', 'tg1', 'tg2', 'tg3', 'tg4')
    ]
  ctr_cols <- c('ID', 'Name', 'Type',
                'Parameter', 'Activated',
                'dValue/dt',
                'Mesurement type',
                'Pegel',
                'Trigger active',
                'Trigger logic',
                'Trigger 1',
                'Trigger 2',
                'Trigger 3',
                'Trigger 4')
  colnames(ctr_tbl) <- ctr_cols
  setorder(ctr_tbl, ID)

  if (html) {
    tg_names <- grep("Trigger \\d",
                     colnames(ctr_tbl), value = TRUE)
    if (is.null(tg.tbl)) {
      tg.tbl <- .get_all_trigger(case.name, sobek.project, tble = tble, html = FALSE)
    }

    for (i in tg_names) {
      tg_hover <- lapply(
        ctr_tbl[[i]],
        get_trigger_popover,
        tg.tbl = tg.tbl,
        tble = tble
      )
      ctr_tbl[[i]] <- cell_spec(
        ctr_tbl[[i]],
        popover = spec_popover2(
          content = tg_hover,
          title = '<strong>Trigger Information</strong>',
          html = TRUE,
          position = 'left'
        )
      )
    }
  }
  return(ctr_tbl)
}


#' Find out dependencies for one controller definition
#'
#' This function finds out structures that use this controller
control_dependency <- function(
  ct.id = NULL,
  ct.nm = NULL,
  ct.tbl = NULL,
  ct.def.f = NULL,
  st.dat.tbl = NULL,
  st.dat.f = NULL,
  case.name = NULL,
  sobek.project = NULL
) {
  if (!is.null(ct.id)) {
    if (ct.id == '-1' | ct.id == "'-1'") {
      # return empty data.table with three columns
      ret <- data.table(st_id = NA, st_ca = NA, st_cj = NA, ct_id = NA)
      ret <- ret[!is.na(st_id)]
      return(ret)
    }
  }
  if (!is.null(case.name)) {
    if (is.null(sobek.project)) stop('case.name and sobek.project must be given together')
    st.dat.f <- get_file_path(case.name, sobek.project, 'struct.dat')
    ct.def.f <- get_file_path(case.name, sobek.project, 'control.def')
  }
  if (!is.null(st.dat.f)) {
    st.dat.tbl <- .get_struct_dat(st.dat.f)
  } else {
    if (is.null(st.dat.tbl)) stop('Not enough information for getting struct.dat table')
  }
  if (!is.null(ct.def.f)) {
    ct.tbl <- .get_control_def(ct.def.f)
  } else {
    if (is.null(ct.tbl)) stop('Not enough information for getting controller table')
  }
  if (is.null(ct.id)) {
    if (is.null(ct.nm)) stop('ct.id or ct.nm must be given')
    ct.id <- ct.tbl[name == ct.nm, id][[1]]
  }
  st.dat.tbl[grepl(paste0("'", ct.id, "'"), cj), dep := 1]
  ret <- st.dat.tbl[!is.na(dep), c('id', 'ca', 'cj')]
  colnames(ret) <- c('st_id', 'st_ca', 'st_cj')
  ret[, ct_id := ct.id]
  return(ret)
}
