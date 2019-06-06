#' Get max value table
#' @param case.w list of cases with the measure
#' @param case.w.desc Description of case.w
#' @param case.wo list of cases without the measure
#' @param case.wo.desc Description of case.wo
#' @param id.name Names assign to the IDs
#' @param html.out Output to html tables
#' @param out.dec Output decimal
#' @param param Discharge, waterlevel,...
#' @param ... parameters to pass to function his_from_case
#' @export
#' @return a data.table
get_delta_table <- function(
  name = '',
  case.w = NULL,
  case.w.desc = NULL,
  case.wo = NULL,
  case.wo.desc = NULL,
  id.names = NULL,
  html.out = TRUE,
  out.dec = ",",
  param = 'discharge',
  ...
){
  f_args <- as.list(match.call())
  # OutDec <- getOption('OutDec')
  # on.exit(options(OutDec = OutDec))
  # options(OutDec = out.dec)
  stopifnot(length(case.w) == length(case.wo))
  stopifnot((is.null(case.w.desc) & is.null(case.wo.desc)) |
              (!is.null(case.w.desc) & !is.null(case.wo.desc)))
  if (!is.null(case.w.desc)){
    stopifnot(length(unique(c(case.w.desc, case.wo.desc))) ==
                length(c(case.w.desc, case.wo.desc))
    )
    stopifnot(str_replace_all(case.w.desc, 'mit', '') ==
                str_replace_all(case.wo.desc, 'ohne', ''))
    delta_cols <- str_replace_all(case.w.desc, 'mit', 'delta')
    col_order <- c('Pegel', case.wo.desc, case.w.desc, delta_cols)
  }
  # reading data from sobek
  value_max_with <- his_from_case(case.list = case.w, get.max = TRUE,
                                  param = param, ...)
  value_max_wo <- his_from_case(case.list = case.wo, get.max = TRUE,
                                param = param, ...)
  # changing case names to their description
  if (!is.null(case.w.desc)){
    for (i in seq_along(case.w)) {
      value_max_with[case == case.w[i], case := case.w.desc[i]]
      value_max_wo[case == case.wo[i], case := case.wo.desc[i]]
    }
  }
  # transforming and merging data
  value_max_with <- value_max_with %>% select(-ts) %>%
    melt(id.vars = 'case', variable.name = 'Pegel') %>%
    dcast(Pegel ~ case)
  value_max_wo <- value_max_wo %>% select(-ts) %>%
    melt(id.vars = 'case', variable.name = 'Pegel') %>%
    dcast(Pegel ~ case)
  data_tbl <- merge(value_max_wo, value_max_with, by = 'Pegel', sort = FALSE)
  # calculate delta
  case_cols <- str_replace_all(case.w.desc, 'mit', '')
  for (i in case_cols){
    col_1 <- paste('mit', i, sep = "")
    col_2 <- paste('ohne', i, sep = "")
    delta <- paste('delta', i, sep = "")
    data_tbl[, eval(delta) := get(col_1) - get(col_2)]
  }
  if (!is.null(case.w.desc)){
    setcolorder(data_tbl, c('Pegel', case.wo.desc, case.w.desc, delta_cols))
  }
  # rounding data
  cols <- colnames(data_tbl)[-1]
  if (param == 'waterlevel'){
    data_tbl[, (cols) := round(.SD, 2), .SDcols = cols]
  } else{
    data_tbl[, (cols) := round(.SD), .SDcols = cols]
  }
  
  if (length(id.names) == length(data_tbl$Pegel)){
    data_tbl$Pegel <- id.names
  }
  # exporting to html table
  if (isTRUE(html.out)){
    # formatting decimal mark, make it easy to copy to Excel
    # data_tbl[, (cols) := format(.SD, decimal.mark = out.dec), .SDcols = cols]
    ngroup <- length(case.wo)
    data_tbl <- data_tbl %>% mutate_at(
      vars(-Pegel), ~format(., decimal.mark = out.dec)
    ) %>%
      htmlTable::htmlTable(
        caption = paste('Modellergebnis Tabelle für Maßnahme', name),
        align = 'lr',
        cgroup = c('',
                   paste(f_args$case.wo, '(1)'),
                   paste(f_args$case.w, '(2)'),
                   'Delta (2) - (1)'),
        n.cgroup = c(1, ngroup, ngroup, ngroup)
      )
  }
  return(data_tbl)
}


#' Get max value table
#' @param name Name of the "Maßnahme"
#' @param bezug List of "Bezugszustand" cases
#' @param plan.ohne List of "Planzustand ohne Maßnahme" cases
#' @param plan.mit List of "Planzustand mit Maßnahme" cases
#' @param hwe.list List of HWE (must unique)
#' @param id.name Names assign to the IDs
#' @param html.out Output to html tables
#' @param out.dec Output decimal
#' @param param Discharge, waterlevel,...
#' @param ... parameters to pass to function his_from_case (ID, sobek.project)
#' @export
#' @return a data.table
get_summary_tbl <- function(
  name = '',
  bezug = NULL,
  plan.ohne = NULL,
  plan.mit = NULL,
  hwe.list = NULL,
  id.names = NULL,
  html.out = TRUE,
  out.dec = ",",
  param = 'discharge',
  ...
){
  f_args <- as.list(match.call())
  ncase <- length(hwe.list)
  stopifnot(!is.null(id.names) & !is.null(bezug))
  stopifnot(length(bezug) == length(plan.ohne))
  stopifnot(length(plan.mit) == length(hwe.list))
  stopifnot(length(plan.mit) == length(plan.ohne))
  stopifnot(ncase == length(unique(hwe.list)))
  
  # reading data from sobek
  bezug_tbl <- his_from_case(case.list = bezug, get.max = TRUE,
                             param = param, ...)
  plan_ohne_tbl <- his_from_case(case.list = plan.ohne, get.max = TRUE,
                                 param = param, ...)
  plan_mit_tbl <- his_from_case(case.list = plan.mit, get.max = TRUE,
                                param = param, ...)
  # changing case names to their description
  
  for (i in seq_along(hwe.list)) {
    bezug_tbl[case == bezug[[i]],
              case := paste('bezug', hwe.list[[i]], sep = "_")]
    plan_ohne_tbl[case == plan.ohne[[i]],
                  case := paste('ohne', hwe.list[[i]], sep = "_")]
    plan_mit_tbl[case == plan.mit[[i]],
                 case := paste('mit', hwe.list[[i]], sep = "_")]
  }
  
  # transforming and merging data
  bezug_tbl <- bezug_tbl %>% select(-ts) %>%
    melt(id.vars = 'case', variable.name = 'Pegel') %>%
    dcast(Pegel ~ case)
  plan_ohne_tbl <- plan_ohne_tbl %>% select(-ts) %>%
    melt(id.vars = 'case', variable.name = 'Pegel') %>%
    dcast(Pegel ~ case)
  plan_mit_tbl <- plan_mit_tbl %>% select(-ts) %>%
    melt(id.vars = 'case', variable.name = 'Pegel') %>%
    dcast(Pegel ~ case)
  data_tbl <- merge(bezug_tbl, plan_ohne_tbl, by = 'Pegel', sort = FALSE) %>%
    merge(plan_mit_tbl, by = 'Pegel', sort = FALSE)
  cols <- c('Pegel',
            paste('bezug', hwe.list, sep = '_'),
            paste('ohne', hwe.list, sep = "_"),
            paste('d_ob', hwe.list, sep = "_"),
            paste('mit', hwe.list, sep = "_"),
            paste('d_mb', hwe.list, sep = "_"),
            paste('d_mo', hwe.list, sep = "_")
  )
  # calculate delta
  for (i in seq_along(hwe.list)){
    col_bezug <- paste('bezug', hwe.list[[i]], sep = '_')
    col_ohne <- paste('ohne', hwe.list[[i]], sep = "_")
    col_mit <- paste('mit', hwe.list[[i]], sep = "_")
    delta_ob <- paste('d_ob', hwe.list[[i]], sep = "_")
    delta_mb <- paste('d_mb', hwe.list[[i]], sep = "_")
    delta_mo <- paste('d_mo', hwe.list[[i]], sep = "_")
    data_tbl[, eval(delta_ob) := get(col_ohne) - get(col_bezug)]
    data_tbl[, eval(delta_mb) := get(col_mit) - get(col_bezug)]
    data_tbl[, eval(delta_mo) := get(col_mit) - get(col_ohne)]
  }
  setcolorder(data_tbl, cols)
  # rounding data
  cols <- cols[-1]
  if (param == 'waterlevel'){
    data_tbl[, (cols) := round(.SD, 2), .SDcols = cols]
  } else{
    data_tbl[, (cols) := round(.SD), .SDcols = cols]
  }
  
  if (length(id.names) == length(data_tbl$Pegel)){
    data_tbl$Pegel <- id.names
  }
  # exporting to html table
  if (isTRUE(html.out)){
    # formatting decimal mark, make it easy to copy to Excel
    # data_tbl[, (cols) := format(.SD, decimal.mark = out.dec), .SDcols = cols]
    data_tbl <- data_tbl %>% mutate_at(
      vars(-Pegel), ~format(., decimal.mark = out.dec)
    ) %>%
      htmlTable::htmlTable(
        caption = paste('Modellergebnis Tabelle für Maßnahme', name),
        align = 'lr',
        cgroup = c('',
                   paste(f_args$bezug, '(1)'),
                   paste(f_args$plan.ohne, '(2)'),
                   'Delta (2) - (1)',
                   paste(f_args$plan.mit, '(3)'),
                   'Delta (3) - (1)',
                   'Delta (3) - (2)'
        ),
        
        n.cgroup = c(1, rep(ncase, 6))
      )
  }
  return(data_tbl)
}


#' Get max value table
#' @param name Name of the "Maßnahme"
#' @param zustand1 List of cases for the reference scenario (substrahend)
#' @param zustand2 List of cases for the comparing scenario (minuend)
#' @param hwe.list List of names for cases (must unique)
#' @param id.name Names assign to the IDs
#' @param html.out Output to html tables
#' @param out.dec Output decimal
#' @param param Discharge, waterlevel,...
#' @param ... parameters to pass to function his_from_case (ID, sobek.project)
#' @export
#' @return a data.table/ a html.table
get_delta_tbl <- function(
  name = '',
  zustand1 = NULL,
  zustand2 = NULL,
  hwe.list = NULL,
  id.names = NULL,
  html.out = TRUE,
  out.dec = ",",
  param = 'discharge',
  ...
){
  f_args <- as.list(match.call())
  ncase = length(hwe.list)
  stopifnot(!is.null(id.names) & !is.null(zustand1))
  stopifnot(length(zustand1) == length(zustand2))
  stopifnot(ncase == length(unique(hwe.list)))
  # reading data from sobek
  zustand1_tbl <- his_from_case(case.list = zustand1, get.max = TRUE,
                                param = param, ...)
  zustand2_tbl <- his_from_case(case.list = zustand2, get.max = TRUE,
                                param = param, ...)
  # changing case names to their description
  
  for (i in seq_along(hwe.list)) {
    zustand1_tbl[case == zustand1[[i]],
                 case := paste('Z1', hwe.list[[i]], sep = "_")]
    zustand2_tbl[case == zustand2[[i]],
                 case := paste('Z2', hwe.list[[i]], sep = "_")]
  }
  
  # transforming and merging data
  zustand1_tbl <- zustand1_tbl %>% select(-ts) %>%
    melt(id.vars = 'case', variable.name = 'Pegel') %>%
    dcast(Pegel ~ case)
  zustand2_tbl <- zustand2_tbl %>% select(-ts) %>%
    melt(id.vars = 'case', variable.name = 'Pegel') %>%
    dcast(Pegel ~ case)
  data_tbl <- merge(zustand1_tbl, zustand2_tbl, by = 'Pegel', sort = FALSE)
  cols <- c('Pegel',
            paste('Z1', hwe.list, sep = '_'),
            paste('Z2', hwe.list, sep = "_"),
            paste('delta', hwe.list, sep = "_")
  )
  # calculate delta
  for (i in seq_along(hwe.list)){
    col_Z1 <- paste('Z1', hwe.list[[i]], sep = '_')
    col_Z2 <- paste('Z2', hwe.list[[i]], sep = "_")
    delta <- paste('delta', hwe.list[[i]], sep = "_")
    data_tbl[, eval(delta) := get(col_Z2) - get(col_Z1)]
  }
  setcolorder(data_tbl, cols)
  # rounding data
  cols <- cols[-1]
  if (param == 'waterlevel'){
    data_tbl[, (cols) := round(.SD, 2), .SDcols = cols]
  } else{
    data_tbl[, (cols) := round(.SD), .SDcols = cols]
  }
  
  if (length(id.names) == length(data_tbl$Pegel)){
    data_tbl$Pegel <- id.names
  }
  # exporting to html table
  if (isTRUE(html.out)){
    # formatting decimal mark, make it easy to copy to Excel
    # data_tbl[, (cols) := format(.SD, decimal.mark = out.dec), .SDcols = cols]
    data_tbl <- data_tbl %>% mutate_at(
      vars(-Pegel), ~format(., decimal.mark = out.dec)
    ) %>%
      htmlTable::htmlTable(
        caption = paste('Modellergebnis Tabelle für Maßnahme', name),
        align = 'lr',
        cgroup = c('',
                   paste(f_args$zustand1, '(1)'),
                   paste(f_args$zustand2, '(2)'),
                   'Delta (2) - (1)'
        ),
        
        n.cgroup = c(1, rep(ncase, 3))
      )
  }
  return(data_tbl)
}
