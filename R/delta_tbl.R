#' Get max value table
#' 
#' This function create a table of maximum values for a list of locations,
#' which is given by, i.e mID = c(...) for three group of cases
#' \describe{
#' \item{bezug}{This is the first group of cases, it will be the subtrahend in the Delta columns}
#' \item{plan.ohne}{This is the second group of cases, referred to the scenario without the measure}
#' \item{plan.mit}{like plan.ohne but with the measure }
#' }
#' The variable name of each group will be taken for naming the group in the table.
#' Keep in mind that it does not matter which cases you put in the groups.
#' It is only about the group names, the delta (plan.ohne - bezug), (plan.mit- bezug) and (plan.mit - plan.ohne) 
#' 
#' @param bezug List of "Bezugszustand" cases
#' @param plan.ohne List of "Planzustand ohne Maßnahme" cases
#' @param plan.mit List of "Planzustand mit Maßnahme" cases
#' @param hwe.list List of HWE (must unique)
#' @param agg.ids List of ID-groups to aggregate (sum) results of those IDs, ex. agg.ids = list(c("ID1", "ID2"), c("ID1", "ID2")). This is not applied for waterlevel.
#' @param id.name Names assign to the IDs
#' @param html.out Output to html tables
#' @param out.dec Output decimal
#' @param param Discharge, waterlevel,...
#' @param p.layout Layout of the page for output table.
#' Give 'p' for portrait, everything else will be regconized as landscape. This works only for html.out = TRUE
#' @param sobek.project Path to sobek project
#' @param ... ID Type and List in form of ID_TYPE = ID_LIST, e.x. mID = c('p_koeln)
#' @export
#' @return a data.table
get_summary_tbl <- function(
  bezug = NULL,
  plan.ohne = NULL,
  plan.mit = NULL,
  hwe.list = NULL,
  agg.ids = NULL,
  id.names = NULL,
  html.out = TRUE,
  out.dec = ",",
  param = 'discharge',
  sobek.project = NULL,
  p.layout = 'l',
  ...
){
  f_args <- as.list(match.call())
  id_args <- list(...)
  id_types <- c('MID', 'WID', 'QID', 'LID', 'LATID', 'SID', 'PID', 'TID')
  id_type <- names(id_args)
  if (length(id_args) != 1 | !toupper(id_type) %in% id_types) {
    wrong_param <- as.list(match.call(expand.dots = FALSE))
    print('You may have typos in parameter names! Check following parameters:')
    print(names(wrong_param$...))
    stop("List of IDs must be given and ID_TYPE is one of: ",
         "c('mID', 'wID', 'qID', 'lID', 'latID', 'sID', 'pID', 'tID')"
    )
  }
  n_ids <- length(id_args[[id_type]])
  n_case <-  length(hwe.list)
  stopifnot(!is.null(bezug) & !is.null(plan.ohne))
  stopifnot(length(plan.ohne) == length(plan.mit))
  # stopifnot(n_case == length(plan.ohne))
  stopifnot(n_case == length(unique(plan.ohne)))
  # check aggregating IDs
  if (!is.null(agg.ids)) {
    if (param == 'waterlevel') stop('Aggregating is not applied for waterlevel')
    if (!is.list(agg.ids)) stop('agg.ids must be given in form of list. Ex. agg.ids = list(pegel_1 = c("ID1", "ID2")')
    for (i_d in unlist(agg.ids)) {
      if (!i_d %in% unlist(id_args)) {
        print(id_args)
        stop('one of ID for aggregating is not in the list of IDs')
      }
    }
  }
  # reading data from sobek
  bezug_tbl <- his_from_case(case.list = bezug, get.max = TRUE,
                             sobek.project = sobek.project,
                             ...,
                             param = param)
  plan_ohne_tbl <- his_from_case(case.list = plan.ohne, get.max = TRUE,
                                 sobek.project = sobek.project,
                                 ...,
                                 param = param)
  plan_mit_tbl <- his_from_case(case.list = plan.mit, get.max = TRUE,
                                sobek.project = sobek.project,
                                ...,
                                param = param)
  # aggregating discharge for locations that have more than one IDs
  if (!is.null(agg.ids)) {
    for (loc_ids in agg.ids) {
      # aggregate data to the first column
      bezug_tbl[, eval(loc_ids[1]) := rowSums(.SD, na.rm = TRUE), 
                   .SDcols = loc_ids
                   ]
      plan_ohne_tbl[, eval(loc_ids[1]) := rowSums(.SD, na.rm = TRUE), 
                   .SDcols = loc_ids
                   ]
      plan_mit_tbl[, eval(loc_ids[1]) := rowSums(.SD, na.rm = TRUE), 
                    .SDcols = loc_ids
                    ]
      # delete the rest
      for (col in loc_ids[-1]) {
        bezug_tbl[, eval(col) := NULL]
        plan_ohne_tbl[, eval(col) := NULL]
        plan_mit_tbl[, eval(col) := NULL]
      }
    }
  }
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
  bezug_tbl <- bezug_tbl %>%
    melt(id.vars = 'case', variable.name = 'Pegel') %>%
    dcast(Pegel ~ case)
  plan_ohne_tbl <- plan_ohne_tbl %>%
    melt(id.vars = 'case', variable.name = 'Pegel') %>%
    dcast(Pegel ~ case)
  plan_mit_tbl <- plan_mit_tbl %>%
    melt(id.vars = 'case', variable.name = 'Pegel') %>%
    dcast(Pegel ~ case)
  data_tbl <- merge(bezug_tbl, plan_ohne_tbl, by = 'Pegel', sort = FALSE) %>%
    merge(plan_mit_tbl, by = 'Pegel', sort = FALSE)
  cols <- c('Pegel',
            paste('bezug', hwe.list, sep = '_'),
            paste('ohne', hwe.list, sep = "_"),
            paste('mit', hwe.list, sep = "_"),
            paste('d_ob', hwe.list, sep = "_"),
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
  if (param == 'waterlevel') {
    data_tbl[, (cols) := round(.SD, 2), .SDcols = cols]
  } else{
    data_tbl[, (cols) := round(.SD), .SDcols = cols]
  }
  
  if (length(id.names) == length(data_tbl$Pegel)) {
    data_tbl$Pegel <- id.names
  }
  # exporting to html table
  if (isTRUE(html.out)) {
    # formatting decimal mark, make it easy to copy to Excel
    # data_tbl[, (cols) := format(.SD, decimal.mark = out.dec), .SDcols = cols]
    data_tbl <- data_tbl %>% mutate_at(
      vars(-Pegel), ~format(., decimal.mark = out.dec)
    )
    if (p.layout == 'p') {
      data_tbl <-
        rbind(
          setNames(select(data_tbl, 'Pegel', starts_with('bezug_')), 
                   c('Pegel', hwe.list)
          ),
          setNames(select(data_tbl, 'Pegel', starts_with('ohne_')), 
                   c('Pegel', hwe.list)
          ),
          setNames(select(data_tbl, 'Pegel', starts_with('mit_')), 
                   c('Pegel', hwe.list)),
          setNames(select(data_tbl, 'Pegel', starts_with('d_ob_')), 
                   c('Pegel', hwe.list)),
          setNames(select(data_tbl, 'Pegel', starts_with('d_mb_')), 
                   c('Pegel', hwe.list)),
          setNames(select(data_tbl, 'Pegel', starts_with('d_mo_')), 
                   c('Pegel', hwe.list))
        ) %>% 
        kable() %>%
        kable_styling(c("striped", "bordered")) %>%
        pack_rows(paste(f_args$bezug, '(1)'), 1, n_ids) %>%
        pack_rows(paste(f_args$plan.ohne, '(2)'), n_ids + 1, 2 * n_ids) %>%
        pack_rows(paste(f_args$plan.mit, '(3)'), 2 * n_ids + 1, 3 * n_ids) %>%
        pack_rows('Delta (2) - (1)', 3 * n_ids + 1, 4 * n_ids) %>%
        pack_rows('Delta (3) - (1)', 4 * n_ids + 1, 5 * n_ids) %>%
        pack_rows('Delta (3) - (2)', 5 * n_ids + 1, 6 * n_ids)
      
    } else{
      # landscape layout
      tbl_header <- c('', 
                      bezug = n_case,
                      plan.one = n_case,
                      plan.mit = n_case,
                      delta21 = n_case,
                      delta31 = n_case,
                      delta32 = n_case
      )
      names(tbl_header) <- c('', 
                             paste(f_args$bezug, '(1)'),
                             paste(f_args$plan.ohne, '(2)'),
                             paste(f_args$plan.mit, '(3)'),
                             'Delta (2) - (1)',
                             'Delta (3) - (1)',
                             'Delta (3) - (2)'
      )
      data_tbl <- data_tbl %>% 
        kable(col.names = c('Pegel/Lage', rep(hwe.list, 6))) %>% 
        kable_styling(c("striped", "bordered")) %>%
        add_header_above(header = tbl_header)
    }
  }
  return(data_tbl)
}


#' Get max value table
#'
#' @param zustand1 List of cases for the reference scenario (substrahend)
#' @param zustand2 List of cases for the comparing scenario (minuend)
#' @param hwe.list List of names for cases (must unique)
#' @param agg.ids List of ID-groups to aggregate (sum) results of those IDs, ex. agg.ids = list(c("ID1", "ID2"), c("ID1", "ID2")). This is not applied for waterlevel.
#' @param id.name Names assign to the IDs
#' @param html.out Output to html tables
#' @param out.dec Output decimal
#' @param param Discharge, waterlevel,...
#' @param p.layout Layout of the page for output table.
#' Give 'p' for portrait, everything else will be regconized as landscape. This works only for html.out = TRUE
#' @param sobek.project Path to sobek project
#' @param ID Type and List in form of ID_TYPE = ID_LIST, e.x. mID = c('p_koeln)
#' @export
#' @return a data.table/ a html.table
get_delta_table <- function(
  zustand1 = NULL,
  zustand2 = NULL,
  hwe.list = NULL,
  agg.ids = NULL,
  id.names = NULL,
  html.out = TRUE,
  out.dec = ",",
  param = 'discharge',
  p.layout = 'l',
  sobek.project = NULL,
  ...
){
  f_args <- as.list(match.call())
  id_args <- list(...)
  id_types <- c('MID', 'WID', 'QID', 'LID', 'LATID', 'SID', 'PID', 'TID')
  id_type <- names(id_args)
  if (length(id_args) != 1 | !toupper(id_type) %in% id_types) {
    wrong_param <- as.list(match.call(expand.dots = FALSE))
    print('You may have typos in parameter names! Check following parameters:')
    print(names(wrong_param$...))
    stop("List of IDs must be given and ID_TYPE is one of: ",
         "c('mID', 'wID', 'qID', 'lID', 'latID', 'sID', 'pID', 'tID')"
         )
  }
  n_ids <- length(id_args[[id_type]])
  n_case <-  length(hwe.list)
  stopifnot(!is.null(zustand2) & !is.null(zustand1))
  stopifnot(length(zustand1) == length(zustand2))
  stopifnot(n_case == length(zustand2))
  stopifnot(n_case == length(unique(hwe.list)))
  # check aggregating IDs
  if (!is.null(agg.ids)) {
    if (param == 'waterlevel') stop('Aggregating is not applied for waterlevel')
    if (!is.list(agg.ids)) stop('agg.ids must be given in form of list. Ex. agg.ids = list(pegel_1 = c("ID1", "ID2")')
    for (i_d in unlist(agg.ids)) {
      if (!i_d %in% unlist(id_args)) {
        print(id_args)
        stop('one of ID for aggregating is not in the list of IDs')
      }
    }
  }
  # reading data from sobek
  zustand1_tbl <- his_from_case(case.list = zustand1, get.max = TRUE,
                                ...,
                                # mID = pegel_ID, 
                                sobek.project = sobek.project,
                                param = param
                                )
  zustand2_tbl <- his_from_case(case.list = zustand2, get.max = TRUE,
                                ...,
                                # mID = pegel_ID, 
                                sobek.project = sobek.project,
                                param = param)
  # aggregating discharge for locations that have more than one IDs
  if (!is.null(agg.ids)) {
    for (loc_ids in agg.ids) {
      # aggregate data to the first column
      zustand1_tbl[, eval(loc_ids[1]) := rowSums(.SD, na.rm = TRUE), 
                   .SDcols = loc_ids
                   ]
      zustand2_tbl[, eval(loc_ids[1]) := rowSums(.SD, na.rm = TRUE), 
                   .SDcols = loc_ids
                   ]
      # delete the rest
      for (col in loc_ids[-1]) {
        zustand1_tbl[, eval(col) := NULL]
        zustand2_tbl[, eval(col) := NULL]
      }
    }
  }
  # changing case names to their description
  for (i in seq_along(hwe.list)) {
    zustand1_tbl[case == zustand1[[i]],
                 case := paste('Z1', hwe.list[[i]], sep = "_")]
    zustand2_tbl[case == zustand2[[i]],
                 case := paste('Z2', hwe.list[[i]], sep = "_")]
  }
  # transforming and merging data
  zustand1_tbl <- zustand1_tbl %>%
    melt(id.vars = 'case', variable.name = 'Pegel') %>%
    dcast(Pegel ~ case)
  zustand2_tbl <- zustand2_tbl %>%
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
  if (param == 'waterlevel') {
    data_tbl[, (cols) := round(.SD, 2), .SDcols = cols]
  } else{
    data_tbl[, (cols) := round(.SD), .SDcols = cols]
  }
  
  if (length(id.names) == length(data_tbl$Pegel)) {
    data_tbl$Pegel <- id.names
  }
  # exporting to html table
  if (isTRUE(html.out)) {
    # formatting decimal mark, make it easy to copy to Excel
    # data_tbl[, (cols) := format(.SD, decimal.mark = out.dec), .SDcols = cols]
    data_tbl <- data_tbl %>% mutate_at(
      vars(-Pegel), ~format(., decimal.mark = out.dec)
    ) 
    if (p.layout == 'p') {
      data_tbl <-
        rbind(
          setNames(select(data_tbl, 'Pegel', starts_with('Z1_')), 
                   c('Pegel', hwe.list)
                   ),
          setNames(select(data_tbl, 'Pegel', starts_with('Z2_')), 
                   c('Pegel', hwe.list)
                   ),
          setNames(select(data_tbl, 'Pegel', starts_with('delta_')), 
                   c('Pegel', hwe.list))
        )
      data_tbl <- data_tbl %>% kable() %>%
        kable_styling(c("striped", "bordered")) %>%
        pack_rows(paste(f_args$zustand1, '(1)'), 1, n_ids) %>%
        pack_rows(paste(f_args$zustand2, '(2)'), n_ids + 1, 2 * n_ids) %>%
        pack_rows('Delta (2) - (1)', 2 * n_ids + 1, 3 * n_ids)
      
    } else{
      # landscape layout
      tbl_header <- c('', 
                      zustand1 = n_case,
                      zustand2 = n_case,
                      delta = n_case
      )
      names(tbl_header) <- c('', 
                             paste(f_args$zustand1, '(1)'),
                             paste(f_args$zustand2, '(2)'),
                             'Delta (2) - (1)'
      )
      data_tbl <- data_tbl %>% kable() %>% 
        kable_styling(c("striped", "bordered")) %>%
        add_header_above(header = tbl_header)
    }
  }
  return(data_tbl)
}
