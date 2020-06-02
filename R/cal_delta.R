#' @export
#'
cal_delta <- function(
  dta,
  cmp = "zustand",
  grp = c("hwe", "vgf", "zielpegel"),
  orig.value = TRUE,
  parse.case = TRUE,
  lage.name = "lage",
  reform = parse.case
) {
  if (parse.case) {
    case_tbl <- parse_case(dta[, unique(case)])
    case_tbl[, zustand := str_replace(zustand, "Planzustand", "Alle Maßnahmen")]
    case_tbl[, zustand := str_replace(zustand, "Nur *", "Nur ")]
    dta <- merge(dta, case_tbl, by = "case")
  }
  if (reform) {
    id_vars <- colnames(dta)
    id_vars <- id_vars[id_vars %in% c("case", "case_desc", "zustand",
                                      "zielpegel", "hwe", "vgf", "notiz")]
    dta <- melt(dta, id.vars = id_vars, value.name = "scheitel",
                variable.name = lage.name)
  }
  dta[, group__by := do.call(paste, .SD), .SDcols = grp]
  dta[, compare__by := do.call(paste, .SD), .SDcols = cmp]
  dta[, merge__by := paste(compare__by, group__by)]
  grp_vars <- dta[, unique(group__by)]
  cmp_vars <- dta[, unique(compare__by)]
  cmp_vars <- cmp_vars[!grepl("Bezug", cmp_vars)]
  dta_delta <-
    dcast.data.table(dta,
                     get(lage.name) ~ compare__by + group__by,
                     value.var = 'scheitel')
  # data.table set the colname to variable name, change back
  colnames(dta_delta)[grepl("lage.name", colnames(dta_delta))] <- lage.name
  for (a_grp in grp_vars) {
    bezug_col <- paste("Bezugszustand", a_grp, sep = '_')
    for (a_cmp in cmp_vars) {
      cmp_col <- paste(a_cmp, a_grp, sep = '_')
      a_grp_col <- paste(a_cmp, a_grp)
      dta_delta[, eval(a_grp_col) := get(cmp_col) - get(bezug_col)]
      dta_delta[, eval(cmp_col) := NULL]
    }
    dta_delta[, eval(bezug_col) := NULL]
  }
  dta_delta <- melt(
    dta_delta,
    id.vars = lage.name,
    variable.name = 'merge__by',
    value.name = 'delta',
    sort = FALSE
  )
  dta <- merge(
    dta[zustand != "Bezugszustand"],
    dta_delta,
    by = c(lage.name, 'merge__by'),
    all = TRUE,
    sort = FALSE
  )
  dta[, delta := round(delta, 2)]
  col_2_remove <- c("group__by", "compare__by", "merge__by",
                    "case_desc", "notiz", "case")
  col_2_remove <- col_2_remove[col_2_remove %in% colnames(dta)]
  dta[,  c(col_2_remove) := rep(NULL, length(col_2_remove))]
  if (orig.value) {
    col_vec <- c("zustand", "hwe", "zielpegel", "vgf" )
    dta_tbl <- dcast(dta, zustand + hwe + zielpegel + vgf ~ get(lage.name),
                     value.var = c("scheitel", "delta"))
    pegels <- unique(dta[[lage.name]]) %>% str_remove(" \\(.*")
    pegel_cols <- colnames(dta_tbl)[-c(1:4)]
    for (i in seq_along(pegels)) {
      col_vec[(2*i - 1 + 4):(2*i + 4)] <- pegel_cols[grepl(pegels[i], pegel_cols)]
    }
    setcolorder(dta_tbl, col_vec)
  } else {
    dta_tbl <- dcast(dta, zustand + hwe + zielpegel + vgf ~ lage,
                     value.var = "delta")
  }

  dta_tbl
}
