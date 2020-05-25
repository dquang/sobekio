cal_delta <- function(
  dta,
  cmp = "zustand",
  grp = c("hwe", "vgf", "zielpegel"),
  orig.value = TRUE,
  parse.case = TRUE,
  lage.name = "lage"
) {
  if (parse.case) {
    case_tbl <- parse_case(dta[, unique(case)])
    case_tbl[, zustand := str_replace(zustand, "Planzustand", "Alle Maßnahmen")]
    case_tbl[, zustand := str_replace(zustand, "Nur *", "Nur ")]
    dta <- melt(dta, id.vars = "case", value.name = "scheitel",
                variable.name = lage.name)
    dta <- merge(dta, case_tbl, by = "case")
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
  dta[, c("group__by", "compare__by", "merge__by",
          "case_desc", "notiz", "case") := rep(NULL, 6)]
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
  setorder(dta_tbl, zustand, hwe, -zielpegel, vgf)
  dta_vgf1 <- dta_tbl[vgf == "VGF1"]
  dta_skl <- dta_tbl[vgf != "VGF1"]
  dta_skl[vgf == "Selten", vgf := "selten2"]
  dta_skl[vgf == "Mittel", vgf := "selten1"]
  dta_tbl <- rbind(dta_vgf1, dta_skl)
  dta_tbl[, hwe := str_remove(hwe, "HW")]
  dta_tbl[zielpegel == "ZPW", zielpegel := "WO"]
  dta_tbl[zielpegel == "ZPK", zielpegel := "KÖ"]
  dta_tbl[, hwe := paste(hwe, vgf, zielpegel, sep = "_")]
  dta_tbl[, hwe := str_remove(hwe, "_VGF1_ZP0")]
  dta_tbl[, c("zielpegel", "vgf") := c(NULL, NULL)]
  dta_tbl
}
