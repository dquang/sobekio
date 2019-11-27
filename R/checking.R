#' Checking Frankfurt data transferring from Main to Rhein Model. Both lines should be the same
#' @param main.case Case from Main Model
#' @param rhein.case Case from Rhein Model
#' @param main.prj Main Project folder
#' @param rhein.prj Rhein Project folder
#' @param print.plot Should a plot be printed. Default = FALSE
#' @param check.dat Checking the input from boundary.dat  if TRUE, or checking model result if FASLE.
#' @param fra.main.id ID for Frankfurt Osthafen Pegel in Main Model. Default: p_frankfurt_ost
#' @param fra.rhein.id ID for Frankfurt Osthafen Pegel in Rhein Model. Default: p_frankfurt_ost
#' @export
check_fra <- function(
  main.case = NULL,
  rhein.case = NULL,
  main.prj = "d:/so21302/main2015.lit",
  rhein.prj = "d:/so21302/rhein29a.lit",
  print.plot = FALSE,
  check.dat = NULL,
  fra.main.id = 'p_frankfurt_ost',
  fra.rhein.id = 'p_frankfurt'
){
  stopifnot(!is.null(check.dat))
  q_fra_main <- his_from_case(
    case.list = main.case,
    sobek.project = main.prj,
    param = 'discharge',
    mID = fra.main.id,
    verbose = FALSE
  )
  q_fra_main$case <- NULL
  colnames(q_fra_main) <- c('ts', 'FRA vom Main')
  max_cmp_row <- q_fra_main[,.N]
  if (isTRUE(check.dat)) {
    q_fra_rhein <- get_data_from_id(
      dat.file = get_file_path(
        case.name = rhein.case,
        sobek.project = rhein.prj,
        type = 'bnd.dat'
      ),
      s.id = '66'
    )
    q_fra_rhein[, `66` := as.numeric(`66`)]
  } else{
    q_fra_rhein <- his_from_case(
      case.list = rhein.case,
      sobek.project = rhein.prj,
      param = 'discharge',
      mID = fra.rhein.id,
      verbose = FALSE
    )
    q_fra_rhein$case <- NULL
    q_fra_rhein <- q_fra_rhein[1:max_cmp_row,]
  }
  colnames(q_fra_rhein) <- c('ts', 'FRA im Rhein')
  qt <- merge(q_fra_main, q_fra_rhein, by = 'ts')
  qt <- melt(qt, id.vars = 'ts')
  if (isTRUE(print.plot)) {
    g <- ggplot(qt, aes(x = ts, y = value,
                        color = variable,
                        linetype = variable)
    ) +
      scale_x_datetime() + geom_line(size = 1) +
      theme(legend.position = 'bottom') +
      ggtitle(paste('Comparing Frankfurt-Ost: \n', main.case, 'vs', rhein.case))
    print(g)
  }
  testthat::expect_equivalent(q_fra_main$`FRA vom Main`, q_fra_rhein$`FRA im Rhein`)
  invisible(list(q_fra_rhein, q_fra_main))
}


#' Checking Worms Input
#' @param case.name Case from Rhein Model
#' @param zustand Scenario for this case
#' @param suffix Extra part of the data column ('zeit', 'regl', 'ereig'...)
#' @param sobek.project Rhein Project folder
#' @param worms.tbl Table of Womrs values
#' @param check.dat Default = FALSE. That means it checks model output vs column in the worms.tbl by default. Set this parameter to TRUE to compare worms.tbl with boundary.dat
#' @param worms.id Worms ID in boundary.dat, default 17
#' @param mid Worms of pegel Worms in Sobek, for getting result from model. Default 'p_worms'
#' @export
check_worms <- function(
  case.name,
  case.desc = case.name,
  zustand = NULL,
  zielpegel = NULL,
  vgf = NULL,
  suffix = '.*',
  sobek.project = so_prj,
  worms.tbl = lubw,
  check.dat = FALSE,
  worms.id = '17',
  lubw.code = lubw_code,
  mid = 'p_worms'
){
  case_desc <- parse_case(case.desc = case.desc, orig.name = case.name)
  if (is.null(zustand)) {
    zustand <- match.arg(case_desc$zustand[1], lubw_code$zustand)
  }
  if (is.null(zielpegel)) {
    zielpegel <- match.arg(case_desc$zielpegel[1], toupper(lubw_code$zielpegel))
  } 
  if (is.null(vgf)) {
    vgf <- match.arg(case_desc$vgf[1], lubw_code$vgf)
  }
  case_pat <- paste(zustand, zielpegel, vgf, sep = '_')
  data_col <- lubw_code[case_name %in% case_pat, colname]
  if (length(data_col) == 0) {
    stop('There is no data column for this case: ', case.name)
  } else if (length(data_col) > 1) {
    data_col <- grep(suffix, data_col, value = TRUE)
    if (length(data_col) != 1) {
      stop('Too many data columns for this case: ', case.name, '. Forgot suffix?')
    }
  }
  qt_lubw <- worms.tbl[, .SD, .SDcols = c('ts', data_col)]
  colnames(qt_lubw) <- c('ts', 'Worms_LUBW')
  if (isTRUE(check.dat)) {
  # reading from boundary.dat
    qt_dat <- get_data_from_id(
      dat.file = get_file_path(
        case.name = case.name,
        sobek.project = sobek.project,
        type = 'bnd.dat'
      ),
      s.id = worms.id
    )
    colnames(qt_dat) <- c('ts', 'Worms_LUBW')
    qt_dat[, Worms_LUBW := as.numeric(Worms_LUBW)]
    setkey(qt_dat, ts)
    testthat::expect_equal(qt_lubw, qt_dat)
    return(TRUE)
  } else {
    qt_lubw[, hwe := year(ts)][hwe == 2002, hwe := 2003]
    qt_mod <- his_from_case(case.list = case.name, sobek.project = sobek.project,
                            mID = mid, param = 'discharge')
    qt <- merge(qt_mod, qt_lubw, by = 'ts')
    qt_cond <- all(near(qt$Worms_LUBW, qt[[mid]], 0.1))
    stopifnot(qt_cond)
    return(TRUE)
  }
}
