#' Checking Frankfurt data transferring from Main to Rhein Model. Both lines should be the same
#' @param main.case Case from Main Model
#' @param rhein.case Case from Rhein Model
#' @param main.prj Main Project folder
#' @param rhein.prj Rhein Project folder
#' @param print.plot Should a plot be printed. Default = FALSE
#' @param check.dat Checking the input from boundary.dat or if TRUE, and checking model result if FASLE.
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
  if (isTRUE(check.dat)){
    q_fra_rhein <- get_data_from_id(
      dat.file = get_file_path(
        case.name = rhein.case,
        sobek.project = rhein.prj,
        type = 'bnd.dat'
      ),
      s.id = '66'
    )
    q_fra_rhein[, `66`:=as.numeric(`66`)]
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
  if (isTRUE(print.plot)){
    g <- ggplot(qt, aes(x = ts, y = value,
                        color = variable,
                        linetype = variable)
    )+
      scale_x_datetime() + geom_line(size = 1) +
      theme(legend.position = 'bottom')+
      ggtitle(paste('Comparing Frankfurt-Ost: \n', main.case, 'vs', rhein.case))
    print(g)
  }
  testthat::expect_equivalent(q_fra_main$`FRA vom Main`, q_fra_rhein$`FRA im Rhein`)
}

#' Checking Worms Input
#' @param case.name Case from Rhein Model
#' @param zustand "Zustand" von Worms (Messung, 'pz27_zpk_mittel', 'pz27_zpk_selten',...)
#' @param sobek.project Rhein Project folder
#' @param worms.tbl Table of Womrs values
#' @param worms.id ID of Worsm boundary node, default 17
#' @export
check_worms <- function(
  case.name = NULL,
  zustand = NULL,
  sobek.project = 'd:/so21302/rhein29a.lit',
  worms.tbl = lubw_ms,
  worms.id = '17'
){

  stopifnot(length(case.name) ==1 & is.character(case.name))
  stopifnot(!c(is.null(case.name), is.null(zustand), is.null(sobek.project),
               is.null(worms.tbl))
            )
  # get zustand column
  zustand_col <- switch(
    tolower(zustand),
    messung = 'Worms_mess_q',
    bz18_zpk_mittel = 'LUBW_04_bz18_zpk_m',
    bz18_zpk_selten = 'LUBW_04_bz18_zpk_s',
    bz18_zpw_mittel = 'LUBW_04_bz18_zpw_m',
    bz18_zpw_selten = 'LUBW_04_bz18_zpw_s',
    pz27_zpk_mittel = 'LUBW_05_pz27_zpk_m',
    pz27_zpk_selten = 'LUBW_05_pz27_zpk_s',
    pz27_zpw_mittel = 'LUBW_05_pz27_zpw_m',
    pz27_zpw_selten = 'LUBW_05_pz27_zpw_s'
    )
  stopifnot(!is.null(zustand_col))
  qt_mod <- lubw_ms[, .SD, .SDcols = c('ts', zustand_col)]
  colnames(qt_mod) <- c('ts', 'Worms')
  # reading from boundary.dat
  qt_dat <- get_data_from_id(
    dat.file = get_file_path(
      case.name = case.name,
      sobek.project = sobek.project,
      type = 'bnd.dat'
    ),
    s.id = worms.id
  )
  colnames(qt_dat) <- c('ts', 'Worms')
  qt_dat[, Worms := as.numeric(Worms)]
  testthat::expect_equal(qt_mod, qt_dat)
}
