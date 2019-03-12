#' Create Sobek Input .DAT from
#' @param id.file Path to ID file
#' @param data.file Path to Data file
#' @param bnd.header Path to header file for boundary
#' @param lat.header Path to header file for Lateral
#' @param factor Vergrößerung Faktor
#' @param bnd.out Output path for Boundary.dat (full file path)
#' @param lat.out Output path for Lateral.dat (full file path)
#' @param col.sep Column seperator of ID & Input Data files. Default TAB.
#' @param dec.char Decimal character of ID & Input Data files. Default ","
#' @param output.case Write output (boundary.dat, lateral.dat) direct to the case.\n
#' Output.case must be a character vector combine (case.name, sobek.project)
#'  like c('case name', 'd:/so21302/main.lit')
#' @details Example for id.file
#'\tabular{llllrr}{
#'Ahr           \tab ahr_mess_fv_q         \tab FLBR id Ahr sc 0 lt 0 dc lt 1 0 0 PDIN 0 0  pdin              \tab Lateral  \tab 1 \tab 100\cr
#'Mai_23        \tab bad_vibel_mess_fv_q   \tab FLBO id Mai_23 st 0 ty 1 q_ dt 1 0 0 PDIN 0 0  pdin           \tab Boundary \tab 1 \tab 100\cr
#'L_10014432    \tab cochem_mess_q         \tab FLBO id Mosel_RE_N_P_P_0 st 0 ty 1 q_ dt 1 0 0 PDIN 0 0  pdin \tab Boundary \tab 1 \tab 100\cr
#'Modau         \tab modau_mess_fv_q       \tab FLBR id Modau sc 0 lt 0 dc lt 1 0 0 PDIN 0 0  pdin            \tab Lateral  \tab 1 \tab 100\cr
#'Mai_Lat_Schwb \tab schwarzbach_mess_fv_q \tab FLBR id Mai_Lat_Schwb sc 0 lt 0 dc lt 1 0 0 PDIN 0 0  pdin    \tab Lateral  \tab 1 \tab 100
#'}
#' @export
create_dat <- function(id.file = "",
                       data.file = "",
                       bnd.header = NULL,
                       lat.header = NULL,
                       factor = 1.0,
                       bnd.out = NULL,
                       lat.out = sub('boundary.dat',
                                     'lateral.dat',
                                     bnd.out,
                                     ignore.case = TRUE,
                                     fixed = TRUE),
                       col.sep = "\t",
                       dec.char = ",",
                       output.case = NULL){
  if (!is.null(output.case)){
    bnd.out = get_file_path(output.case[[1]],
                            output.case[[2]],
                            type = "bnd.dat"
                            )
    lat.out = get_file_path(output.case[[1]],
                            output.case[[2]],
                            type = "lat.dat"
    )
  }
  if (tolower(bnd.out) == tolower(lat.out)){
    stop('output for boundary and lateral should be different')
  }
  # checking availability of the files
  if (FALSE %in% file.exists(id.file,
                             data.file
                             # bnd.header,
                             # lat.header
                             )){
    stop("id and/or data file does not exist")
  }
	if (!is.null(bnd.header)){
		if (!file.exists(bnd.header)) stop(bnd.header, " does not exist")
	}
	if (!is.null(lat.header)){
		if (!file.exists(lat.header)) stop(lat.header, " does not exist")
	}
  if(!dir.exists(dirname(bnd.out))) dir.create(dirname(bnd.out), recursive = TRUE)
  if(!dir.exists(dirname(lat.out))) dir.create(dirname(lat.out), recursive = TRUE)
  outDec <- getOption("OutDec")
  options(OutDec = ".")
  on.exit(options(OutDec = outDec))
  node_id     <- fread(file = id.file, header = FALSE,
                       sep = col.sep, dec = dec.char)
  input_data  <- fread(file = data.file, header = TRUE,
                       sep = col.sep, dec = dec.char)
  colnames(input_data)[1:2] <- c("date", "time")
  input_data[, date:=format(as.Date(date,
                                    tryFormats = c("%Y-%m-%d",
                                                   "%Y/%m/%d",
                                                   "%d.%m.%Y",
                                                   "%d-%m-%Y")
                                    ),
                            format = "%Y/%m/%d"
                            )
             ]
  input_data <- input_data[order(date, time, decreasing = F), ]
  # sep = "\n" make sure that we read all lines in one column
  # file.copy(from = bnd.header,
  #           to = bnd.out,
  #           # recursive = TRUE,
  #           overwrite = TRUE)
  # file.copy(from = lat.header,
  #           to = lat.out,
  #           # recursive = TRUE,
  #           overwrite = TRUE)
  bnd_h       <- fread(file = bnd.header, header = FALSE,
  										 sep = "\n", quote = ""
  										 )
  lat_h       <- fread(file = lat.header, header = FALSE,
  										 sep = "\n", quote = ""
  										 )
  # start writing boundary.dat with basic input information
  fwrite(data.table(list(paste("* Input Datei:", data.file),
                         paste("* create on ", Sys.time()),
                         paste("* header file: ", bnd.header)

                         )
                    ),
         file = bnd.out,
         append = FALSE,
         quote = FALSE,
         col.names = FALSE
         )
  # write the boundary header to the boundary.dat
  fwrite(bnd_h,
         file = bnd.out,
         append = TRUE,
         quote = FALSE,
         col.names = FALSE
         )
  # start writing lateral.dat with basic input information
  fwrite(data.table(list(paste("* Input Datei:", data.file),
                         paste("* create on ", Sys.time()),
                         paste("* header file: ", lat.header)
                         )
                    ),
         file = lat.out,
         append = FALSE,
         quote = FALSE,
         col.names = FALSE
         )
  # write the lateral header to the lateral.dat
  fwrite(lat_h,
         file = lat.out,
         append = TRUE,
         quote = FALSE,
         col.names = FALSE
         )
  setkey(node_id, V1)
  dta_colnames <- colnames(input_data)
  # file.remove(output)

  for (i in node_id$V1){

    dcol  <- node_id[i, V2]
    nid   <- node_id[i, V3]
    ntype <- tolower(node_id[i, V4])
    nfile <- NULL
    if (ntype == "lateral") nfile <- lat.out
    if (ntype == "boundary") nfile <- bnd.out
    if (is.null(nfile)){
      warning("node ", i, " is neither Lateral nor Boundary")
      next
    }

    basis_q  <- node_id[i, V6] # Basisabfluss
    # above_zp <- node_id[i, ]$V5 # Above Ziel Pegel
    faktor   <- ifelse(
      node_id[i, ]$V5 == 1, # Above Ziel Pegel?
      factor,      # Yes, then take the faktor
      1L)                   # No, set faktor = 1
    # if
    if (dcol %in% dta_colnames){ # node_id found in data table?
      # first write the sobek node string
      fwrite(data.table(list(paste("* factor =", faktor),
                             paste("* Basisabfluss =", basis_q),
                             # paste("* Input Datei Pfad =", data.file),
                             nid,
                             "TBLE"
                             )
                        ),
             file = nfile,
             append = TRUE,
             col.names = FALSE,
             quote = FALSE
             )
      this_col <- input_data[, .(date, time, get(dcol))]
      this_col <- this_col[!is.na(V3), ] # removing all NA rows
      if (all(is.na(this_col$V3))) warning(dcol, " are all NA. This may lead to a problem in SOBEK")
      this_col[V3 >= basis_q, V3 := V3 * faktor]
      this_col[, txt_2_write := paste("'", paste(date, time, sep = ";"),
                                    "'", " ", V3, " <", sep ="")]
      fwrite(this_col[, .(txt_2_write)],
             file = nfile,
             append = TRUE,
             col.names = FALSE,
             quote = F
             )
      fwrite(list(paste("tble", tolower(substr(nid, 1, 4)), sep = " ")),
             file = nfile,
             append = TRUE,
             col.names = FALSE,
             quote = F
             )
    } else {
      warning("Node: ", i, " is not found in data file.")
    }
  }
}


#' Change Rhein Modell Boundary 'Frankfurt Ost (66) by ts from Main Output
#' This function is to extract Main Model output at Frankfurt Osthafen
#' and paste it as input to Rhein Model (boundary node '66')
#' @param main.case Name of case in Main Model
#' @param rhein.case Name of case in Rhein Model
#' @param main.prj Path to Main Model (default d:/so21302/main2015.lit)
#' @param rhein.prj Path to Rhein Model (default d:/so21302/rhein29a.lit)
#' @result Modified boundary.dat for the rhein.case
#' @export
transfer_fra <- function(
  main.case = '',
  rhein.case = '',
  main.prj = 'd:/so21302/main2015.lit',
  rhein.prj = "d:/so21302/rhein29a.lit"
){
  fra_main_id <- 'Pegel_F-Ost'
  fra_main <- his_from_case(case.list = main.case,
                            sobek.project = main.prj,
                            param = "discharge",
                            mID = fra_main_id)
  colnames(fra_main) <- c('ts', 'fra', 'case')
  # worms_pz27_lubw[, ts:=strptime(ts, format = '%d.%m.%Y %H:%M:%S', tz = 'GMT')]
  fra_main[, fra_prn := paste("'",
                              format(ts,
                                     format = '%Y/%m/%d;%H:%M:%S', tz = 'GMT'),
                              "' ",
                              fra,
                              " <",
                              sep = ""
  )
  ]
  bnd_file <- get_file_path(case.name = rhein.case,
                            sobek.project = rhein.prj,
                            type = "bnd.dat")
  sobekio::change_tble(dat.file = bnd_file,
                       s.id = '66',
                       tble = fra_main[, c("fra_prn")],
                       output = bnd_file,
                       comments = c('Frankfurt Ost from Main Modell',
                                    main.case))

}
