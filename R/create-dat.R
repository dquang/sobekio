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
                       factor = 1L,
                       bnd.out = NULL,
                       lat.out = NULL,
                       col.sep = "\t",
                       dec.char = ","){

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

  if (is.null(bnd.out) && is.null(lat.out)) stop("Please give output names for .dat files")

	# if (!is.null(bnd.header) && !is.null(lat.header)){
	# 	stop("only accept either lat- or bnd.header, not both at the same time")
	# }
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
             col.names = FALSE
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
             col.names = FALSE
             )
      fwrite(list(paste("tble", tolower(substr(nid, 1, 4)), sep = " ")),
             file = nfile,
             append = TRUE,
             col.names = FALSE
             )
    } else {
      warning("Node: ", i, " is not found in data file.")
    }
  }
}

# system.time(creat_data(id.file = "ID_ohne_Worms_FRA.txt",
           # data.file = "inputRhein.txt"))

