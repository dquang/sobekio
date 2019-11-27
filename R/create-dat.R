#' Extract .DAT header file from case
#' 
#' This functions reads boundary.dat and lateral.dat and extracts header files
#' 
#' @param case.name Name of the case
#' @param sobek.projeckt Path to sobek project
#' @param bnd.file Boundary file (in case there is no sobek project)
#' @param lat.file Lateral file (in case there is no sobek project)
#' @param output Path to store the header file
#' @return a data.table (invisible)
#' @export
extract_header <- function(
  case.name = NULL,
  sobek.project = NULL,
  bnd.file = NULL,
  lat.file = NULL,
  output = '.'
) {
  case_input <- !is.null(case.name) & !is.null(sobek.project)
  file_input <- !is.null(bnd.file) | !is.null(lat.file)
  bnd_header <- NULL
  lat_header <- NULL
  if (!case_input & !file_input) {
    stop('Either a sobek case or a boundary file must be given')
  }
  if (case_input) {
    bnd.file <- get_file_path(
      case.name = case.name, sobek.project = sobek.project, type = 'bnd.dat'
    )
    lat.file <- get_file_path(
      case.name = case.name, sobek.project = sobek.project, type = 'lat.dat'
    )
  }
  dt_str <- NA_character_
  if (!is.null(bnd.file)) {
    if (!file.exists(bnd.file)) stop('bnd.file does not exist')
    bnd_tbl <- get_bnd_tbl(bnd.file)
    id_header_list <- bnd_tbl[bnd_var != 'dt']$id
    bnd_header <- bnd_tbl[id %in% id_header_list, c('V1')]
    if (file.exists(output)) {
      dt_str <- format(Sys.time(), format = "%d%m%Y_%H%M%S")
      # comment above header file
      bnd_comment <- data.table(
        V1 = list(
          paste('* boundary header file created on:', Sys.time()),
          paste('* source file:', bnd.file)
        ))
      bnd_name <- file.path(dirname(output), paste0('bnd_header_', dt_str, '.dat'))
      fwrite(bnd_comment, bnd_name, col.names = FALSE, quote = FALSE)
      fwrite(bnd_header, bnd_name, col.names = FALSE, quote = FALSE, append = TRUE)
    }
  }
  if (!is.null(lat.file)) {
    if (!file.exists(lat.file)) stop('lat.file does not exist')
    lat_tbl <- get_lat_tbl(lat.file)
    id_header_list <- lat_tbl[dt == 'dc lt 0']$id
    lat_header <- lat_tbl[id %in% id_header_list, c('V1')]
    if (file.exists(output)) {
      if (is.na(dt_str)) dt_str <- format(Sys.time(), format = "%d%m%Y_%H%M%S")
      # comment above header file
      lat_comment <- data.table(
        V1 = list(
          paste('* lateral header file created on:', Sys.time()),
          paste('* source file:', lat.file)
        ))
      lat_name <- file.path(dirname(output), paste0('lat_header_', dt_str, '.dat'))
      fwrite(lat_comment, lat_name, col.names = FALSE, quote = FALSE)
      fwrite(lat_header, lat_name, col.names = FALSE, quote = FALSE, append = TRUE)
    }
  }
  invisible(list(bnd = bnd_header, lat = lat_header))
}


#' Read boundary.dat to a table
#' @param bnd.file Path to boundary.dat file
#' @return a data.table
#' @export
get_bnd_tbl <- function(
  bnd.file
) {
  bnd_tbl <- fread(bnd.file, sep = "\n", encoding = 'Latin-1', header = FALSE)
  # removing comment lines
  bnd_tbl <- bnd_tbl[substr(V1, 1, 1) != '*']
  # insert original line ID
  bnd_tbl[, org_line_nr := .I]
  id_tbl <- bnd_tbl[grepl(" id '([^']*)'", V1)]
  id_tbl[, id := str_match(V1, " id '([^']*)'")[, 2]]
  id_tbl[, sobek_module := str_match(V1, "(^[^ ]+) id")[, 2]]
  id_tbl[, type := str_match(V1, " id '.* ty (\\d) ")[, 2]]
  str_mt <- id_tbl[, str_match(V1, " id '.* ([qhu])_ ([wdts]{1,2}) (\\d)")]
  id_tbl$bnd_fun <- str_mt[, 2]
  id_tbl$bnd_var <- str_mt[, 3]
  id_tbl$bnd_ts <- str_mt[, 4]
  # merging info back to bnd_tbl
  bnd_tbl <- merge(bnd_tbl, id_tbl[, -c('V1')], by = 'org_line_nr', all.x = TRUE)
  # assign same id for its lines
  bnd_tbl[, id := id[1], by = .(cumsum(!is.na(id)))]
  
  return(bnd_tbl)
}


#' Read boundary.dat to a table
#' @param bnd.file Path to boundary.dat file
#' @return a data.table
#' @export
get_lat_tbl <- function(
  lat.file
) {
  lat_tbl <- fread(lat.file, sep = "\n", encoding = 'Latin-1', header = FALSE)
  # removing comment lines
  lat_tbl <- lat_tbl[substr(V1, 1, 1) != '*']
  # insert original line ID
  lat_tbl[, org_line_nr := .I]
  id_tbl <- lat_tbl[grepl(" id '([^']*)'", V1)]
  id_tbl[, id := str_match(V1, " id '([^']*)'")[, 2]]
  id_tbl[, sobek_module := str_match(V1, "(^[^ ]+) id")[, 2]]
  id_tbl[, type := str_match(V1, " id '.* lt (-*\\d) dc lt")[, 2]]
  id_tbl[, dt := str_match(V1, " id '.* (dc lt \\d{1,2}) ")[, 2]]
  # merging info back to lat_tbl
  lat_tbl <- merge(lat_tbl, id_tbl[, -c('V1')], by = 'org_line_nr', all.x = TRUE)
  # assign same id for its lines
  lat_tbl[, id := id[1], by = .(cumsum(!is.na(id)))]
  
  return(lat_tbl)
}


#' Create input data for sobek
#' 
#' This function creates boundary.dat and lateral.dat from text data
#' 
#' @param out.case Write the output directly to the sobek case
#' @param out.project Write the output directly to the sobek project
#' @param factor multiplying factor. Default NULL.
#' Set factor to a single number to apply it for all nodes. 
#' If there is an id.tbl given, the factor will be applied only for
#' nodes that have value 1 in the column apply_factor.
#' If factor is set to "from_id". The id.tbl must be given and have at least 4 columns:
#' node_id, col_name (column in data.tbl of the node), apply_factor (value 1/0), and
#' factor (numeric with seperator as given by dec.char)
#' If there is a column named Q0 in the id.tbl, the factor will be applied only if: Q > Q0
#' @param id.tbl Path to ID file
#' @param data.tbl Path to Data file
#' @param in.bnd Path to old boundary.dat as a template
#' @param in.lat Path to old lateral.dat as a template
#' @param in.case extract header file from this sobek case
#' @param in.project extract header file from this sobek project
#' @param output Output folder
#' @param col.sep Column seperator of ID & Input Data files. Default TAB.
#' @param dec.char Decimal character of ID & Input Data files. Default ","
create_bnd_file <- function(
  out.case = NULL,
  out.project = NULL,
  factor = NULL,
  id.tbl = NULL,
  data.tbl = NULL,
  in.bnd = NULL,
  in.lat = NULL,
  in.case = NULL,
  in.project = NULL,
  output = '.',
  col.sep = '\t',
  dec.char = ','
) {
  f_args <- as.list(match.call())
  outDec <- getOption("OutDec")
  options(OutDec = ".")
  on.exit(options(OutDec = outDec))
  factor_check <- FALSE # should factor be applied
  if (!is.null(factor)) {
    factor_check <- TRUE
    if (!is.numeric(factor) & isTRUE(factor != 'from_id')) {
      stop("factor only accept NULL/numeric or 'from_id'")
    }
  } 
  q0_check <- FALSE # should basic discharge be applied
  # check data.tbl
  stopifnot(!is.null(data.tbl))
  if (!is.data.frame(data.tbl)) {
    data_fcheck <- is.character(data.tbl) && length(data.tbl)
    if (isTRUE(data_fcheck)) {
      stopifnot(file.exists(data.tbl))
    } else {
      stop('data.tbl must be given as path to file or a data.frame (data.table)')
    }
    data_file <- data.tbl
    input_data <- fread(data.tbl, dec = dec.char, sep = col.sep, header = TRUE,
                        blank.lines.skip = TRUE)
  } else {
    data_file <- paste(f_args$data.tbl)
    input_data <- as.data.table(data.tbl)
  }
  input_data_cols <- colnames(input_data)
  stopifnot('date' %in% input_data_cols & 'time' %in% input_data_cols)
  input_data[, date := format(as.Date(date,
                                      tryFormats = c("%Y-%m-%d",
                                                     "%Y/%m/%d",
                                                     "%d.%m.%Y",
                                                     "%d-%m-%Y")),
                              format = "%Y/%m/%d"
  )
  ]
  # odering by date & time to avoid errors while simulating
  setorder(input_data, date, time)
  id_tbl <- data.table()
  # reading id.tbl
  if (!is.null(id.tbl)) {
    if (is.character(id.tbl) && length(id.tbl) == 1) {
      stopifnot(file.exists(id.tbl))
      id_tbl <- fread(id.tbl, sep = col.sep, dec = dec.char, header = TRUE,
                      blank.lines.skip = TRUE)
    } else {
      stopifnot(is.data.frame(id.tbl))
      id_tbl <- as.data.table(id.tbl)
    }
    # check id_tbl and change 'factor' column to 'fac' avoid confusing with 
    # factor parameter in data.table
    id_tbl_cols <- tolower(colnames(id_tbl))
    if (isTRUE(factor == 'from_id')) {
      if (!'factor' %in%  id_tbl_cols && !'apply_factor' %in%  id_tbl_cols) {
        stop("if factor = 'from_id' then id.tbl must have a 'factor' column")
      }
    }
    if ('q0' %in% id_tbl_cols) q0_check <- TRUE
    stopifnot('node_id' %in% id_tbl_cols & 'col_name' %in% id_tbl_cols)
    id_tbl_cols[id_tbl_cols == 'factor'] <- 'fac'
    colnames(id_tbl) <- id_tbl_cols
  } else {
    if (isTRUE(factor == 'from_id')) stop("if factor = 'from_id' then id.tbl must be given")
  }
  # reading boundary and lateral templates
  if (is.null(in.bnd) | is.null(in.lat)) {
    if (is.null(in.case) | is.null(in.project)) {
      stop('Not enough information for boundary and lateral template files')
    }
    bnd_tbl <- get_bnd_tbl(
      bnd.file = get_file_path(case.name = in.case, sobek.project = in.project,
                               type = 'bnd.dat')
    )
    lat_tbl <- get_lat_tbl(
      lat.file = get_file_path(case.name = in.case, sobek.project = in.project,
                               type = 'lat.dat')
    )
  } else {
    bnd_tbl <- get_bnd_tbl(bnd.file = in.bnd)
    lat_tbl <- get_lat_tbl(lat.file = in.lat)
  } 
  bnd_header_ids <- bnd_tbl[bnd_var != 'dt']$id
  bnd_dt_ids <- bnd_tbl[bnd_var == 'dt']$id
  bnd_header <- bnd_tbl[id %in% bnd_header_ids, c('V1')]
  lat_header_ids <- lat_tbl[dt == 'dc lt 0']$id
  lat_dt_ids <- lat_tbl[dt == 'dc lt 1']$id
  lat_header <- lat_tbl[id %in% lat_header_ids, c('V1')]
  dt_ids <- data.table(node_id = c(bnd_dt_ids, lat_dt_ids))
  if (nrow(id_tbl) > 0) {
    dt_ids <- merge(dt_ids, id_tbl, by = 'node_id', all.x = TRUE)
    if (nrow(dt_ids[is.na(col_name)]) > 0) {
      stop('There are some node_ids that do not have a col_name in id.tbl.')
    }
  } else {
    dt_ids[, col_name := node_id][, q0 := -Inf][, fac := 1]
    if (is.numeric(factor)) dt_ids[, fac := factor]
  }
  dt_ids_cols <- tolower(colnames(dt_ids))
  # if there was no id.tbl given, initialize some additional columns
  if (!'apply_factor' %in% dt_ids_cols) dt_ids[, apply_factor := 1]
  if (!'q0' %in% dt_ids_cols) dt_ids[, q0 := -Inf]
  if (!'fac' %in% dt_ids_cols) {
    dt_ids[, fac := 1]
  }
  if (!'col_name' %in% dt_ids_cols) dt_ids[, col_name := node_id]
  # if the factor was given, overwrite factor column with factor's value
  if (is.numeric(factor)) {
    dt_ids[apply_factor != 1, fac := 1]
    dt_ids[apply_factor == 1, fac := factor]
  }
  # check if all ID have a column in input_data
  for (n_id in dt_ids$col_name) {
    if (!n_id %in% input_data_cols) stop('There is no data for node: ', n_id)
  }
  # processing boundary
  factor_txt <- 'not applied' # for commenting in output file
  q0_text <- 'not applied (either factor = 1 or Q0 was not defined)'
  if (!is.null(factor)) factor_txt <- factor
  dat_comment <-  data.table(
    V1 = list(
      paste('* DAT file created on:', Sys.time()),
      paste('* input data from:', data_file),
      paste('* factor for all:', factor_txt)
    ))
  bnd_out <- rbind(dat_comment, bnd_header)
  lat_out <- rbind(dat_comment, lat_header)
  for (b_id in bnd_dt_ids) {
    b_id_data_col <- dt_ids[node_id == b_id, col_name]
    b_id_fac <- dt_ids[node_id == b_id, fac]
    if (q0_check) b_id_q0 <- dt_ids[node_id == b_id, q0]
    b_id_col <- input_data[, .(date, time, get(b_id_data_col))]
    if (nrow(b_id_col[!is.na(V3)]) > 0) {
      # removing all NA rows and give a warning
      b_id_col <- b_id_col[!is.na(V3)]
      warning('There are NA rows in data for node: ', b_id, 
              '. Column name: ', b_id_data_col,
              '. They were removed!'
              )
    }
    if (!near(b_id_fac, 1)) {
      if (q0_check) {
        q0_text <- b_id_q0
        b_id_col[V3 >= b_id_q0, V3 := V3 * b_id_fac]
      } else {
        b_id_col[, V3 := V3 * b_id_fac]
      }
    }
    b_id_col[, V1 := paste("'", paste(date, time, sep = ";"),
                           "'", " ", V3, " <", sep = "")]
    b_id_col[, V3 := NULL][, date := NULL][, time := NULL]
    b_id_comment <- data.table(
      V1 = list(
        paste('* for Node:', b_id, '- data column:', b_id_data_col),
        paste('* factor: ', ifelse(factor_check, b_id_fac, factor_txt)),
        paste('* basic discharge: ', q0_text)
    ))
    b_id_start_line <- bnd_tbl[id == b_id][1:2, c('V1')]
    b_id_end_line <- bnd_tbl[id == b_id][.N, c('V1')]
    bnd_out <- rbind(bnd_out,
                        b_id_comment,
                        b_id_start_line,
                        b_id_col,
                        b_id_end_line)
  }
  # processing lateral
  for (b_id in lat_dt_ids) {
    b_id_data_col <- dt_ids[node_id == b_id, col_name]
    b_id_fac <- dt_ids[node_id == b_id, fac]
    if (q0_check) b_id_q0 <- dt_ids[node_id == b_id, q0]
    b_id_col <- input_data[, .(date, time, get(b_id_data_col))]
    if (nrow(b_id_col[!is.na(V3)]) > 0) {
      # removing all NA rows and give a warning
      b_id_col <- b_id_col[!is.na(V3)]
      warning('There are NA rows in data for node: ', b_id, 
              '. Column name: ', b_id_data_col,
              '. They were removed!'
      )
    }
    if (!near(b_id_fac, 1)) {
      if (q0_check) {
        q0_text <- b_id_q0
        b_id_col[V3 >= b_id_q0, V3 := V3 * b_id_fac]
      } else {
        b_id_col[, V3 := V3 * b_id_fac]
      }
    }
    b_id_col[, V1 := paste("'", paste(date, time, sep = ";"),
                           "'", " ", V3, " <", sep = "")]
    b_id_col[, V3 := NULL][, date := NULL][, time := NULL]
    b_id_comment <- data.table(
      V1 = list(
        paste('* for Node:', b_id, '- data column:', b_id_data_col),
        paste('* factor: ', ifelse(factor_check, b_id_fac, factor_txt)),
        paste('* basic discharge: ', q0_text)
      ))
    b_id_start_line <- bnd_tbl[id == b_id][1:2, c('V1')]
    b_id_end_line <- bnd_tbl[id == b_id][.N, c('V1')]
    lat_out <- rbind(lat_out,
                        b_id_comment,
                        b_id_start_line,
                        b_id_col,
                        b_id_end_line)
  }
  # writing output
  if (!is.null(out.case) & !is.null(out.project)) {
    bnd_out_file <- get_file_path(case.name = out.case, sobek.project = out.project,
                                  type = 'bnd.dat')
    lat_out_file <- get_file_path(case.name = out.case, sobek.project = out.project,
                                  type = 'lat.dat')
  } else {
    bnd_out_file <- file.path(output, 'boundary.dat')
    lat_out_file <- file.path(output, 'lateral.dat')
  }
  fwrite(bnd_out, bnd_out_file, col.names = FALSE)
  fwrite(lat_out, lat_out_file, col.names = FALSE)
}


#' Create Sobek Input
#' 
#' This function creates boundary.dat and lateral.dat from time series stored 
#' in text file and header files
#'  
#' 
#' @param id.file Path to ID file
#' @param data.file Path to Data file
#' @param bnd.header Path to boundary header file
#' @param lat.header Path to lateral header file
#' @param factor manificition factor
#' @param bnd.out Output path for Boundary.dat (full file path)
#' @param lat.out Output path for Lateral.dat (full file path)
#' @param col.sep Column seperator of ID & Input Data files. Default TAB.
#' @param dec.char Decimal character of ID & Input Data files. Default ","
#' @param output.case Write output (boundary.dat, lateral.dat) direct to the case.
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
  if (!is.null(output.case)) {
    bnd.out = get_file_path(output.case[[1]],
                            output.case[[2]],
                            type = "bnd.dat"
                            )
    lat.out = get_file_path(output.case[[1]],
                            output.case[[2]],
                            type = "lat.dat"
    )
  }
  if (tolower(bnd.out) == tolower(lat.out)) {
    stop('output for boundary and lateral should be different')
  }
  # checking availability of the files
  if (FALSE %in% file.exists(id.file,
                             data.file
                             )) {
    stop("id and/or data file does not exist")
  }
	if (!is.null(bnd.header)) {
		if (!file.exists(bnd.header)) stop(bnd.header, " does not exist")
	}
	if (!is.null(lat.header)) {
		if (!file.exists(lat.header)) stop(lat.header, " does not exist")
	}
  if (!dir.exists(dirname(bnd.out))) dir.create(dirname(bnd.out), recursive = TRUE)
  if (!dir.exists(dirname(lat.out))) dir.create(dirname(lat.out), recursive = TRUE)
  outDec <- getOption("OutDec")
  options(OutDec = ".")
  on.exit(options(OutDec = outDec))
  node_id     <- fread(file = id.file, header = FALSE,
                       sep = col.sep, dec = dec.char)
  input_data  <- fread(file = data.file, header = TRUE,
                       sep = col.sep, dec = dec.char)
  colnames(input_data)[1:2] <- c("date", "time")
  input_data[, date := format(as.Date(date,
                                    tryFormats = c("%Y-%m-%d",
                                                   "%Y/%m/%d",
                                                   "%d.%m.%Y",
                                                   "%d-%m-%Y")
                                    ),
                            format = "%Y/%m/%d"
                            )
             ]
  input_data <- input_data[order(date, time, decreasing = F), ]
  bnd_h       <- fread(file = bnd.header, header = FALSE,
  										 sep = "\n", quote = ""
  										 )
  lat_h       <- fread(file = lat.header, header = FALSE,
  										 sep = "\n", quote = ""
  										 )
  # start writing boundary.dat with basic input information
  fwrite(data.table(list(paste("* Input data:", data.file),
                         paste("* created on ", Sys.time()),
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

  for (i in node_id$V1) {

    dcol  <- node_id[i, V2]
    nid   <- node_id[i, V3]
    ntype <- tolower(node_id[i, V4])
    nfile <- NULL
    if (ntype == "lateral") nfile <- lat.out
    if (ntype == "boundary") nfile <- bnd.out
    if (is.null(nfile)) {
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
    if (dcol %in% dta_colnames) { # node_id found in data table?
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
                                    "'", " ", V3, " <", sep = "")]
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
#' @return Modified boundary.dat for the rhein.case
#' @export
transfer_fra <- function(
  main.case = '',
  rhein.case = '',
  main.prj = 'd:/so21302/main2015.lit',
  rhein.prj = "d:/so21302/rhein.lit",
  fra.id = 'p_frankfurt_ost'
){
  fra_main <- his_from_case(case.list = main.case,
                            sobek.project = main.prj,
                            param = "discharge",
                            mID = fra.id)
  colnames(fra_main) <- c('ts', 'fra', 'case')
  bnd_file <- get_file_path(case.name = rhein.case,
                            sobek.project = rhein.prj,
                            type = "bnd.dat")
  change_tble(dat.file = bnd_file,
                       s.id = '66',
                       tble = fra_main[, c('ts', 'fra')],
                       output = bnd_file,
                       comments = c('Frankfurt Ost from Main Modell',
                                    main.case))

}


#' Change Worms timeseries based on case naming
#' @param case.list List of case
#' @param zustand Scenario
#' @param ereig Ereignis Optimierung?
#' @param sobek.project Default 'd:/so21302/rhein.lit'
#' @export
change_worms <- function(
  case.list = NULL,
  zustand,
  ereig,
  sobek.project = so_prj
) {
  stopifnot(ereig %in% c(TRUE, FALSE))
  zustand <- match.arg(zustand, choices = c(
    'nur_rhein', 'nur_polder', 'nur_nf', 
    'nur_drv', 'nur_nf', 'bezug', 'plan', 'hist'
  ))
  for (i in case.list) {
    i_lower <- str_to_lower(i)
    dta_zp <- str_extract(i_lower, 'zpk|zpw|zp0')
    dta_vgf <- str_extract(i_lower, 'mittel|selten|vgf1')
    if (ereig) {
      dta_col <- paste(dta_zp, zustand, dta_vgf, 'ereig', sep = '_')
    } else {
      dta_col <- paste(dta_zp, zustand, dta_vgf, sep = '_')
    }
    chk_vgf1 <- isTRUE(dta_vgf == 'vgf1')
    if (chk_vgf1) {
      dta = lubw_vgf1[, .SD, .SDcols = c('ts', dta_col)]
      stopifnot(nrow(dta) == 13514)
    } else {
      dta = lubw[, .SD, .SDcols = c('ts', dta_col)]
      stopifnot(nrow(dta) == 1489)
    }
    change_tble(
      dat.file = get_file_path(case.name = i, 
                               sobek.project = sobek.project, type = 'bnd.dat'),
      s.id = '17',
      tble = dta,
      comments = paste('LUBW:', dta_col)
    )
  }
}