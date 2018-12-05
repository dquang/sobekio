#' Export .HIS information for a case
#' @param case.name Name of Sobek case
#' @param sobek.project Sobek Folder
#' @param his.type c("node", "reach", "lateral", "struct", "measstation")
#' @param info c("location", "general")
#' @export
sobek_case_info <- function(
	case.name = NULL,
	sobek.project = NULL,
	his.type = NULL,
	info = NULL
){
	# check SOBEK project
	sobek_cmt <- paste(sobek.project, "caselist.cmt", sep = "/")
	if (!file.exists(sobek_cmt)) {
		stop("Case list or Sobek Caselist.cmt does not exist!")
	}

	# reading SOBEK caselist.cmt
	sobek_clist <- data.table::fread(
		file = sobek_cmt,
		header = FALSE,
		sep = " ",
		quote = "'",
		stringsAsFactors = FALSE,
		blank.lines.skip = TRUE,
		col.names = c("case_number", "case_name")
	)
	case_number <- .get_case_number(case.name = case.name,
																	case.list = sobek_clist)
	if (is.na(case_number)) stop("Case with name: ", case.name,
																	" not found in ", sobek_cmt)
	his_file <- switch(his.type,
										 node = "calcpnt.his",
										 reach = "reachseg.his",
										 lateral = "lateral.his",
										 struct = "struc.his",
										 measstation = "measstat.his")
	his_file <- paste(sobek.project, case_number, his_file, sep = "/")

	if (tolower(info) == "location"){
		tmp <- his_location(his.file = his_file)
		return(tmp)
	} else{
			if (tolower(info) == "general"){
				tmp <- his_info(his.file = his_file)
				return(tmp)
			} else{
				return(NULL)
			}
	}

}

################################################################################
#' Get file path from sobek case name
#' @param case.name Name of the case
#' @param sobek.project Path to Sobek project folder
#' @param type Type of file to get path (lateral, boundary, reach, node, structure...)
#' @result Path to the needed file
#' @export
get_file_path <- function(case.name = NULL,
                          sobek.project = NULL,
                          type = NULL){
  # check SOBEK project
  sobek_cmt <- paste(sobek.project, "caselist.cmt", sep = "/")
  if (!file.exists(sobek_cmt)) {
    stop("Case list or Sobek Caselist.cmt does not exist!")
  }

  # reading SOBEK caselist.cmt
  sobek_clist <- data.table::fread(
    file = sobek_cmt,
    header = FALSE,
    sep = " ",
    quote = "'",
    stringsAsFactors = FALSE,
    blank.lines.skip = TRUE,
    col.names = c("case_number", "case_name")
  )
  case_number <- .get_case_number(case.name = case.name,
                                  case.list = sobek_clist)
  if (is.na(case_number)) stop("Case with name: ", case.name,
                               " not found in ", sobek_cmt)
  his_file <- switch(type,
                     node = "calcpnt.his",
                     reach = "reachseg.his",
                     lateral = "lateral.his",
                     struct = "struc.his",
                     measstation = "measstat.his",
                     bnd.dat = "boundary.dat",
                     lat.dat = "lateral.dat",
                     setting = "settings.dat",
                     trigger = "trigger.def",
                     control = "controll.def"
                     )
  his_file <- paste(sobek.project, case_number, his_file, sep = "/")
  his_file <- ifelse(file.exists(his_file), his_file, NA)

  return(his_file)

}
