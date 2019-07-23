#' Read .HIS information for a case
#' @param case.name Name of Sobek case
#' @param sobek.project Sobek Folder
#' @param his.type c("node", "reach", "lateral", "struct", "measstation")
#' @param info c("location", "general")
#' @return A list of general information or a data.table of location
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
	sobek_clist[, case_name := gsub('"', '', case_name, fixed = TRUE)]
	case_number <- .get_case_number(case.name = case.name,
																	case.list = sobek_clist)
	if (is.na(case_number)) stop("Case with name: ", case.name,
																	" not found in ", sobek_cmt)
	his_file <- switch(type,
	                   node = "CALCPNT.HIS",
	                   reach = "REACHSEG.HIS",
	                   lateral = "QLAT.HIS",
	                   struct = "STRUC.HIS",
	                   measstation = "MEASSTAT.HIS",
	                   bnd.dat = "BOUNDARY.HIS",
	                   lat.dat = "LATERAL.DAT",
	                   setting = "SETTINGS.DAT",
	                   trigger = "TRIGGER.DEF",
	                   trigger.tbl = "TRIGGER.TBL",
	                   control.def = "CONTROL.DEF",
	                   # control.dat = "control.dat",
	                   profile.dat = "PROFILE.DAT",
	                   profile.def = "PROFILE.DEF",
	                   struct.dat = "STRUCT.DAT",
	                   struct.def = "STRUCT.DEF"
	)
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


#' Get file path from sobek case name
#' @param case.name Name of the case
#' @param sobek.project Path to Sobek project folder
#' @param type Type of file to get path (lat.dat, bnd.dat, reach, node, structure...)
#' @details type -> file
#' * bnd.dat: get path to boundary.dat
#' * lat.dat: get path to lateral.dat
#' * reach: get path to reachseg.his
#' * node: get path to calcpnt.his
#' * structure: get path to struc.his
#' * measstation: get path to measstat.his
#' * trigger: get path to trigger.def
#' * control.def: get path to control.def
#' * setting: get path to settings.dat
#' * profile.dat: get path to profile.dat
#' * profile.def: get path to profile.def
#' * struct.dat: get path to struct.dat
#' * struct.def: get path to struct.def
#' @examples
#' get_file_path(case.name = 'Default', sobek.project = 'd:/so21302/rhein.lit', type = 'bnd.dat')
#' @md
#' @return Path to the needed file
#' @export
get_file_path <- function(case.name = NULL,
                          sobek.project = NULL,
                          type = NULL){
  # check SOBEK project
  sobek_cmt <- paste(sobek.project, "CASELIST.CMT", sep = "/")
  if (!file.exists(sobek_cmt)) {
    stop("Case list or Sobek Caselist.cmt does not exist!")
  }
  # reading SOBEK caselist.cmt
  sobek_clist <- fread(
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
                     node = "CALCPNT.HIS",
                     reach = "REACHSEG.HIS",
                     lateral = "QLAT.HIS",
                     struct = "STRUC.HIS",
                     measstation = "MEASSTAT.HIS",
                     bnd.dat = "BOUNDARY.HIS",
                     lat.dat = "LATERAL.DAT",
                     setting = "SETTINGS.DAT",
                     trigger = "TRIGGER.DEF",
                     trigger.tbl = "TRIGGER.TBL",
                     control.def = "CONTROL.DEF",
                     # control.dat = "control.dat",
                     profile.dat = "PROFILE.DAT",
                     profile.def = "PROFILE.DEF",
                     struct.dat = "STRUCT.DAT",
                     struct.def = "STRUCT.DEF"
                     )
  his_file <- paste(sobek.project, case_number, his_file, sep = "/")
  his_file <- ifelse(file.exists(his_file), his_file, NA)
  return(his_file)
}
