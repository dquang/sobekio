#' Get information of a structure
#' 
#' @param st.list List of structure IDs
#' @param case.list List of cases
#' @param case.desc Renaming the cases in the final table (make it short)
#' @param sobek.project Path to sobek project
#' @param html Output to HTML table? Default TRUE
#' @param trigger If TRUE, information about triggers will be given
#' @param control If TRUE, information about controllers will be given
#' @param nrow.ct Number of maximum rows for controlling table 
#' @param nrow.tg Number of maximum rows for triggering table 
#' @export
#' @return a data.table or a HTML object
get_struct_info <- function(
  st.list = NULL,
  case.list = NULL,
  case.desc = case.list,
  sobek.project = NULL,
  html = TRUE,
  trigger = TRUE,
  control = TRUE,
  nrow.ct = 10,
  nrow.tg = 5
){
  str_info_tbl_list <- list()
  for (i in seq_along(case.list)) {
    str_def_f <- get_file_path(case.name = case.list[i],
                               sobek.project = sobek.project,
                               type = 'struct.def')
    st.def <- .get_struct_def(str_def_f)
    str_dat_f <- get_file_path(case.name = case.list[i],
                               sobek.project = sobek.project,
                               type = 'struct.dat')
    st.dat <- .get_struct_dat(str_dat_f)
    if (control) {
      ct_def_f <- get_file_path(case.name = case.list[i], 
                                sobek.project, type = 'control.def')
      ct.tbl <- .get_control_def(ct_def_f)
    } else {
      ct.tbl <- NULL
    }
    if (trigger) {
      tg_def_f <- get_file_path(case.name = case.list[i], 
                                sobek.project, type = 'trigger.def')
      tg.tbl <- .get_trigger_def(tg_def_f)
    } else {
      tg.tbl <- NULL
    }
    str_info_tbl_i <- list()
    for (j in seq_along(st.list)) {
      str_info_tbl_i[[j]] <- .get_struct_info(
        s.id = st.list[j],
        case.name = case.list[i],
        sobek.project = sobek.project,
        html = FALSE,
        trigger = trigger,
        control = control,
        st.def = st.def,
        st.dat = st.dat,
        ct.tbl = ct.tbl,
        tg.tbl = tg.tbl,
        nrow.ct = nrow.ct,
        nrow.tg = nrow.tg,
        na.rm = FALSE
      )
    }
    str_info_tbl_list[[i]] <- rbindlist(str_info_tbl_i)
    colnames(str_info_tbl_list[[i]])[2] <- case.desc[i]
    if (i == 1) {
      str_info_tbl <- str_info_tbl_list[[i]]
    } else {
      str_info_tbl <- cbind(str_info_tbl, 
                            str_info_tbl_list[[i]][, .SD, 
                                                   .SDcols = eval(case.desc[i])]
                            )
    }
  }
  str_info_tbl <- na.omit(str_info_tbl, cols = case.desc)
  str_info_tbl[is.na(Parameter), Parameter := '']
  if (html) {
    # calculating number of rows for each Controller group
    str_info_tbl[, orig_line := .I]
    row_begin <- str_info_tbl[Parameter == 'Controller ID' | 
                                Parameter == 'Trigger ID' |
                                Parameter == 'Structure name', 
                              orig_line]
    row_names <- str_info_tbl[row_begin, Parameter] %>% str_replace(' name| ID', '')
    row_end <- shift(row_begin, -1, fill = nrow(str_info_tbl))
    n_group <- length(row_begin)
    str_info_tbl[, orig_line := NULL]
    str_info_tbl <- kable(str_info_tbl, escape = FALSE) %>%
      kable_styling(c('hover', 'striped'), fixed_thead = TRUE)
    for (i in 1:n_group) {
      str_info_tbl <- str_info_tbl %>%
        pack_rows(row_names[i], row_begin[i], row_end[i])
    }
  }
  invisible(str_info_tbl)
}


#' Get information of a structure
#' 
#' @param s.id ID of the structure
#' @param case.name Name of the case
#' @param sobek.project Path to sobek project
#' @param html Output to HTML table? Default TRUE
#' @param trigger If TRUE, information about triggers will be given
#' @param control If TRUE, information about controllers will be given
#' @export
#' @return a data.table or a HTML object
.get_struct_info <- function(
  s.id = NULL,
  case.name = NULL,
  sobek.project = NULL,
  html = TRUE,
  trigger = TRUE,
  control = TRUE,
  st.def = NULL,
  st.dat = NULL,
  ct.tbl = NULL,
  tg.tbl = NULL,
  nrow.ct = 10,
  nrow.tg = 5,
  na.rm = FALSE
){
  # get path to files
  if (is.null(st.def)) {
    str_def_f <- get_file_path(case.name = case.name,
                               sobek.project = sobek.project,
                               type = 'struct.def')
    st.def <- .get_struct_def(str_def_f)
  }
  if (is.null(st.dat)) {
    str_dat_f <- get_file_path(case.name = case.name,
                               sobek.project = sobek.project,
                               type = 'struct.dat')
    st.dat <- .get_struct_dat(str_dat_f)
  }
  if (is.null(ct.tbl)) {
    ct_def_f <- get_file_path(case.name, sobek.project, type = 'control.def')
    ct.tbl <- .get_control_def(ct_def_f)
  }
  if (is.null(tg.tbl)) {
    tg_def_f <- get_file_path(case.name, sobek.project, type = 'trigger.def')
    tg.tbl <- .get_trigger_def(tg_def_f)
  }
  if (!s.id %in% st.dat$id) {
    stop(s.id, ' not found in struct.dat. Remember that cases are sensitive')
  }
  str_id_tbl <- st.dat[id == s.id][1,]
  str_id_def <- st.def[def_ID == str_id_tbl$def_ID][1,]
  # avoid having NA in name, for html table later
  str_id_tbl[is.na(name), name := '']
  str_id_list <- c(
    "Structure name" = str_id_tbl$name,
    "Structure ID" = s.id,
    'Structure definition ID' = str_id_tbl$def_ID,
    "Structure type" = .get_str_type(str_id_def$def_ty),
    "Crest level" = str_id_def$cl,
    "Crest width" = str_id_def$cw,
    "Control active" = str_id_tbl$ca,
    "Possible flow direction" = .get_rt_type(str_id_def$rt),
    'Total controllers' = 0L
  )
  if (!is.na(str_id_tbl$cj)) {
    ca_list <- str_split(str_id_tbl$ca, ' ', simplify = TRUE)[1, ]
    str_id_list[['Total controllers']] <- length(ca_list[ca_list == '1'])
    cj_list <- str_split(str_id_tbl$cj, ' ', simplify = TRUE)[1, ]
    ct_id_list <- gsub("'", "", cj_list)
    ct_id_list <- ct_id_list[ca_list == '1']
    # for structure that have less than 4 controllers, make sure ct_id_list has always 4 members
    n_ct <- length(ct_id_list)
    if (n_ct < 4) {
      for (i in seq.int(n_ct + 1, 4)) ct_id_list[[i]] <- '-1'
    }
    for (i in 1:4) {
        ct_name <- paste('Controller', i, 'ID: ')
        if (ct_id_list[[i]] != '-1') {
          str_id_list[[ct_name]] <- ct_id_list[[i]]
        } else {
          str_id_list[[ct_name]] <- NA
        }
    }
  } else {
    # we have to make a table that have always 4 controllers, every controllers
    # always have 4 triggers
    ct_id_list <- c('-1', '-1', '-1', '-1')
  }
  str_info_tbl <- data.table(
    Parameter = names(str_id_list),
    Value = unlist(str_id_list)
  )
  if (isTRUE(control)) {
    ct_tbl <- rbindlist(lapply(ct_id_list, get_control_info,
                               case.name = case.name, 
                               sobek.project = sobek.project,
                               tble = control,
                               ct.tbl = ct.tbl,
                               tg.tbl = tg.tbl,
                               nrow.ct = nrow.ct,
                               nrow.tg = nrow.tg,
                               html = FALSE,
                               trigger = trigger))
    str_info_tbl <- rbind(str_info_tbl, ct_tbl)
    if (html) {
      # calculating number of rows for each Controller group
      str_info_tbl[, orig_line := .I]
      n_group <- str_info_tbl[Parameter == 'Controller ID' | Parameter == 'Trigger ID'
                                , orig_line]
      r_group <- unlist(c("Structure Information", 
                          str_info_tbl[n_group, paste0('Infos for ', 
                                                       Parameter, ': ', Value)]
      ))
      n_group <- n_group - 1
      n_group <- n_group - shift(n_group, 1, fill = 0)
      # giving every Controller a color grouping
      gr_tbl <- data.table(r_g = r_group)
      gr_tbl[, gr_color := str_match(r_g, 'Controller ID: (.*)')[, 2]]
      gr_tbl[1, gr_color := 'Struct']
      gr_tbl[, gr_color := gr_color[1], by = .(cumsum(!is.na(gr_color)))]
      gr_tbl[, col_id := .GRP, by = gr_color]
      # there are only max 4 controllers
      color_r_group <- c('none', '#f0f9e8', '#bae4bc', '#7bccc4', '#2b8cbe')[gr_tbl$col_id]
      str_info_tbl[, orig_line := NULL]
    }
  } else {
    if (html) {
      r_group <- c("Structure Information")
      color_r_group <- 'none'
      n_group <- nrow(str_info_tbl) # Number of rows for "Structure information"
    }
  }
  if (na.rm) str_info_tbl <- str_info_tbl[!is.na(Value)]
  if (isTRUE(html)) {
      str_info_tbl <- htmlTable::htmlTable(
        str_info_tbl,
        align = 'l',
        col.rgroup = color_r_group,
        rgroup = r_group,
        n.rgroup = n_group,
        caption = paste(
          "Information table of the structure:", s.id),
        tfoot = paste('Case:', case.name)
      )
  }
  return(str_info_tbl)
}


#' Get list of structure for one case
#' 
#' This functions read information from struct.dat and struct.def then produces
#' a table listing all structures in the case together with their ids, names, definition ids and controllers.
#' 
#' @param case.name Name of the case
#' @param sobek.project Path to sobek project
#' @param html Default TRUE. Export a html table
#' @return a data.table or htmlTable
#' @export
#' @examples
#' \dontrun{
#' case_name <- 'NurRhein_ZPK_HW1988_Mittel'
#' so_prj <- 'd:/so21302/rhein.lit'
#' get_all_struct(
#'   case.name = case_name,
#'   sobek.project = so_prj,
#'   html = FALSE,
#'   output = 'd:/users/YourNameHere/desktop'
#'   ) # output will be file with name struct_info_tbl_xxx.xlsx to desktop
#'}
struct_report <- function(
  st.list = NULL,
  case.list = NULL,
  case.desc = NULL,
  sobek.project = NULL,
  html = TRUE,
  tble = TRUE,
  output = NULL
) {
  html <- isTRUE(html)
  tble <- isTRUE(tble)
  sobek.project <- gsub('\\\\', '/', sobek.project)
  if (!is.null(output)) {
    folder_name <- dirname(output)
    file_name <- basename(output)
    folder_chk <- file_test('-d', output)
    if (folder_chk) {
      # output was given as a path to an existing folder
      file_out <- tempfile(pattern = 'struct_info_tbl_', 
                           tmpdir = output, 
                           fileext = ifelse(html, '.html', '.xlsx')
      )
    } else {
      # output was given as a path to a file
      folder_chk <- file_test('-d', folder_name)
      if (!folder_chk) stop('output path does not exist')
      file_ext <- str_extract(file_name, '\\..+$')
      if (is.na(file_ext)) {
        file_name <- paste0(file_name, ifelse(html, '.html', '.xlsx'))
      }
      file_out <- file.path(folder_name, file_name)
    }
  }
  if (!html) {
    cat('this function is coming soon \n')
  } else {
    rmd_f <- system.file('Rmd/struct_report.Rmd', package = 'sobekio')
    rmd_tmp <- tempfile(pattern = 'struct_info_', fileext = '.Rmd')
    rmd <- fread(file = rmd_f, sep = '\n', quote = "", header = FALSE,
                 strip.white = FALSE)
    case_list <- paste0("'", case.list, "'", collapse = ',\n')
    case_desc <- paste0("'", case.desc, "'", collapse = ',\n')
    st_list <- paste0("'", st.list, "'", collapse = ',')
    rmd[V1 == 'tble', V1 := 
          paste0("tble <- ", ifelse(tble, 'TRUE', 'FALSE'))]
    rmd[V1 == 'case.list', V1 := paste0("case.list <- c(", case_list, ")")]
    rmd[V1 == 'case.desc', V1 := paste0("case.desc <- c(", case_desc, ")")]
    rmd[V1 == 'st.list', V1 := paste0("st.list <- c(", st_list, ")")]
    rmd[V1 == 'sobek.project', V1 := paste0("sobek.project <- '", 
                                            sobek.project, "'")]
    fwrite(
      file = rmd_tmp,
      rmd,
      sep = '\n',
      col.names = FALSE,
      append = FALSE,
      quote = FALSE
    )
    html_tmp <- str_replace(rmd_tmp, 'Rmd$', 'html')
    rmarkdown::render(rmd_tmp, output_format = 'html_document', 
                      output_file = html_tmp)
    if (!is.null(output)) {
      file.copy(from = html_tmp, to = file_out)
      print(paste('and copied to:', file_out))
      html_tmp <- file_out
    }
    browseURL(html_tmp)
  }
}


#' Get list of structure for one case
#' 
#' This functions read information from struct.dat and struct.def then produces
#' a table listing all structures in the case together with their ids, names, definition ids and controllers.
#' 
#' @param case.name Name of the case
#' @param sobek.project Path to sobek project
#' @param html Default TRUE. Export a html table
#' @return a data.table or htmlTable
#' @export
#' @examples
#' \dontrun{
#' case_name <- 'NurRhein_ZPK_HW1988_Mittel'
#' so_prj <- 'd:/so21302/rhein.lit'
#' get_all_struct(
#'   case.name = case_name,
#'   sobek.project = so_prj,
#'   html = FALSE,
#'   output = 'd:/users/YourNameHere/desktop'
#'   ) # output will be file with name struct_info_tbl_xxx.xlsx to desktop
#'}
get_all_struct <- function(
  case.name = NULL,
  sobek.project = NULL,
  html = TRUE,
  tble = TRUE,
  output = NULL
) {
  html <- isTRUE(html)
  tble <- isTRUE(tble)
  if (!is.null(output)) {
    folder_name <- dirname(output)
    file_name <- basename(output)
    folder_chk <- file_test('-d', output)
    if (folder_chk) {
      # output was given as a path to an existing folder
      file_out <- tempfile(pattern = 'struct_info_tbl_', 
                           tmpdir = output, 
                           fileext = ifelse(html, '.html', '.xlsx')
      )
    } else {
      # output was given as a path to a file
      folder_chk <- file_test('-d', folder_name)
      if (!folder_chk) stop('output path does not exist')
      file_ext <- str_extract(file_name, '\\..+$')
      if (is.na(file_ext)) {
        file_name <- paste0(file_name, ifelse(html, '.html', '.xlsx'))
      }
      file_out <- file.path(folder_name, file_name)
    }
  }
  str_dat_tbl <- .get_all_struct(case.name = case.name, 
                                 sobek.project = sobek.project, tble = tble)
  if (!html) {
    if (!is.null(output)) {
      # write output to excel file
      xlsx_wb <- createWorkbook()
      xlsx_sheet <- createSheet(xlsx_wb, sheetName = 'struct_info_tbl')
      addDataFrame(str_dat_tbl, xlsx_sheet, row.names = FALSE,
                   startRow = 5)
      autoSizeColumn(xlsx_sheet, seq.int(11))
      cell_style <- CellStyle(
        xlsx_wb, 
        alignment = Alignment(horizontal = 'ALIGN_CENTER')) + 
        Font(xlsx_wb, heightInPoints = 14,
             color = 'blue', isBold = TRUE)
      title_rows <- createRow(xlsx_sheet, rowIndex = 1:3)
      title_cells <- createCell(title_rows, colIndex = 1)
      t_tbl <- data.table(
        V1 = c('Structure table', 
               paste0('Case name: ', case.name),
               paste0('Sobek project: ', sobek.project)
        )
      )
      addDataFrame(t_tbl, xlsx_sheet, col.names = FALSE, row.names = FALSE)
      for (i in seq.int(3)) {
        setCellStyle(title_cells[[i, 1]], cell_style)
        addMergedRegion(xlsx_sheet, i, i, 1, 11)
      }
      saveWorkbook(xlsx_wb, file = file_out)
    }
    invisible(str_dat_tbl)
  } else {
    rmd_f <- system.file('Rmd/struct_table.Rmd', package = 'sobekio')
    rmd_tmp <- tempfile(pattern = 'struct_info_', fileext = '.Rmd')
    rmd <- read.table(rmd_f, sep = '\n', quote = "", header = FALSE) %>% 
      as.data.table()
    rmd[V1 == 'tble', V1 := 
          paste0("tble <- ", ifelse(tble, 'TRUE', 'FALSE'))]
    rmd[V1 == 'case.name', V1 := paste0("case.name <- '", case.name, "'")]
    rmd[V1 == 'sobek.project', V1 := paste0("sobek.project <- '", 
                                            sobek.project, "'")]
    fwrite(
      file = rmd_tmp,
      rmd,
      sep = '\n',
      col.names = FALSE,
      append = FALSE,
      quote = FALSE
    )
    html_tmp <- str_replace(rmd_tmp, 'Rmd$', 'html')
    rmarkdown::render(rmd_tmp, output_format = 'html_document', 
                      output_file = html_tmp)
    if (!is.null(output)) {
      file.copy(from = html_tmp, to = file_out)
      print(paste('and copied to:', file_out))
      html_tmp <- file_out
    }
    browseURL(html_tmp)
    invisible(str_dat_tbl)
  }
}


.get_all_struct <- function(
  case.name = NULL,
  sobek.project = NULL,
  html = FALSE,
  tble = TRUE
) {
  
  str_dat_f <- get_file_path(
    case.name = case.name, 
    sobek.project = sobek.project,
    'struct.dat'
  )
  str_def_f <- get_file_path(
    case.name = case.name, 
    sobek.project = sobek.project,
    'struct.def'
  )
  str_def_tbl <- .get_struct_def(str_def_f)
  str_def_tbl <- str_def_tbl[grepl(" id '.*'", V1)]
  str_def_tbl[, def_ty := 
                sapply(str_def_tbl$def_ty, .get_str_type)]
  str_def_tbl[, rt := 
                sapply(str_def_tbl$rt, .get_rt_type)]
  str_def_tbl <- str_def_tbl[, c('def_ID', 'def_name', 'def_ty', 'cl', 'cw', 'rt')]
  str_dat_tbl <- .get_struct_dat(str_dat_f)
  str_mtx <- str_match(
    str_dat_tbl$cj,
    "'([^']+)' '([^']+)' '([^']+)' '([^']+)'")[, -1] %>% as.data.table()
  str_mtx[V1 == '-1', V1 := ''][V2 == '-1', V2 := '']
  str_mtx[V3 == '-1', V3 := ''][V4 == '-1', V4 := '']
  str_mtx[is.na(V1), V1 := ''][is.na(V2), V2 := '']
  str_mtx[is.na(V3), V3 := ''][is.na(V4), V4 := '']
  str_dat_tbl[, c('ct1', 'ct2', 'ct3', 'ct4') := str_mtx]
  # get controllers for structure that have only one controller
  str_dat_tbl[!grepl("ca \\d \\d ", V1), 
              ct1 := str_match(V1, " cj '([^']*)' ")[, 2]]
  str_dat_tbl[is.na(ct1), ct1 := '']
  str_dat_tbl <- str_dat_tbl[, c('id', 'name', 'def_ID',
                                 'ct1', 'ct2', 'ct3', 'ct4')]
  str_dat_tbl <- merge(str_dat_tbl, str_def_tbl, by.x = 'def_ID',
                       no.dups = TRUE,
                       by.y = 'def_ID') %>% setkey(NULL) %>% unique()
  str_cols <- c('id',
                'name',
                'def_ty',
                'def_name',
                'cl',
                'cw',
                'rt',
                'ct1',
                'ct2',
                'ct3',
                'ct4')
  str_cols_names <-
    c(
      'ID',
      'Name',
      'Type',
      'Definition name',
      'Crest level',
      'Crest width',
      'Flow direction',
      'Controller 1',
      'Controller 2',
      'Controller 3',
      'Controller 4'
    )
  str_dat_tbl <- str_dat_tbl[, .SD ,.SDcols = str_cols]
  colnames(str_dat_tbl) <- str_cols_names
  setorder(str_dat_tbl, ID)
  
  if (html) {
    ct_tbl <- .get_control_def(get_file_path(case.name, sobek.project, 'control.def'))
    tg_tbl <- .get_trigger_def(get_file_path(case.name, sobek.project, 'trigger.def'))
    ct_names <- grep("Controller \\d", 
                     colnames(str_dat_tbl), value = TRUE)
    for (i in ct_names) {
      ct_hover <- lapply(
        str_dat_tbl[[i]],
        get_control_popover,
        ct.tbl = ct_tbl,
        tg.tbl = tg_tbl,
        html = TRUE,
        tble = tble
      )
      str_dat_tbl[[i]] <- cell_spec(
        str_dat_tbl[[i]],
        popover = spec_popover2(
          content = ct_hover,
          title = '<strong>Controller Information</strong>',
          html = TRUE,
          position = 'left'
        )
      )
    }
  }
  return(str_dat_tbl)
}


#' Turn off Weir(s) / Weir(s)
#'
#' Turn off Weir(s) / Weir(s) by deactivate all controllers and set crest width to 0
#'
#' @param struct Name(s) of the (River) Weir(s)
#' @param case Case name
#' @param sobek.project Path to sobek project
#' @export
set_struct_off <- function(
  struct = NULL,
  case.name = NULL,
  sobek.project = NULL) {
  struct.dat.f <- get_file_path(case.name = case.name, 
                                sobek.project = sobek.project,
                                type = 'struct.dat')
  struct.def.f <- get_file_path(case.name = case.name, 
                                sobek.project = sobek.project,
                                type = 'struct.def')
  struct_dat <- .get_struct_dat(struct.dat.f)
  struct_def <- .get_struct_def(struct.def.f)
  for (i in seq_along(struct)) {
    struct_def_id <- struct_dat[id == struct[[i]], def_ID]
    # deactivate all controllers, prevent time controllers open the structure
    ca_match_patt <- " ca \\d \\d \\d \\d "
    cj_match_patt <- " cj '[^']+' '[^']+' '[^']+' '[^']+' "
    # for structure with 4 controllers
    struct_dat[id == struct[[i]], 
               V1 := str_replace(V1, ca_match_patt, ' ca 0 0 0 0 ')]
    struct_dat[id == struct[[i]], 
               V1 := str_replace(V1, cj_match_patt, " cj '-1' '-1' '-1' '-1' ")]
    # for structure with 1 controllers
    struct_dat[id == struct[[i]], 
               V1 := str_replace(V1, ' ca \\d ', 'ca 0 ')]
    struct_dat[id == struct[[i]], 
               V1 := str_replace(V1, " cj '[^']+' " ," cj '-1' ")]
    
    # change crest-width to 0, no water coming in
    struct_def[def_ID == struct_def_id, 
               V1 := str_replace(V1, ' cw \\S+ ', ' cw 0 ')]
  }
  file.copy(struct.dat.f, paste(struct.dat.f, ".BAK", sep = ""))
  file.copy(struct.def.f, paste(struct.dat.f, ".BAK", sep = ""))
  fwrite(struct_dat[, .SD, .SDcols = c("V1")], struct.dat.f, sep = "\n",
         col.names = FALSE, quote = FALSE)
  fwrite(struct_def[, .SD, .SDcols = c("V1")], struct.def.f, sep = "\n",
         col.names = FALSE, quote = FALSE)
}


#' Turn on one River Weir / Weir
#'
#' Turn on one River Weir / Weir by activate related controllers and set its  characters
#'
#' @param struct Name(s) of the (River) Weir(s)
#' @param cw Struct Crest Width
#' @param ct Struct controller ID(s), ex. c("##114", "##112")
#' @param case Case name
#' @param sobek.project Path to sobek project
#' @export
set_struct_on <- function(
  struct = NULL,
  cw = NULL,
  ct = NULL,
  cl = NULL,
  rt = NULL,
  case.name = NULL,
  sobek.project = NULL) {
  
  struct.dat.f <- get_file_path(case.name = case.name,
                                sobek.project = sobek.project,
                                type = 'struct.dat')
  struct.def.f <- get_file_path(case.name = case.name,
                                sobek.project = sobek.project,
                                type = 'struct.def')
  control.def.f <- get_file_path(case.name = case.name,
                                 sobek.project = sobek.project,
                                 type = 'control.def')
  control_def <- .get_control_def(control.def.f)
  control_list <- control_def[grepl(" id '.*' ", V1), id]
  struct_dat <- .get_struct_dat(struct.dat.f)
  struct_def <- .get_struct_def(struct.def.f)
  struct_def_id <- struct_dat[id == struct, def_ID]
  struct_type <- struct_def[def_ID == struct_def_id, def_ty][[1]]
  if (!is.null(ct)) {
    ct <- unlist(ct)
    for (ct_id in ct) {
      if (!ct_id %in% control_list) {
        stop('Controller with ID: ', ct.id, ' is not defined in the control.def')
      }
    }
    # number of controllers is between 1 and 4
    stopifnot(length(ct) < 4 & length(ct) > 0)
    if (!struct_type %in% c("0", "6")) stop('Only support weir or river weir')
    # struct_type 0 for River Weir with max 4 Controllers
    if (struct_type == "0") {
      ca_match_patt <- " ca \\d \\d \\d \\d "
      cj_match_patt <- " cj '[^']+' '[^']+' '[^']+' '[^']+' "
      ca_rep_patt <- c(" ca", '0', '0', '0', '0', '')
      cj_rep_patt <- c(" cj", "'-1'", "'-1'", "'-1'", "'-1'", "")
      for (s in seq_along(ct)) {
        ca_rep_patt[s + 1] <- '1'
        cj_rep_patt[s + 1] <- paste("'", ct[[s]], "'", sep = "")
      }
      ca_rep_patt <- paste(ca_rep_patt, collapse = " ")
      cj_rep_patt <- paste(cj_rep_patt, collapse = " ")
    }
    # struct_type 6 for simple Weir with max only one controller
    if (struct_type == "6") {
      if (length(ct) > 1) stop("Too many controllers for a weir")
      ca_match_patt <- " ca \\d "
      cj_match_patt <- " cj '[^']+' "
      ca_rep_patt <- c(" ca 1 ")
      cj_rep_patt <- paste(" cj '", ct[[1]], "' ")
    }
    struct_dat[id == struct, 
               V1 := str_replace(V1, ca_match_patt, ca_rep_patt)]
    struct_dat[id == struct, 
               V1 := str_replace(V1, cj_match_patt, cj_rep_patt)]
  }
  # change crest-width to cw
  if (!is.null(cw)) {
    stopifnot(is.numeric(as.numeric(cw)))
    cw_rep <- paste0(' cw ', cw, ' ')
    struct_def[def_ID == struct_def_id, 
               V1 := str_replace(V1, " cw \\S+ ", cw_rep)
               ]
  }
  # change crest-level to cl
  if (!is.null(cl)) {
    stopifnot(is.numeric(as.numeric(cl)))
    cl_rep <- paste0(' cl ', cl, ' ')
    struct_def[def_ID == struct_def_id, 
               V1 := str_replace(V1, " cl \\S+ ", cl_rep)
               ]
  }
  # change flow direction
  if (!is.null(rt)) {
    rt_types <- c(0, 1, 2, 3, 'both', 'positive', 'negative', 'no flow')
    rt <- which(rt_types == rt)
    if (length(rt) != 1) {
      stop('rt must be one of: ', paste(rt_types, collapse = ', '))
    }
    rt <- c(0, 1, 2, 3, 0, 1, 2, 3)[rt]
    rt_rep <- paste0(' rt ', rt, ' ')
    struct_def[def_ID == struct_def_id, 
               V1 := str_replace(V1, " rt \\d ", rt_rep)
               ]
  }
  file.copy(struct.dat.f, paste(struct.dat.f, ".BAK", sep = ""))
  file.copy(struct.def.f, paste(struct.dat.f, ".BAK", sep = ""))
  fwrite(struct_dat[, .SD, .SDcols = c("V1")], struct.dat.f, sep = "\n",
         col.names = FALSE, quote = FALSE)
  fwrite(struct_def[, .SD, .SDcols = c("V1")], struct.def.f, sep = "\n",
         col.names = FALSE, quote = FALSE)
}