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
  st_info_tbl_list <- list()
  for (i in seq_along(case.list)) {
    st_def_f <- get_file_path(case.name = case.list[i],
                               sobek.project = sobek.project,
                               type = 'struct.def')
    st.def <- .get_struct_def(st_def_f)
    st_dat_f <- get_file_path(case.name = case.list[i],
                               sobek.project = sobek.project,
                               type = 'struct.dat')
    st.dat <- .get_struct_dat(st_dat_f)
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
    st_info_tbl_i <- list()
    for (j in seq_along(st.list)) {
      st_info_tbl_i[[j]] <- .get_struct_info(
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
    st_info_tbl_list[[i]] <- rbindlist(st_info_tbl_i)
    colnames(st_info_tbl_list[[i]])[2] <- case.desc[i]
    if (i == 1) {
      st_info_tbl <- st_info_tbl_list[[i]]
    } else {
      st_info_tbl <- cbind(st_info_tbl,
                            st_info_tbl_list[[i]][, .SD,
                                                   .SDcols = eval(case.desc[i])]
                            )
    }
  }
  st_info_tbl <- filter_at(st_info_tbl,
                            vars(case.desc), any_vars(!is.na(.))) %>%
    as.data.table()
  st_info_tbl[is.na(Parameter), Parameter := '']
  if (html) {
    # calculating number of rows for each Controller group
    st_info_tbl[, orig_line := .I]
    row_begin <- st_info_tbl[Parameter == 'Controller ID' |
                                Parameter == 'Trigger ID' |
                                Parameter == 'Structure name',
                              orig_line]
    row_names <- st_info_tbl[row_begin, Parameter] %>% str_replace(' name| ID', '')
    row_end <- shift(row_begin, -1, fill = nrow(st_info_tbl))
    n_group <- length(row_begin)
    st_info_tbl[, orig_line := NULL]
    st_info_tbl <- kable(st_info_tbl, escape = FALSE) %>%
      kable_styling(c('hover', 'striped'), fixed_thead = TRUE)
    for (i in 1:n_group) {
      st_info_tbl <- st_info_tbl %>%
        pack_rows(row_names[i], row_begin[i], row_end[i])
    }
  }
  st_info_tbl
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
    st_def_f <- get_file_path(case.name = case.name,
                               sobek.project = sobek.project,
                               type = 'struct.def')
    st.def <- .get_struct_def(st_def_f)
  }
  if (is.null(st.dat)) {
    st_dat_f <- get_file_path(case.name = case.name,
                               sobek.project = sobek.project,
                               type = 'struct.dat')
    st.dat <- .get_struct_dat(st_dat_f)
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
  st_id_tbl <- st.dat[id == s.id][1,]
  st_id_def <- st.def[def_ID == st_id_tbl$def_ID][1,]
  # avoid having NA in name, for html table later
  st_id_tbl[is.na(name), name := '']
  st_id_list <- c(
    "Structure name" = st_id_tbl$name,
    "Structure ID" = s.id,
    'Structure definition ID' = st_id_tbl$def_ID,
    "Structure type" = .get_str_type(st_id_def$def_ty),
    "Crest level" = st_id_def$cl,
    "Crest width" = st_id_def$cw,
    "Control active" = st_id_tbl$ca,
    "Possible flow direction" = .get_rt_type(st_id_def$rt),
    'Total controllers' = 0L
  )
  if (!is.na(st_id_tbl$cj)) {
    ca_list <- str_split(st_id_tbl$ca, ' ', simplify = TRUE)[1, ]
    st_id_list[['Total controllers']] <- length(ca_list[ca_list == '1'])
    cj_list <- str_split(st_id_tbl$cj, ' ', simplify = TRUE)[1, ]
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
          st_id_list[[ct_name]] <- ct_id_list[[i]]
        } else {
          st_id_list[[ct_name]] <- NA
        }
    }
  } else {
    # we have to make a table that have always 4 controllers, every controllers
    # always have 4 triggers
    ct_id_list <- c('-1', '-1', '-1', '-1')
  }
  st_info_tbl <- data.table(
    Parameter = names(st_id_list),
    Value = unlist(st_id_list)
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
    st_info_tbl <- rbind(st_info_tbl, ct_tbl)
    if (html) {
      # calculating number of rows for each Controller group
      st_info_tbl[, orig_line := .I]
      n_group <- st_info_tbl[Parameter == 'Controller ID' | Parameter == 'Trigger ID'
                                , orig_line]
      r_group <- unlist(c("Structure Information",
                          st_info_tbl[n_group, paste0('Infos for ',
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
      st_info_tbl[, orig_line := NULL]
    }
  } else {
    if (html) {
      r_group <- c("Structure Information")
      color_r_group <- 'none'
      n_group <- nrow(st_info_tbl) # Number of rows for "Structure information"
    }
  }
  if (na.rm) st_info_tbl <- st_info_tbl[!is.na(Value)]
  if (html) {
      st_info_tbl <- htmlTable::htmlTable(
        st_info_tbl,
        align = 'l',
        col.rgroup = color_r_group,
        rgroup = r_group,
        n.rgroup = n_group,
        caption = paste(
          "Information table of the structure:", s.id),
        tfoot = paste('Case:', case.name)
      )
  }
  return(st_info_tbl)
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
      file_ext <- st_extract(file_name, '\\..+$')
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
  st_dat_tbl <- .get_all_struct(case.name = case.name,
                                 sobek.project = sobek.project, tble = tble)
  if (!html) {
    if (!is.null(output)) {
      # write output to excel file
      xlsx_wb <- createWorkbook()
      xlsx_sheet <- createSheet(xlsx_wb, sheetName = 'struct_info_tbl')
      addDataFrame(st_dat_tbl, xlsx_sheet, row.names = FALSE,
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
    invisible(st_dat_tbl)
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
    html_tmp <- st_replace(rmd_tmp, 'Rmd$', 'html')
    rmarkdown::render(rmd_tmp, output_format = 'html_document',
                      output_file = html_tmp)
    if (!is.null(output)) {
      file.copy(from = html_tmp, to = file_out)
      print(paste('and copied to:', file_out))
      html_tmp <- file_out
    }
    browseURL(html_tmp)
    invisible(st_dat_tbl)
  }
}


.get_all_struct <- function(
  case.name = NULL,
  sobek.project = NULL,
  html = FALSE,
  tble = TRUE
) {

  st_dat_f <- get_file_path(
    case.name = case.name,
    sobek.project = sobek.project,
    'struct.dat'
  )
  st_def_f <- get_file_path(
    case.name = case.name,
    sobek.project = sobek.project,
    'struct.def'
  )
  st_def_tbl <- .get_struct_def(st_def_f)
  st_def_tbl <- st_def_tbl[grepl(" id '.*'", V1)]
  st_def_tbl[, def_ty :=
                sapply(st_def_tbl$def_ty, .get_st_type)]
  st_def_tbl[, rt :=
                sapply(st_def_tbl$rt, .get_rt_type)]
  st_def_tbl <- st_def_tbl[, c('def_ID', 'def_name', 'def_ty', 'cl', 'cw', 'rt')]
  st_dat_tbl <- .get_struct_dat(st_dat_f)
  st_mtx <- str_match(
    st_dat_tbl$cj,
    "'([^']+)' '([^']+)' '([^']+)' '([^']+)'")[, -1] %>% as.data.table()
  st_mtx[V1 == '-1', V1 := ''][V2 == '-1', V2 := '']
  st_mtx[V3 == '-1', V3 := ''][V4 == '-1', V4 := '']
  st_mtx[is.na(V1), V1 := ''][is.na(V2), V2 := '']
  st_mtx[is.na(V3), V3 := ''][is.na(V4), V4 := '']
  st_dat_tbl[, c('ct1', 'ct2', 'ct3', 'ct4') := st_mtx]
  # get controllers for structure that have only one controller
  st_dat_tbl[!grepl("ca \\d \\d ", V1),
              ct1 := str_match(V1, " cj '([^']*)' ")[, 2]]
  st_dat_tbl[is.na(ct1), ct1 := '']
  st_dat_tbl <- st_dat_tbl[, c('id', 'name', 'def_ID',
                                 'ct1', 'ct2', 'ct3', 'ct4')]
  st_dat_tbl <- merge(st_dat_tbl, st_def_tbl, by.x = 'def_ID',
                       no.dups = TRUE,
                       by.y = 'def_ID') %>% setkey(NULL) %>% unique()
  st_cols <- c('id',
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
  st_cols_names <-
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
  st_dat_tbl <- st_dat_tbl[, .SD ,.SDcols = st_cols]
  colnames(st_dat_tbl) <- st_cols_names
  setorder(st_dat_tbl, ID)

  if (html) {
    ct_tbl <- .get_control_def(get_file_path(case.name, sobek.project, 'control.def'))
    tg_tbl <- .get_trigger_def(get_file_path(case.name, sobek.project, 'trigger.def'))
    ct_names <- grep("Controller \\d",
                     colnames(st_dat_tbl), value = TRUE)
    for (i in ct_names) {
      ct_hover <- lapply(
        st_dat_tbl[[i]],
        get_control_popover,
        ct.tbl = ct_tbl,
        tg.tbl = tg_tbl,
        html = TRUE,
        tble = tble
      )
      st_dat_tbl[[i]] <- cell_spec(
        st_dat_tbl[[i]],
        popover = spec_popover2(
          content = ct_hover,
          title = '<strong>Controller Information</strong>',
          html = TRUE,
          position = 'left'
        )
      )
    }
  }
  return(st_dat_tbl)
}


#' Turn off Weir(s) / Weir(s)
#'
#' Turn off Weir(s) / Weir(s) by deactivate all controllers and set crest width to 0
#'
#' @param st.id Name(s) of the (River) Weir(s)
#' @param case Case name
#' @param sobek.project Path to sobek project
#' @export
set_struct_off <- function(
  st.id = NULL,
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
    struct_def_id <- struct_dat[id == st.id[[i]], def_ID]
    # deactivate all controllers, prevent time controllers open the structure
    ca_match_patt <- " ca \\d \\d \\d \\d "
    cj_match_patt <- " cj '[^']+' '[^']+' '[^']+' '[^']+' "
    # for structure with 4 controllers
    struct_dat[id == st.id[[i]],
               V1 := str_replace(V1, ca_match_patt, ' ca 0 0 0 0 ')]
    struct_dat[id == st.id[[i]],
               V1 := str_replace(V1, cj_match_patt, " cj '-1' '-1' '-1' '-1' ")]
    # for structure with 1 controllers
    struct_dat[id == st.id[[i]],
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
#' @param st.id Name(s) of the (River) Weir(s)
#' @param cw Struct Crest Width
#' @param ct Struct controller ID(s), ex. c("##114", "##112")
#' @param case Case name
#' @param sobek.project Path to sobek project
#' @export
set_struct_on <- function(
  st.id = NULL,
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
  struct_def_id <- struct_dat[id == st.id, def_ID]
  struct_type <- struct_def[def_ID == struct_def_id, def_ty][[1]]
  if (!is.null(ct)) {
    ct <- unlist(ct)
    for (ct_id in ct) {
      if (!ct_id %in% control_list) {
        stop('Controller with ID: ', ct_id, ' is not defined in the control.def')
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
    struct_dat[id == st.id,
               V1 := str_replace(V1, ca_match_patt, ca_rep_patt)]
    struct_dat[id == st.id,
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



#' Transfer a structure from one case to another
#'
#' This function copies the definition of a structure in the control.def from one case, and paste/replace the structure with same id in the other case. By using this method, all information will be copied (both in dat/def files)
#'
#' @param from Name of ogirinal case
#' @param to Name of destination case
#' @param st.id ID of the controller
#' @param sobek.project Path to sobek project
#' @export
transfer_struct <- function(
  from,
  to,
  st.id,
  sobek.project,
  control = TRUE
) {
  st_dat_to_file <- get_file_path(case.name = to,
                                   sobek.project = sobek.project,
                                   type = 'struct.dat')
  st_def_to_file <- get_file_path(case.name = to,
                                   sobek.project = sobek.project,
                                   type = 'struct.def')
  ct_def_to_file <- get_file_path(case.name = to,
                                  sobek.project = sobek.project,
                                  type = 'control.def')
  st_dat_from <- .get_struct_dat(
    get_file_path(case.name = from, sobek.project = sobek.project,
                  type = 'struct.dat')
  )
  st_def_from <- .get_struct_def(
    get_file_path(case.name = from, sobek.project = sobek.project,
                  type = 'struct.def')
  )
  st_dat_to <- .get_struct_dat(st_dat_to_file)
  st_def_to <- .get_struct_def(st_def_to_file)
  ct_def_to <- .get_control_def(ct_def_to_file)
  strid_dat_from <- st_dat_from[id == st.id]
  strid_def_id_from <- strid_dat_from$def_ID[[1]]
  strid_def_from <- st_def_from[def_ID == strid_def_id_from]
  if (nrow(strid_dat_from) == 0) {
    stop('Structure with ID ', st.id, ' is not found in case: ', from)
  }
  ctr_ids_from <- strid_dat_from[1, cj]
  ctr_ids_from <- unlist(str_split(ctr_ids_from, ' '))
  ctr_ids <- str_replace_all(ctr_ids_from[ctr_ids_from != "'-1'"], "'", "")
  control <- length(ctr_ids) > 0 & control
  if (control) {
    ct_overwrite <- vector()
    for (i in seq_along(ctr_ids)) {
      ct_id_dep <- control_dependency(ct.id = ctr_ids[i],
                                      ct.tbl = ct_def_to,
                                      st.dat.tbl = st_dat_to)
      if (nrow(ct_id_dep) > 0) {
        ct_overwrite[i] <- ifelse(all(unique(ct_id_dep$st_id) == st.id), TRUE, FALSE)
      } else {
        ct_overwrite[i] <- TRUE
      }
    }
    ctr_list <- transfer_controller(
      ct.ids = ctr_ids,
      overwrite = ct_overwrite,
      from = from, to = to, sobek.project = sobek.project,
      write.def = FALSE
    )
    for (ct in ctr_ids) {
      ctr_ids_from <- str_replace(
        ctr_ids_from,
        ct,
        ctr_list$ct_tbl[orig_id == ct, new_id]
        )
    }
    ctr_ids_from <- paste(ctr_ids_from, collapse = ' ')
    strid_dat_from[, V1 := str_replace(V1, cj, ctr_ids_from)]
  } else {
    # remove controllers from strid_dat_from
    strid_dat_from[, V1 := str_replace(V1, ' ca \\d \\d \\d \\d ',
                                       ' ca 0 0 0 0 ')]
    strid_dat_from[, V1 := str_replace(V1,
                                       " cj '[^']+' '[^']+' '[^']+' '[^']+' ",
                                       " cj '-1' '-1' '-1' '-1' ")]
    strid_dat_from[, V1 := str_replace(V1, ' ca \\d ',' ca 0 ')]
    strid_dat_from[, V1 := str_replace(V1, " cj '[^']+' ", " cj '-1' ")]
    strid_def_from[grepl("STDS id '", V1),
                   V1 := str_replace(V1, " cw \\d*\\.*\\d* ", " cw 0 ")
                   ]
  }
  st_name_from <- strid_dat_from$name[[1]]
  if (nchar(st_name_from) > 1) {
    st_name_to <- st_name_from
    while (st_name_to %in% st_dat_to$name) {
      st_name_to <- paste(
        st_name_to,
        substr(basename(tempfile(pattern = '', fileext = '')), 1, 6),
        sep = '_'
      )
    }
    strid_dat_from[, V1 :=
                     str_replace(V1,
                                 paste0(" nm '", st_name_from, "'"),
                                 paste0(" nm '", st_name_to, "'")
                                 )]
  }
  strid_def_id_to <- strid_def_id_from
  # strid_def_from will be copied to the new def file
  strid_def_from <- st_def_from[def_ID == strid_def_id_from]
  strid_dat_to <- st_dat_to[id == st.id]
  # check if strid_def_id_from is already used in the st_def_to
  while (strid_def_id_to %in% st_def_to[, unique(def_ID)]) {
    strid_def_id_to <- substr(
      basename(tempfile(pattern = 'st_', fileext = '')),
      1, 10)
  }
  strid_def_nm_from <- strid_def_from[1, def_name]
  strid_def_nm_to <- strid_def_nm_from
  while (strid_def_nm_to %in% st_def_to[, unique(def_name)]) {
    strid_def_nm_to <- paste(
      strid_def_nm_from,
      substr(basename(tempfile(pattern = '', fileext = '')),  1, 6),
      sep = '_'
      )
  }
  strid_def_from[1, V1 := str_replace(
    V1,
    paste0(" id '", strid_def_id_from),
    paste0(" id '", strid_def_id_to)
  )]
  strid_def_from[1, V1 := str_replace(
    V1,
    paste0(" nm '", strid_def_nm_from),
    paste0(" nm '", strid_def_nm_to)
  )]
  strid_dat_from[, V1 := str_replace(
    V1,
    paste0(" dd '", strid_def_id_from),
    paste0(" dd '", strid_def_id_to)
  )]
  # Transfer struct in dat file-----
  # definition line of destination struct.dat will be replaced
  nrow_dat_to <- nrow(strid_dat_to)
  if (nrow_dat_to > 0) {
    st_dat_to_begin <- strid_dat_to[, min(orig_line_nr)]
    st_dat_to_end <- strid_dat_to[, max(orig_line_nr)]
    # it is ok with an empty data.table
    st_dat_new <- rbind(st_dat_to[orig_line_nr < st_dat_to_begin, c('V1')],
                         strid_dat_from[, c('V1')],
                         st_dat_to[orig_line_nr > st_dat_to_end, c('V1')])
  } else {
    st_dat_new <- rbind(st_dat_to[, c('V1')], strid_dat_from[, c('V1')])
  }
  # Transfer struct in def file-----
  st_def_new <- rbind(st_def_to[, c('V1')], strid_def_from[, c('V1')])
  # back up files
  file.copy(
    from = st_dat_to_file,
    to = paste0(st_dat_to_file, '.bak'),
    overwrite = TRUE
  )
  file.copy(
    from = st_def_to_file,
    to = paste0(st_def_to_file, '.bak'),
    overwrite = TRUE
  )
  # write result to files
  fwrite(
    st_dat_new,
    file = st_dat_to_file,
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE,
    sep = "\n"
  )
  fwrite(
    st_def_new,
    file = st_def_to_file,
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE,
    sep = "\n"
  )
  if (control) {
    ctr_def_to_file <- get_file_path(to, sobek.project, 'control.def')
    file.copy(
      from = ctr_def_to_file,
      to = paste0(ctr_def_to_file, '.bak'),
      overwrite = TRUE
    )
    fwrite(
      ctr_list$def_to,
      file = ctr_def_to_file,
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE,
      sep = "\n"
    )
  }
}


#' Get information of a structure
#' @param s.id ID of the structure
#' @param case.name Name of the case
#' @param sobek.project Path to sobek project
#' @param html Output to HTML table? Default TRUE
#' @param trigger If TRUE, information about triggers will be given
#' @param control If TRUE, information about controllers will be given
#' @import data.table
#' @export
#' @return a data.table or a HTML object
get_struct_info_old <- function(
  s.id = NULL,
  case.name = NULL,
  sobek.project = NULL,
  html = TRUE,
  trigger = TRUE,
  control = TRUE
){

  # get path to files
  st_def_f <- get_file_path(case.name = case.name,
                             sobek.project = sobek.project,
                             type = 'struct.def')
  st_dat_f <- get_file_path(case.name = case.name,
                             sobek.project = sobek.project,
                             type = 'struct.dat')

  st_dat_tbl <- .get_struct_dat(st_dat_f)
  if (!s.id %in% st_dat_tbl$id) {
    stop(s.id, ' not found in struct.dat. Remember that cases are sensitive')
  }
  st_def_tbl <- .get_struct_def(st_def_f)
  st_id_tbl <- st_dat_tbl[id == s.id][1,]
  st_id_def <- st_def_tbl[def_ID == st_id_tbl$def_ID][1,]
  st_id_list <- list(
    Struct_ID = s.id,
    Struct_name = st_id_tbl$name,
    Struct_type = .get_str_type(st_id_def$def_ty),
    "Crest_level" = st_id_def$cl,
    "Crest_width" = st_id_def$cw,
    Controller = st_id_tbl$ca,
    "Possible_flow_direction" = st_id_def$rt,
    'Total_controllers' = 0L,
    'Definition_ID' = st_id_tbl$def_ID
  )
  if (!is.na(st_id_tbl$cj)) {
    cj_list <- str_split(st_id_tbl$cj, ' ', simplify = TRUE)[1, ]
    ct_id_list <- gsub("'", "", cj_list[!grepl("'-1'", cj_list)])
    if (length(ct_id_list) > 0) {
      st_id_list$Total_controllers <- length(ct_id_list)
      # ct_id_tbl <- subset(ct_def_tbl, id %in% ct_id_list & !is.na(ct))
      for (i in seq_along(ct_id_list)){
        ct_name <- paste('Control', i, sep = "_")
        st_id_list[[ct_name]] <- ct_id_list[[i]]
      }
    }
  } else {
    ct_id_list <- NULL
  }
  st_info_tbl <- data.table(
    Parameter = names(st_id_list),
    Value = st_id_list
  )
  r.group <- c("Structure Information")
  n.rgroup <- c(11) # Number of rows for "Structure information"
  if (isTRUE(control) & length(ct_id_list) > 0) {
    ct_tbl <- rbindlist(lapply(ct_id_list, get_control_info_old,
                               def.file = NULL,
                               case.name = case.name,
                               sobek.project = sobek.project,
                               html = FALSE,
                               trigger = trigger))
    st_info_tbl <- rbind(st_info_tbl, ct_tbl)
    # calculating number of rows for each Controller group
    st_info_tbl[, orig_line := .I - 1]
    r.group <- c("Structure Information", paste('Controller', ct_id_list))
    n.rgroup <- c(st_info_tbl[Parameter == 'Control_ID', orig_line],
                  nrow(st_info_tbl))
    n.rgroup <- n.rgroup - shift(n.rgroup, 1, fill = 0)
    st_info_tbl[, orig_line := NULL]
  }
  if (isTRUE(html)) {
    st_info_tbl <- htmlTable::htmlTable(
      st_info_tbl,
      align = 'l',
      rgroup = r.group,
      n.rgroup = n.rgroup,
      caption = paste(
        "Information table of the structure:", s.id),
      tfoot = paste('Case:', case.name)
    )
  }
  return(st_info_tbl)
}


#' Get information of a controller
#' @param ct.id ID of the controller
#' @param def.file Path to control.def file
#' @param case.name Name of the case (considered if def.file == NULL)
#' @param sobek.project Path to sobek.project (considered if def.file == NULL)
#' @param trigger If TRUE, information about triggers will be given
#' @export
#' @return a list
get_control_info_old <- function(ct.id = NULL,
                             def.file = NULL,
                             case.name = NULL,
                             sobek.project = NULL,
                             trigger = FALSE,
                             html = TRUE
) {

  if (is.null(def.file)) {
    def.file <- get_file_path(case.name, sobek.project, type = 'control.def')
  } else {
    if (isTRUE(trigger)) {
      stopifnot(!is.null(case.name) & !is.null(sobek.project))
    }
  }
  ct_def <- .get_control_def(control.def.f = def.file)
  ct_id_tbl <- ct_def[id == ct.id][1, ]
  ct_info_list <- list(
    'Control_ID' = ct_id_tbl$id,
    'Control_name' = ct_id_tbl$name,
    'Control_type' = .get_ct_type(ct_id_tbl$ct),
    'Control_parameter' = .get_cp_type(ct_id_tbl$ca),
    'Controlled_active' = ct_id_tbl$ac,
    'Control_measurement' = ct_id_tbl$ml,
    'Measured_parameter' = .get_ct_param_type(ct_id_tbl$cp),
    'Time_lag' = ct_id_tbl$mp,
    'Update_frequency' = ct_id_tbl$cf,
    'Trigger_active' = ct_id_tbl$ta,
    'Trigger_IDs' = ct_id_tbl$gi,
    'dValue/dt' = ct_id_tbl$mc,
    'Control_tble' = .get_control_tbl(ct.id, ct_def)
  )
  ct_info_tbl <- data.table(Parameter = names(ct_info_list),
                            Value = ct_info_list)
  r.group <- c("Structure Information")
  n.rgroup <- c(11) # Number of rows for "Structure information"
  if (isTRUE(trigger)) {
    trig_all <- str_match(
      ct_info_tbl[Parameter == 'Trigger_IDs', Value],
      "'([^']+)' '([^']+)' '([^']+)' '([^']+)'"
    )[, 2:5]
    trig_all <- trig_all[trig_all != '-1']
    if (length(trig_all) > 0) {
      trig_tbl <- rbindlist(lapply(trig_all, get_trigger_info,
                                   case.name = case.name,
                                   sobek.project = sobek.project,
                                   html = FALSE)
      )
      ct_info_tbl <- rbind(ct_info_tbl, trig_tbl)
      r.group <- c("Controller Information")
      n.rgroup <- c(13) # Number of rows for "Controller information"
      ct_info_tbl[, orig_line := .I - 1]
      r.group <- c("Controller Information", paste('Trigger', trig_all))
      n.rgroup <- c(ct_info_tbl[Parameter == 'Trigger_ID', orig_line],
                    nrow(ct_info_tbl))
      n.rgroup <- n.rgroup - shift(n.rgroup, 1, fill = 0)
      ct_info_tbl[, orig_line := NULL]
    }
  }
  if (isTRUE(html)) {
    ct_info_tbl <- htmlTable::htmlTable(
      ct_info_tbl,
      align = 'l',
      rgroup = r.group,
      n.rgroup = n.rgroup,
      caption = paste(
        "Information table of the Controller:", ct.id),
      tfoot = paste('Case:', case.name)
    )
  }
  return(ct_info_tbl)
}


# this function get controlling table of a controller
.get_control_tbl <- function(
  ct.id, ct.def
){
  ct_id_tbl <- ct.def[id == ct.id, c("V1")]
  ct_id_tbl_nrow <- nrow(ct_id_tbl)
  if (ct_id_tbl_nrow > 3) {
    ct_id_tbl <- ct_id_tbl[3:(ct_id_tbl_nrow - 1)]
    return(paste(ct_id_tbl$V1, collapse = "<br>"))
  } else{
    return(NA)
  }
}


#' Finding dependency for one structure
#'
#' @param st.id ID of structure in struct.dat
#' @param st.dat.tbl Table of struct.dat
#' @param st.def.tbl Table of struct.def
#' @param ct.tbl Table of control.def
#' @param tg.tbl Table of trigger.def
#' @param st.dat.f Path to struct.dat
#' @param st.def.f Path to struct.def
#' @param ct.def.f Path to control.def
#' @param tg.def.f Path to trigger.def
#' @param case.name Name of sobek case
#' @param sobek.project Path to sobek.project
struct_dependency <- function(
  st.id,
  st.dat.tbl = NULL,
  st.def.tbl = NULL,
  ct.tbl = NULL,
  tg.tbl = NULL,
  st.dat.f = NULL,
  st.def.f = NULL,
  ct.def.f = NULL,
  tg.def.f = NULL,
  case.name = NULL,
  sobek.project = NULL
) {
  # reading dat, def difinitions-----
  if (!is.null(case.name)) {
    if (is.null(sobek.project)) stop('case.name and sobek.project must be given together')
    st.dat.f <- get_file_path(case.name, sobek.project, 'struct.dat')
    st.def.f <- get_file_path(case.name, sobek.project, 'struct.def')
    ct.def.f <- get_file_path(case.name, sobek.project, 'control.def')
    tg.def.f <- get_file_path(case.name, sobek.project, 'trigger.def')
  }
  if (!is.null(st.dat.f)) {
    st.dat.tbl <- .get_struct_dat(st.dat.f)
  } else {
    if (is.null(st.dat.tbl)) stop('Not enough information for getting struct.dat table')
  }
  if (!is.null(st.def.f)) {
    st.def.tbl <- .get_struct_def(st.def.f)
  } else {
    if (is.null(st.def.tbl)) stop('Not enough information for getting struct.def table')
  }
  if (!is.null(ct.def.f)) {
    ct.tbl <- .get_control_def(ct.def.f)
  } else {
    if (is.null(ct.tbl)) stop('Not enough information for getting controller table')
  }
  if (!is.null(tg.def.f)) {
    tg.tbl <- .get_trigger_def(tg.def.f)
  } else {
    if (is.null(tg.tbl)) stop('Not enough information for getting trigger table')
  }
  # controllers that are used by this structure
   st_cj <- st.dat.tbl[id == st.id, cj]
   st_cj <- stri_replace_all_fixed(st_cj, "'", "")
   st_cj <- unlist(stri_split_fixed(st_cj, ' '))
   st_cj_list <- lapply(st_cj, FUN = control_dependency,
                        ct.tbl = ct.tbl,
                        st.dat.tbl = st.dat.tbl)
   st_ct_dep <- unique(rbindlist(st_cj_list))
   return(st_ct_dep)
}


#' Copy files of structures, controllers, triggers from one case to another
#'
#' @param from Name of origin case
#' @param to Name of destination case
#' @param sobek.project Path to sobek project
#' @export
transfer_struct_files <- function(
  from,
  to,
  sobek.project
) {
  from_folder <- dirname(get_file_path(from, sobek.project, 'struct.dat'))
  to_folder <- dirname(get_file_path(to, sobek.project, 'struct.dat'))

  from_files <- list.files(from_folder, 'struc|control|trigger',
                           full.names = FALSE, ignore.case = TRUE)
  from_files <- from_files[!grepl('his$', from_files, ignore.case = TRUE)]
  to_files <- file.path(to_folder, from_files)
  from_files <- file.path(from_folder, from_files)

  file.copy(from_files, to_files, overwrite = TRUE)
}
