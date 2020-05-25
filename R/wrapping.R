# wrapping functions


#' Saving PNG image for potrait A4
#'
#'  This is only a wraping for ggsave function. With default png, width 9,
#'  height 6, dpi 350, and positions of 2 parameters `plot` and `file` are exchanged
#'  If there is no file name given, it will take the plot parameter as name
#'
#' @export
save_polder <- function(plot, filename = NULL,
            device = "png", width = 9, height = 6, dpi = 350
            , ...){
  f_args <- match.call(expand.dots = FALSE)
  if (is.null(filename)) filename <- paste(f_args$plot, ".png", sep = "")
  ggsave(filename = filename, plot = plot,
         device = device, width = width, height = height, dpi = dpi, ...)
}


#' Saving PNG image for landscape A4
#'
#'  This is only a wraping for ggsave function. With default png, width 15,
#'  height 10, dpi 350, and positions of 2 parameters `plot` and `file` are exchanged.
#'  If there is no file name given, it will take the plot parameter as name
#'
#' @export
save_profile <- function(plot, filename = NULL,
                        device = "png", width = 18, height = 10, dpi = 350
                        , ...){
  f_args <- match.call(expand.dots = FALSE)
  if (is.null(filename)) filename <- paste(f_args$plot, ".png", sep = "")
  ggsave(filename = filename, plot = plot,
         device = device, width = width, height = height, dpi = dpi, ...)
}


#' Saving WMF image for insert inline
#'
#'  This is only a wraping for ggsave function. With default device wmf, width 15,
#'  height 8, and positions of 2 parameters `plot` and `file` are exchanged.
#'  If there is no file name given, it will take the plot parameter as name
#'
#' @export
save_wmf <- function(plot, filename = NULL,
                         device = "wmf", width = 15, height = 8
                         , ...){
  f_args <- match.call(expand.dots = FALSE)
  if (is.null(filename)) filename <- paste(f_args$plot, ".wmf", sep = "")
  ggsave(filename = filename, plot = plot,
         device = device, width = width, height = height, ...)
}


#' Saving WMF image for insert inline
#'
#'  This is only a wraping for ggsave function. With default device wmf, width 15,
#'  height 8, and positions of 2 parameters `plot` and `file` are exchanged.
#'  If there is no file name given, it will take the plot parameter as name
#'
#' @export
save_svg <- function(plot, filename = NULL,
                     device = "svg", width = 15, height = 8
                     , ...){
  f_args <- match.call(expand.dots = FALSE)
  if (is.null(filename)) filename <- paste(f_args$plot, ".svg", sep = "")
  ggsave(filename = filename, plot = plot,
         device = device, width = width, height = height, ...)
}

#' Create new variable with suffix
#'
#' This function creates new variable with a suffix, get value from paste0
#'
#' @param ... List of variables
#' @param suffix string as suffix
#'
#' @return nothing. Variables created in the global enviroment
#' @examples
#' cases = c('Case 1', 'Case 2')
#' new_suffix(cases)
#' @export
new_suffix <- function(..., suffix = '_EreigOpt') {
  f_args <- as.list(match.call())
  for (i in seq_along(f_args)) {
    if (i > 1) {
      old_var <- eval(f_args[[i]])
      new_var <- paste0(eval(f_args[i]), suffix)
      cmd <- expr(!!ensym(new_var) <- paste0(!!old_var, suffix))
      eval(cmd, envir = globalenv())
    }
  }
}


#'@export
elbe_desc <- function(cases) {
  case_desc <- vector(length = length(cases))
  for (i in seq_along(cases)) {
    case_find <- elbe_case_tbl[cases[i], case_desc]
    case_desc[i] <- ifelse(!is.na(case_find), case_find, cases[i])
  }
  case_desc
}

#'@export
vgf_label <- function(x, ms = TRUE) {
  ret <- vector(mode = 'character', length = length(x))
  for (i in seq_along(x)) {
    ret[i] <- x[[i]]
    if (isTRUE(grepl('vgf1', x[[i]], ignore.case = TRUE))) ret[i] <- 'Faktor 1,0'
    if (isTRUE(grepl('mittel$', x[[i]], ignore.case = TRUE))) ret[i] <- 'HQmittel (HQselten1)'
    if (isTRUE(grepl('selten$', x[[i]], ignore.case = TRUE))) ret[i] <- 'HQselten (HQselten2)'
    if (ms)
      ret[i] <- str_replace(str_replace(ret[i], 'HQmittel$', 'HQselten1'),
                            'HQselten$',
                            'HQselten2'
      )
  }
  ret
}

#'@export
zp_label <- function(x) {
  ret <- vector(mode = 'character', length = length(x))
  for (i in seq_along(x)) {
    ret[i] <- switch(tolower(x[[i]]),
                     zp0 = '(ohne)',
                     ta = 'TA',
                     wb = 'LW',
                     lw = 'LW',
                     zpk = 'Köln',
                     zpw = 'Worms',
                     x[[i]])
  }
  ret
}
