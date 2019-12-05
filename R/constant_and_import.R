# excel standard color
#' Standard colors in Excel
#' @import stringr
#' @import ggplot2
#' @importFrom xlsx createWorkbook createSheet addDataFrame saveWorkbook autoSizeColumn createRow createCell Font Alignment addMergedRegion setCellStyle
#' @importFrom stringi stri_conv stri_pad_left stri_pad_right
#' @importFrom dplyr %>% select starts_with near filter filter_at
#' @importFrom purrr map_dbl map_int
#' @import data.table
#' @import grid
#' @importFrom kableExtra kable kable_styling pack_rows add_header_above cell_spec spec_popover
#' @importFrom utils write.table
#' @export
exl_std <- c(
  "#C00000",
  "#FF0000",
  "#FFC000",
  "#FFFF00",
  "#92D050",
  "#00B050",
  "#00B0F0",
  "#0070C0",
  "#002060",
  "#7030A0"
)

# Nice set of 6 colors
#' @export
six_colors <- exl_std[c(6, 2, 3, 8, 9, 10)]

# Excel 1st Column colors
#' @export
exl_set1 <- c(
  "#FFFFFF",
  "#F2F2F2",
  "#D9D9D9",
  "#BFBFBF",
  "#A6A6A6",
  "#808080"
)

# Excel 2nd Column colors
#' @export
exl_set2 <- c(
  "#000000",
  "#808080",
  "#595959",
  "#404040",
  "#262626",
  "#0D0D0D"
)

# Excel 3rd Column colors
#' @export
exl_set3 <- c(
  "#EEECE1",
  "#DDD9C4",
  "#C4BD97",
  "#948A54",
  "#494529",
  "#1D1B10"
)

# Excel 4th Column colors
#' @export
exl_set4 <- c(
  "#1F497D",
  "#C5D9F1",
  "#8DB4E2",
  "#538DD5",
  "#16365C",
  "#0F243E"
)

# Excel 5th Column colors
#' @export
exl_set5 <- c(
  "#4F81BD",
  "#DCE6F1",
  "#B8CCE4",
  "#95B3D7",
  "#366092",
  "#244062"
)

# Excel 6th Column colors
#' @export
exl_set6 <- c(
  "#4F81BD",
  "#DCE6F1",
  "#B8CCE4",
  "#95B3D7",
  "#366092",
  "#244062"
)

# Excel 7th Column colors
#' @export
exl_set7 <- c(
  "#9BBB59",
  "#EBF1DE",
  "#D8E4BC",
  "#C4D79B",
  "#76933C",
  "#4F6228"
)

# Excel 8th Column colors
#' @export
exl_set8 <- c(
  "#8064A2",
  "#E4DFEC",
  "#CCC0DA",
  "#B1A0C7",
  "#60497A",
  "#403151"
)

# Excel 9th Column colors
#' @export
exl_set9 <- c(
  "#4BACC6",
  "#DAEEF3",
  "#B7DEE8",
  "#92CDDC",
  "#31869B",
  "#215967"
)

# Excel 10th Column colors
#' @export
exl_set10 <- c(
  "#F79646",
  "#FDE9D9",
  "#FCD5B4",
  "#FABF8F",
  "#E26B0A",
  "#974706"
)

# 20 distinct colors
#' @export
color20 <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')
