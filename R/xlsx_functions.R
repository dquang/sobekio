# Formating excel file
add_xlsx_cell <- function(
  wb, sheet, col_id, row_id,
  value,
  fill = NULL, 
  # for Font
  height = 12, color = 'black', bold = FALSE, italic = FALSE, underline = NULL,
  strike.out = FALSE,
  # for Fill
  fil.fg.color = 'lightblue', fil.bg.color = 'lightblue', fil.pat = "SOLID_FOREGROUND",
  # for CellProtection
  locked = TRUE, hidden = FALSE
) {
  rows <- createRow(sheet, rowIndex = row_id)
  cells <- createCell(rows, colIndex = col_id)
  cell_style <- CellStyle(wb) + Font(wb, heightInPoints = height,
                                     color = color, isBold = bold,
                                     underline = underline, isStrikeout = strike.out
                                     )
  for (i in row_id) {
    for (j in col_id) {
      setCellValue(cells[[i, j]], value[i, j])
      setCellStyle(cells[[i, j]], value[i, j])
    }
  }
}