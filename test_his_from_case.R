library(data.table)
library(sobekio)
soprj <- 'c:/so21503/vgtb.lit'
cases <- c('Default 3', 'Default 4', 'Default 3', 'Default 4', 'Default 3', 'Default 4',
           'Default 3', 'Default 4', 'Default 3', 'Default 4','Default 3', 'Default 4', 'Default 3', 'Default 4', 'Default 3', 'Default 4')
calcpnt <- get_file_path('Default 3',
                         soprj, 'node')
hloc <- his_location(calcpnt)
hloc[, name:=paste('loc_', location, sep = '')]
write.table(hloc[1:100, c(2,4)], 'wid.txt', row.names = F, col.names = F, quote = F)
system.time(tmp <- his_from_case(case.list = cases[1], sobek.project = soprj,
                     param = 'waterlevel', wID = 'wid.txt'))
length(cases)
length(colnames(tmp))
