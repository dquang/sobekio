# test
his.file <- "c:/so21503/vgtb.lit/3/calcpnt.his"
loc <- seq.int(1, 3000, 1)
loc1 <- his_location(his.file)

system.time(locdf1 <- .his_df(his.file))
system.time(locdf2 <- .his_from_locs(his.file, loc, 1))
system.time(locdf3 <- his_from_list(his.file, loc1$sobek.id, 1))

locs1 <- sapply(loc1$sobek.id, .id2loc, loc1)

# checking with NA
locs2 <- loc1$sobek.id[1:5]
locs2 <- c('NA', '2', 'Na2', 'Na 3', '3')
locs2df <- his_from_list(his.file, locs2, 'water level')

par = .his_parameter(his.file)
system.time(hdf1 <- .his_df(his.file, 1))
system.time(hdf2 <- .his_df2(his.file, 1))
View(hdf2[,1:2])
View(hdf1[,1:2])
cases = c('Default 3', 'Default 4')
soprj = 'c:/so21503/vgtb.lit'
hloc <- sobek_case_info(case.name = cases[1], soprj, his.type = 'node', info = 'location')
his.file <- get_file_path('Default 3', soprj, type = 'node')

system.time(tmp <- his_from_case(cases, soprj, wID = hloc$sobek.id[1:10],
                                 param = 'waterlevel')[[1]]
            )
