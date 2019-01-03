library(data.table)
so_prj <- "d:/so21302/main2015.lit"
cmtl <- fread(file = "d:/so21302/main2015.lit/caselist.cmt",
              header = F, sep = " "
              )
cname <- 'MainMod_HW2003_vg1529'
bnd_dat <- get_file_path(case.name = cname,
                         sobek.project = so_prj,
                         type = "bnd.dat")

rhis <- get_file_path(case.name = cname,
                         sobek.project = so_prj,
                         type = "reach")
rhis <- "D:/So21302/NHWSP.lit/25/reachseg.his"
rloc <- his_location(rhis)

system.time(rlist <- his_from_list(rhis, id.list = rloc$sobek.id[1:5],
                       param = "discharge"))

q_kemm <- get_data_from_id(dat.file = bnd_dat,
                           s.id = "1")
q_kemm[, "V5" := V4*2]


