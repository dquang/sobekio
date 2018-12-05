cname <- "NHWSP_Rhein_NebenflussundPolderHQ50_1510"
so_prj <- "d:/so21302/nhwsp.lit"
rhis <- get_file_path(cname, so_prj, "reach")
chis <- get_file_path(cname, so_prj, "node")
file.size(rhis)/10^6

case_list <- list(
  "NHWSP_Rhein_NebenflussundPolderHQ50_1510",
  "Kopie NHWSP_Rhein_NebenflussundPolderHQ50_1510"
)
single_case <- "2013_Basis_V6_hist_HW1999"

rloc <- his_location(rhis)
qid <- rloc$long.id

# his_from_list
system.time(rlist <- his_from_list())
