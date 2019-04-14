library(ncdf4)
library(data.table)
library(stringr)

ncf <- "C:/Users/quang/Downloads/Delft3DFM_150_201810/Tutorials/Tutorial_D-Flow_FM/tutorial09/tutorial09.dsproj_data/westerscheldt01_output/dflowfm/DFM_OUTPUT_westerscheldt01\\westerscheldt01_map.nc"
#----DELFT3DFM NetCDF files------
dims <- c(
  'time',
  'name_len',
  'stations',
  'cross_section',
  'cross_section_name_len',
  'cross_section_pts'
)

var_list_all  <- c(
  'station_x_coordinate',
  'station_y_coordinate',
  'station_id',
  'station_name',
  'waterlevel',
  'Waterdepth',
  'x_velocity',
  'y_velocity',
  'windx',
  'windy',
  'cross_section_x_coordinate',
  'cross_section_y_coordinate',
  'cross_section_name',
  'cross_section_discharge',
  'cross_section_discharge_int',
  'cross_section_discharge_avg',
  'cross_section_area',
  'cross_section_area_avg',
  'cross_section_velocity',
  'cross_section_velocity_avg',
  'WaterBalance_total_volume',
  'WaterBalance_storage',
  'WaterBalance_volume_error',
  'WaterBalance_boundaries_in',
  'WaterBalance_boundaries_out',
  'WaterBalance_boundaries_total',
  'WaterBalance_exchange_with_1D_in',
  'WaterBalance_exchange_with_1D_out',
  'WaterBalance_exchange_with_1D_total',
  'WaterBalance_precipitation',
  'WaterBalance_source_sink',
  'time'
)

var_list_waterbalance <- c(
  'WaterBalance_total_volume',
  'WaterBalance_storage',
  'WaterBalance_volume_error',
  'WaterBalance_boundaries_in',
  'WaterBalance_boundaries_out',
  'WaterBalance_boundaries_total',
  'WaterBalance_exchange_with_1D_in',
  'WaterBalance_exchange_with_1D_out',
  'WaterBalance_exchange_with_1D_total',
  'WaterBalance_precipitation',
  'WaterBalance_source_sink'
)

var_list_plot_sections <- c(
  'cross_section_discharge',
  'cross_section_discharge_int',
  'cross_section_discharge_avg',
  'cross_section_area',
  'cross_section_area_avg',
  'cross_section_velocity',
  'cross_section_velocity_avg'
)

var_list_plot_obs <- c('waterlevel',
                       'Waterdepth',
                       'x_velocity',
                       'y_velocity',
                       'windx',
                       'windy')

var_list_stations <- c('station_id',
                       'station_name')

var_list_obs_loc <- c('station_x_coordinate',
                      'station_y_coordinate')

var_list_obs_var_names <- c('station_name')

var_list_sect_var_names <- c('cross_section_name')

#----working with NC----
nc <- nc_open(ncf)
nc_t_units <- ncatt_get(nc, 'time')$units
nc_t_t0 <- str_extract(nc_t_unit, "\\d{1,}.*$") %>%
  as.POSIXct(tz = 'GMT', format = "%Y-%m-%d %H:%M:%S")
nc_t_unit <- str_match(nc_t_units, "(^.*) since")[1,2]
nc_t <- ncvar_get(nc, 'time') %>% as.POSIXct(origin = nc_t_t0)
lon <- ncvar_get(nc, 'FlowElem_xcc')
lat <- ncvar_get(nc, 'FlowElem_ycc')
elev <- ncvar_get(nc, 'FlowElem_zcc')
FlowElem <- data.table(lon, lat, elev)
wt <- ncvar_get(nc, 's1')
nc_element <- ncvar_get(nc, 'nFlowElem')
print(nc)
wt1 <- wt[1, ]
wt11 <- data.table(nc_t, wt1)

pt_a <- list(xcc = 78874.57, ycc = 10.4466386)
FlowElem[, da := ((lon - pt_a$xcc)**2 + (lat - pt_a$ycc)**2)]
FlowElem[da == min(da), which = TRUE]
