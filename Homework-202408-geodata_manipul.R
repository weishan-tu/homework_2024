 ## ---------------------------
 ##
 ## Script name: geodata_manipul.R
 ##
 ## Purpose of script:Write R code to perform the following tasks, and name the homework as geodata_manipul.R:
 # 1.	set 2-km buffer along the Doubs river and clip from the 
 # map to extract the raster values of the catchment area 
 # and slope for each point with the qgisprocess package.
 # # 2.	merge the extracted data with other environmental 
 # factors from Doubs dataset to form a dataframe, and finally 
 # transfer the dataframe to a sf object, which contains a geometry column.
 # 
 ##
 ## Author: Weishan Tu 
 ##
 ## Date Created: 
 ##
 ## Copyright (c) Timothy Farewell, 
 ## Email: weishan@mail.ustc.edu.cn
 ## ---------------------------

 
 cat("\014") #clears rhe console
 rm(list=ls()) #remove all variales

 ## ---------------------------
  # pak::pak("qgisprocess")
  library(qgisprocess)
 library(sf)
 library(terra)

qgis_version()
qgis_plugins()
qgis_providers()

qgis_enable_plugins(c("grassprovider", "processing_saga_nextgen"))
##To get the complete overview of available (cached) geoalgorithms, run:
algs <- qgis_algorithms()
algs
# For a directed search, 
qgis_search_algorithms(algorithm = "buffer", group = "[Vv]ector")
# To find out about a specific geoalgorithm and a description of its arguments, 
qgis_show_help("native:buffer")
# To find out the arguments of a specific geoalgorithm, run:
qgis_get_argument_specs("native:buffer")

##load doubs rivers
doubs_river <- read_sf("doubs/doubs_river.shp")  #EPSG:4326
##project to WGS 84 / UTM zone 30N
doubs_river_project <- st_transform(doubs_river, 32630) ##32630 WGS 84 / UTM zone 30N
## create 2km buffer
# data("random_points", package = "spDataLarge")
buff_2km <- qgis_run_algorithm("native:buffer", 
                                     INPUT = doubs_river_project,
                                     DISTANCE = 2000) 
## use r create buffer
doubs_buf  <- st_buffer(doubs_river_project, 2000)
# attach QGIS output
# either do it "manually":
doubs_buf <- read_sf(qgis_extract_output(buff_2km, "OUTPUT"))
# or use the st_as_sf.qgis_result method:
doubs_buf <- sf::st_as_sf(buff_2km)
# plot your result
library("mapview")
mapview(doubs_buf, col.regions = "blue") + 
  mapview(doubs_river, col.regions = "red", cex = 3)

##attach digital elevation model 
# install.packages("elevatr")
library(elevatr)
doubs_dem <- get_elev_raster(doubs_buf, z = 10, clip = "locations")
# Please connect to the internet and try again.
# or use WorldClim 2.1 Elevation 5 minutes
global_dem <- rast("doubs/wc2.1_5m_elev.tif")
doubs_buf_ws84 <- st_transform(doubs_river, 4326)
doubs_dem <- mask(global_dem,doubs_buf_ws84)
plot(doubs_dem)
# saveRDS(doubs_dem,"doubs_dem.RDS")
#save_raster
writeRaster(doubs_dem,"doubs/doubs_dem.tif")
doubs_dem <- rast("doubs/doubs_dem.tif")
##get catchment area and  slops
# qgis_enable_plugins("processing_saga_nextgen")
oldopt <- options(qgisprocess.tmp_raster_ext = ".sdat")
info <- qgis_run_algorithm(algorithm = "sagang:sinkremoval", DEM = doubs_dem, 
                             METHOD = 1) |>
  qgis_extract_output("DEM_PREPROC") |>
  qgis_run_algorithm(algorithm = "sagang:sagawetnessindex",
                     DEM = _)
# qgis_search_algorithms(algorithm = "sagawetnessindex")
nms <- qgis_get_output_specs("sagang:sagawetnessindex")$name
##read doubs samples points
library(tidyverse)
doubs_samples <- read_sf("doubs/doubs_samples.shp")#%>%select(-c(fid,sample_id))

##add catchment area and  slops to point
doubs_add <- qgis_run_algorithm(
  "sagang:addrastervaluestopoints",
  SHAPES = doubs_samples,
  GRIDS = qgis_extract_output(info, "AREA"),
  GRIDS = qgis_extract_output(info, "SLOPE"),
  RESAMPLING = 0)
##change to r sf
doubs_add_sf <- sf::st_as_sf(doubs_add)
colnames(doubs_add_sf) <- c("xcoord" ,"ycoord","AREA","SLOPE","geom" )
###add ade4 env
library(ade4)
data("doubs")
doubs_env <- doubs$env
doubs_samples_all_env <- cbind(doubs_add_sf,doubs_env)
##save shp
write_sf(doubs_samples_all_env,"doubs/doubs_samples_all_env.shp")
