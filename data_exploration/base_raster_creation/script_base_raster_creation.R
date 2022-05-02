#namanpaul
#base raster generation script

#load the packages, install if the packages have not been installed
if (!require('raster')) install.packages('raster'); library('raster')
if (!require('sf')) install.packages('sf'); library('sf')

#set the working directory to the current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

current_wd <- getwd()
#set up the boundary limits for Canada

#read in Canada simplified
canada_simplified <- st_read(paste0(current_wd,'/canada_simplified/canada_simplified.shp'))

#set crs to 3347 (EPSG NAD83 / Statistics Canada Lambert)
canada_simplified <- st_transform(canada_simplified, 3347)

#extract the bounding box values
bbox_vals <- st_bbox(canada_simplified)

#unname the bbox
bbox_vals <- unname(bbox_vals)

#setting up the xmin/max ymin/max for the base raster
x_min <- bbox_vals[1]
x_max <- bbox_vals[3]
y_min <- bbox_vals[2]
y_max <- bbox_vals[4]

#specify the extent
ext  <- extent(x_min,x_max, y_min,y_max)

#resolutionval 5km (5000 meters)
resolution_val <- 5000

#base raster creation
base_raster <- raster(ext, 
                      ncol=(x_max - x_min)/resolution_val,
                      nrow=(y_max - y_min)/resolution_val,
                      "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#assigning cell numbers as values
base_raster_cell_values = 1:ncell(base_raster)

#set the values of the base raster
base_raster  <- setValues(base_raster, base_raster_cell_values)

#set crs 3347, change the proj4string to 3347
proj4string(base_raster) <- CRS("+init=epsg:3347")

#number of cells in base raster
num_cells_base_raster <- as.numeric(ncell(base_raster))

#create a dataframe
base_raster_df = as.data.frame(seq(1,num_cells_base_raster,1))

#changing the colname to rasterCellNum
colnames(base_raster_df) = "rasterCellNum"

#write raster
writeRaster(base_raster, 
            filename = 'base_raster.tif')

#saving the dataframe as well (will come handy later)
save(base_raster_df,
     file='base_raster_df.RData',
     compress = T)
