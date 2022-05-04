#namanpaul
#converting CanOSSEM base raster from Statistics Canada NAD 83 projection to lon-lat projection

#load the packages
library(sf)
library(sp)
library(ggplot2)
library(raster)
library(rgdal)
library(dplyr)
library(purrr)
library(stringr)
library(ncdf4)
library(rgdal)
library(gdalUtils)
library(rgeos)
library(lubridate)

#set the working directory to the current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

current_wd <- getwd()

#source the base_raster script
base_raster <- raster('../data_exploration/base_raster_creation/base_raster.tif')

#save it as a base raster dataframe
df_base_raster <- as.data.frame(rasterToPoints(base_raster))

#rename cols for base_raster_df
df_base_raster <- df_base_raster %>% 
  dplyr::rename(lon_3347 = x,
                lat_3347 = y,
                rasterCellNum = base_raster)

#Raster to points
base_raster_points <- rasterToPoints(base_raster, spatial = T)

#crs 4326 would be required
crs_4326 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#transform it with 4326
base_raster_4326 <- spTransform(base_raster_points,
                                CRSobj = crs_4326)

#convert it to a dataframe
df_base_raster_4326 <- as.data.frame(base_raster_4326)

#renaming cols
df_base_raster_4326 <- df_base_raster_4326 %>% 
  dplyr::rename(longitude = x,
                latitude = y,
                rasterCellNum = base_raster)

#bind it together
df_base_raster_4326 <- merge(df_base_raster_4326,
                             df_base_raster,
                             by = "rasterCellNum")

#save lon lat 4326
xy_base_raster_4326 <- sapply(df_base_raster_4326[,2:3], as.numeric)

#save the dataframe
save(df_base_raster_4326,
     file = 'CanOSSEM_base_raster_data_frame_3347_4326_projection.RData',
     compress = T)

