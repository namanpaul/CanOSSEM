#loading the packages
library(sf)
library(sp)
library(ggplot2)
library(raster)
library(gridExtra)
library(rasterVis)
library(rcartocolor)
library(rgdal)
library(dplyr)
library(purrr)
library(stringr)
library(mapview)
library(ncdf4)
library(rgdal)
library(gdalUtils)
library(rgeos)
library(RGeostats)
library(maptools)
library(lubridate)
library(foreach)


# set paths ---------------------------------------------------------------
setwd("../data_exploration/MERRA/output/bound/")

MERRA_input_path <- paste0(getwd(), '/')

# input -------------------------------------------------------------------

#set the working directory to the current location (especially to have a dynamic path setting for MERRA o/p)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

MERRA_output_path <- paste0(getwd(), '/output/MERRA/')

#load the df with CanOSSEM raster cell num with lon-lat and coordinates in meters
load('CanOSSEM_base_raster_data_frame_3347_4326_projection.RData')


# function definition -----------------------------------------------------

rCN_assign_MERRA <- function(MERRA_input_path, MERRA_output_path,year_val){
  
  #create the date range in julian days
  date_range <- format(seq(as.Date(paste(year_val,"-01-01", sep = "")),
                           as.Date(paste(year_val,"-01-02", sep = "")),
                           by=1), "%Y-%m-%d")
  
  foreach(i = seq_along(date_range)) %do% {
    
    #print the date
    print(date_range[i])
    
    #load the MERRA df
    load(paste0(MERRA_input_path,"MERRA_data_bound_",date_range[i],".RData"))
    
    
    #step1: U850 and V850 may contain some missing values, replace them with 0s
    MERRA_bound[is.na(MERRA_bound)] <- 0
    
    #daily mean calculation
    daily_mean_MERRA <- MERRA_bound %>%
      dplyr::group_by(longitude, latitude) %>%
      summarise_all(funs(mean))
    
    #xy of MERRA
    xy_MERRA <- sapply(daily_mean_MERRA[, 1:2], as.numeric)
    
    #xy of base raster in 4326 projection
    xy_base_raster_4326 <- sapply(df_base_raster_4326[, 2:3], as.numeric)
    
    #max num of cells
    num_cells_base_raster <- max(df_base_raster_4326$rasterCellNum)
    
    #spatial MERRA spdf
    spatial_MERRA <- SpatialPointsDataFrame(coords = xy_MERRA, data = daily_mean_MERRA,
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    
    #converting the spdf into dataframe
    df_spatial_MERRA <- as.data.frame(spatial_MERRA)
    
    #cleaning up the df
    df_spatial_MERRA <- df_spatial_MERRA %>% 
      mutate(MERRA_row_num = 1:nrow(df_spatial_MERRA)) %>% 
      rename(MERRA_lon = longitude,
             MERRA_lat = latitude) %>% 
      dplyr::select(-longitude.1, -latitude.1)
    
    
    
    #calling the nn2 function
    nn2output <- RANN::nn2(xy_MERRA,
                           query = xy_base_raster_4326,
                           k =1,
                           treetype = "kd",
                           searchtype = "priority")
    
    #converting the indices into a df
    nn2outputdf <- as.data.frame(nn2output[["nn.idx"]])
    
    #finding the closest rCN to MERRA row
    nn2outputdf_kd_priority <- nn2outputdf %>% 
      mutate(dists = nn2output[["nn.dists"]]) %>%
      rename(MERRA_row_num = V1) %>% 
      mutate(rasterCellNum = 1:nrow(nn2outputdf))
    
    #putting into a df
    nn2_lonlat <- left_join(nn2outputdf_kd_priority,
                            df_base_raster_4326,
                            by = "rasterCellNum") %>% 
      dplyr::select(-lon_3347, -lat_3347)
    
    
    #nn2 MERRA
    nn2_MERRA <- left_join(nn2_lonlat,
                         df_spatial_MERRA,
                         by = "MERRA_row_num")
    
    
    nn2_MERRA <- nn2_MERRA %>% 
      dplyr::select(-MERRA_row_num)
    
    
    
    
    #MERRA lon/lat
    lon1 <- as.numeric(nn2_MERRA$MERRA_lon)
    lat1 <- as.numeric(nn2_MERRA$MERRA_lat)
    
    #base raster lon/lat
    lon2 <- as.numeric(nn2_MERRA$longitude)
    lat2 <- as.numeric(nn2_MERRA$latitude)
    
    #defining the haversine distance function
    haversine_distance <- function(lon1,lat1,lon2,lat2){
      phi1 = lat1 * pi/180
      phi2 = lat2 * pi/180
      delta_phi = (lat2-lat1) * pi/180
      delta_lambda = (lon2-lon1) * pi/180
      R = 6371e3
      x = delta_lambda*cos((phi1+phi2)/2)
      y = delta_phi
      distance = R*sqrt(x^2 + y^2)
      return(distance/1000)
    }
    
    merra_distance <- as.data.frame(haversine_distance(lon1, lat1, lon2, lat2))
    
    merra_distance_bincols <- bind_cols(nn2_MERRA, merra_distance)
    
    merra_distance_new <- merra_distance_bincols %>% 
      rename(`Distance (km)` = `haversine_distance(lon1, lat1, lon2, lat2)`) %>% 
     dplyr::select(-dists)
    
  
    #dropping all the values where distance >50
    merra_distance_new <- merra_distance_new %>%
      filter(`Distance (km)`<=50) %>% 
      add_count(rasterCellNum, name = "count_cells", .drop = F) #%>% 
      
    #--don't need this next piece of code anymore
    #summarising for each cell
    #averaged_MERRA_data <- MERRA_new_distance %>% 
    #  #select(-MERRA_row_num) %>%
    #  group_by(rasterCellNum) %>%
    #  summarise_all(funs(mean)) %>% 
    #  select(rasterCellNum, ACQ_DATE:`Distance (km)`)
      
    
    #fill up the values for other rasterCellNum
    MERRA_filled_up_base <- merra_distance_new %>% 
      dplyr::select(-count_cells, -longitude, -latitude, -MERRA_lon, -MERRA_lat) %>% 
      tidyr::complete(rasterCellNum = seq(1:num_cells_base_raster)) %>% 
      dplyr::mutate(ACQ_DATE = date_range[i]) %>% 
      padr::fill_by_value(value = NA)
    
    #save the merra data
    save(MERRA_filled_up_base, 
         file = paste0(MERRA_output_path,'MERRA_data_rCN_assigned_', date_range[i], '.RData'))
    
  }
}



# function calling --------------------------------------------------------
for(year_val in 2019){
  rCN_assign_MERRA(MERRA_input_path, MERRA_output_path, year_val)
}


