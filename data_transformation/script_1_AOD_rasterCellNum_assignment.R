#namanpaul
#AOD raserCellNum assignment

#loading the packages
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
library(maptools)
library(lubridate)
library(foreach)


# set paths ---------------------------------------------------------------
#set the working directory to the current location (especially to have a dynamic path setting for AOD o/p)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

setwd("../data_exploration/AOD/output/")

AOD_input_path <- getwd()

AOD_input_path <- paste0(AOD_input_path, '/')
# input -------------------------------------------------------------------
#call the base raster script and convert it to 4326
#AOD input path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

current_wd <- getwd()
AOD_output_path <- paste0(current_wd, '/output/AOD/')

#load the df with CanOSSEM raster cell num with lon-lat and coordinates in meters
load('CanOSSEM_base_raster_data_frame_3347_4326_projection.RData')

# function definition -----------------------------------------------------
rCN_assign_AOD <- function(AOD_input_path, AOD_output_path, year_val){
  
  #create the date range in julian days
  date_range <- format(seq(as.Date(paste(year_val,"-01-01", sep = "")),
                           as.Date(paste(year_val,"-01-02", sep = "")),
                           by=1), "%Y-%m-%d")
  
  
  #foreach loop
  foreach(i = seq_along(date_range)) %do% {
    
    #printing the date_range[i]
    print(date_range[i])
    
    #get the list of files for the day
    filelist_forday <- 
        list.files(path = AOD_input_path,
                   pattern = paste("*",date_range[i],sep="_"), full.names = T, recursive = T)
    
    for(k in seq_along(filelist_forday)) {
        
      #load all the files for the day
      load(filelist_forday[k])
      
      if(!nrow(clean_AOD_data)==0){
      #make sure distinct observations only
      daily_AOD_data <- clean_AOD_data %>% 
        distinct()
      
      #xy of AOD
      xy_AOD <- sapply(daily_AOD_data[, 1:2], as.numeric)
      xy_base_raster_4326 <- sapply(df_base_raster_4326[, 2:3], as.numeric)
      num_cells_base_raster <- max(df_base_raster_4326$rasterCellNum)
      
      #spatial AOD spdf
      spatial_AOD <- SpatialPointsDataFrame(coords = xy_AOD, data = daily_AOD_data,
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      
      #converting the spdf into dataframe
      df_spatial_AOD <- as.data.frame(spatial_AOD)
      
      #cleaning up the df
      df_spatial_AOD <- df_spatial_AOD %>% 
        mutate(AOD_row_num = 1:nrow(df_spatial_AOD)) %>% 
        rename(AOD_lon = longitude,
               AOD_lat = latitude) %>% 
        dplyr::select(-longitude.1, -latitude.1)
      
      #RANN nn2()
      #trying nn2 function
      nn2output <- RANN::nn2(xy_AOD,
                             query = xy_base_raster_4326,
                             k =1,
                             treetype = "kd",
                             searchtype = "priority")
      
      #converting the rastercell indices into a dataframe
      nn2outputdf <- as.data.frame(nn2output[["nn.idx"]])
      
      #renaming the var, and adding a row count  
      nn2outputdf_kd_priority <- nn2outputdf %>% 
        mutate(dists = nn2output[["nn.dists"]]) %>%
        rename(AOD_row_num = V1) %>% 
        mutate(rasterCellNum = 1:nrow(nn2outputdf))
      
      #creating a lon-lat df
      nn2_lonlat <- left_join(nn2outputdf_kd_priority,
                              df_base_raster_4326,
                              by = "rasterCellNum") %>% 
        dplyr::select(-lon_3347, -lat_3347)
      
      #doing the joining specifically for AOD, alongwith the AOD lon-lat vals
      nn2_AOD <- left_join(nn2_lonlat,
                           df_spatial_AOD,
                           by = "AOD_row_num")
      
      #removing the AOD rownum since it won't be required beyond
      nn2_AOD <- nn2_AOD %>% 
        dplyr::select(-AOD_row_num)
      
      
      #AOD lon/lat
      lon1 <- as.numeric(nn2_AOD$AOD_lon)
      lat1 <- as.numeric(nn2_AOD$AOD_lat)
      
      #base raster lon/lat
      lon2 <- as.numeric(nn2_AOD$longitude)
      lat2 <- as.numeric(nn2_AOD$latitude)
      
      
      #define the function to calculate haversine distance
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
      
      aod_distance <- as.data.frame(haversine_distance(lon1, lat1, lon2, lat2))
      
      aod_distance_new <- bind_cols(nn2_AOD,aod_distance)
      
      aod_distance_new <- aod_distance_new %>% 
        dplyr::rename(`Distance (km)` = `haversine_distance(lon1, lat1, lon2, lat2)`) %>% 
        dplyr::select(-dists)
      
      #dropping all the values where distance >50
      AOD_new_distance <- aod_distance_new %>%
        filter(`Distance (km)`<=50) %>% 
        add_count(rasterCellNum, name = "count_cells", .drop = F)
      
      #summarising for each cell
      averaged_AOD_data <- AOD_new_distance %>% 
        dplyr::select(-satellite_name, -time_CST) %>%
        group_by(rasterCellNum) %>%
        summarise_all(funs(mean))
      
      #fill up the values for other rasterCellNum
      AOD_filled_up_base <- averaged_AOD_data %>% 
        dplyr::select(-count_cells, -longitude, -latitude, -AOD_lon, -AOD_lat) %>% 
        tidyr::complete(rasterCellNum = seq(1:num_cells_base_raster)) %>%
        dplyr::mutate(ACQ_DATE = date_range[i]) %>% 
        padr::fill_by_value(value = 0)
      
      
      #save the AOD data
      save(AOD_filled_up_base, 
           file = paste0(AOD_output_path,'/','AOD_data_rCN_assigned_', date_range[i], '.RData'),
           compress = T)
    }
    else{
      print("no data found")
      
      #create a dataframe with no values
      empty_AOD_data <- clean_AOD_data %>% 
        dplyr::mutate(rasterCellNum = 1,
                      `Distance (km)` = 0) %>% 
        dplyr::select(rasterCellNum,`Distance (km)`, opt_depth_470nm:mass_conc_land)
      
      #fill up the dataframe
      AOD_filled_up_base <- empty_AOD_data %>% 
        tidyr::complete(rasterCellNum = seq(1:num_cells_base_raster)) %>%
        dplyr::mutate(ACQ_DATE = date_range[i]) %>%
        padr::fill_by_value(value = NA)
      
      #save this df
      save(AOD_filled_up_base, 
           file = paste0(AOD_output_path,'AOD_data_rCN_assigned_', date_range[i], '.RData'),
           compress = T)
      
    }
    }
  }
  
}


# function calling --------------------------------------------------------
for(year_val in 2019){
  rCN_assign_AOD(AOD_input_path, AOD_output_path, year_val)
}


