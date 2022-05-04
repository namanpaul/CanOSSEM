#namanpaul
#extraction for MYD files for AOD data (Terra satellite)

#loading the pckgs (not all would be used)
library(sp)
library(raster)
library(gridExtra)
library(rgdal)
library(dplyr)
library(stringr)
library(gdalUtils)
library(rgeos)
library(RGeostats)
library(lubridate)
library(foreach)

#flx i/p path
#set the working directory to the current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

current_wd <- getwd()

MYD_input_path <- paste0(current_wd, '/input/MYD/')
MYD_output_path <- paste0(current_wd, '/output/MYD_mult/')

#satellite MYD or MYD (Terra/Aqua)
satellite_name <- 'MYD'
year_val <- 2019

#to ensure the input errors doesn't block
input_errors <- readxl::read_excel("known_AOD_input_errors.xlsx", 
                                   sheet = "AOD")

#filter for the satellite
input_errors <- input_errors %>% 
  dplyr::filter(satellite == satellite_name)


process_MYD <- function(MYD_input_path, MYD_output_path, satellite_name, year_val){
  
  #create the date range in julian days
  date_range1 <- format(seq(as.Date(paste(year_val,"-01-01", sep = "")),
                            as.Date(paste(year_val,"-01-02", sep = "")),
                            by=1), "%Y%j")
  
  #using str substitute
  list_julian_days <- stringr::str_sub(date_range1, -3)
  
  
  #filter out missing days
  date_range <- date_range1[!date_range1 %in% input_errors$year_val_j]
  
  for(i in seq_along(date_range)){
    
    #printing to keep a check of how many files have been processed
    #using strsub to extract the julian day
    julian_day <- stringr::str_sub(date_range[i], -3)
    
    print(julian_day)
    
    #list of all the files
    MYD_files <- list.files(path = paste0(MYD_input_path,year_val,"/",julian_day),
                            pattern = "\\.hdf$")
    
    if(!purrr::is_empty(MYD_files)){
      
      foreach(k = seq_along(MYD_files)) %do% {
        
        #get the list of subdatasets
        sds <- gdalUtils::get_subdatasets(paste0(MYD_input_path,year_val,
                                                 "/",julian_day,"/",MYD_files[k]))
        #get the cor_optical_depth
        cor_optical_depth <- sds[16]
        
        #convert this into a raster
        #use rasterTmpFile(), complimentary functions: showTmpFiles() removeTmpFiles()
        rasterfile <- rasterTmpFile()
        
        #add extension tif?
        raster::extension(rasterfile) <- 'tif'
        
        #gdal translate to convert this
        gdal_translate(src_dataset = cor_optical_depth,
                       dst_dataset = rasterfile)
        
        #read the new raster file created as a raster layer
        raster_cor_optical_depth_band1 <- raster(rasterfile, band=1)
        
        raster_cor_optical_depth_band2 <- raster(rasterfile, band=2)
        
        raster_cor_optical_depth_band3 <- raster(rasterfile, band=3)
        
        #temp files no longer required
        removeTmpFiles()
        
        #get the 2.11 um (2110nm) opt depth as well
        #get the cor_optical_depth
        cor_optical_depth_2110 <- sds[70]
        
        #convert this into a raster
        #use rasterTmpFile(), complimentary functions: showTmpFiles() removeTmpFiles()
        rasterfile <- rasterTmpFile()
        
        #add extension tif?
        raster::extension(rasterfile) <- 'tif'
        
        #gdal translate to convert this
        gdal_translate(src_dataset = cor_optical_depth_2110,
                       dst_dataset = rasterfile)
        
        #read the new raster file created as a raster layer
        raster_cor_optical_depth_2110 <- raster(rasterfile)
        
        #temp files no longer required
        removeTmpFiles()
        
        #mass conc Land, going with sds[22] since its 16-bit int
        mass_conc_land <- sds[22]
        
        #convert this into a raster
        #use rasterTmpFile(), complimentary functions: showTmpFiles() removeTmpFiles()
        rasterfile <- rasterTmpFile()
        
        #add extension tif?
        raster::extension(rasterfile) <- 'tif'
        
        #gdal translate to convert this
        gdal_translate(src_dataset = mass_conc_land,
                       dst_dataset = rasterfile)
        
        #read the new raster file created as a raster layer
        raster_mass_conc_land <- raster(rasterfile)
        
        #temp files no longer required
        removeTmpFiles()
        
        #get lon 
        longitude <- sds[52]
        
        #convert this into a raster
        #use rasterTmpFile(), complimentary functions: showTmpFiles() removeTmpFiles()
        rasterfile <- rasterTmpFile()
        
        #add extension tif?
        raster::extension(rasterfile) <- 'tif'
        
        #gdal translate to convert this
        gdal_translate(src_dataset = longitude,
                       dst_dataset = rasterfile)
        
        #read the new raster file created as a raster layer
        raster_longitude <- raster(rasterfile)
        
        #temp files no longer required
        removeTmpFiles()
        
        ###latitude
        #get lat 
        latitude <- sds[53]
        
        #convert this into a raster
        #use rasterTmpFile(), complimentary functions: showTmpFiles() removeTmpFiles()
        rasterfile <- rasterTmpFile()
        
        #add extension tif?
        raster::extension(rasterfile) <- 'tif'
        
        #gdal translate to convert this
        gdal_translate(src_dataset = latitude,
                       dst_dataset = rasterfile)
        
        #read the new raster file created as a raster layer
        raster_latitude <- raster(rasterfile)
        
        #temp files no longer required
        removeTmpFiles()
        
        
        ###time
        #gettime 
        time <- sds[1]
        
        #convert this into a raster
        #use rasterTmpFile(), complimentary functions: showTmpFiles() removeTmpFiles()
        rasterfile <- rasterTmpFile()
        
        #add extension tif?
        raster::extension(rasterfile) <- 'tif'
        
        #gdal translate to convert this
        gdal_translate(src_dataset = time,
                       dst_dataset = rasterfile)
        
        #read the new raster file created as a raster layer
        raster_time <- raster(rasterfile)
        
        #temp files no longer required
        removeTmpFiles()
        
        
        #convert the rasters to points to df
        df_cor_optical_depth_1 <- as.data.frame(rasterToPoints(raster_cor_optical_depth_band1))
        
        df_cor_optical_depth_2 <- as.data.frame(rasterToPoints(raster_cor_optical_depth_band2))
        
        df_cor_optical_depth_3 <- as.data.frame(rasterToPoints(raster_cor_optical_depth_band3))
        
        df_cor_optical_depth_2110 <- as.data.frame(rasterToPoints(raster_cor_optical_depth_2110))
        
        #the 2110 requires filtering of the missing values which here are -9.99, basically remove all negative values
        df_cor_optical_depth_2110 <- df_cor_optical_depth_2110 %>% 
          dplyr::filter(!df_cor_optical_depth_2110[,3] < -1)
        
        #mass conc land
        df_mass_conc_land <- as.data.frame(rasterToPoints(raster_mass_conc_land))
        
        #lon
        df_longitude <- as.data.frame(rasterToPoints(raster_longitude))
        
        #renaming the third column
        df_longitude <- df_longitude %>% 
          dplyr::rename_at(3, ~"longitude")
        
        #lat
        df_latitude <- as.data.frame(rasterToPoints(raster_latitude))
        
        #renaming the third column
        df_latitude <- df_latitude %>% 
          rename_at(3, ~"latitude")
        
        #df time
        df_time <- as.data.frame(rasterToPoints(raster_time))
        
        #renaming the third column
        df_time <- df_time %>% 
          rename_at(3, ~"time")
        #Valid Range: 0.0 to 3.1558E+9 seconds since 1 January 1993 00:00:00 
        
        
        #using lubridate::as_datetime, specifying the Central standard time (CST)
        df_time <- df_time %>% 
          dplyr::mutate(time_UTC = lubridate::as_datetime(time, origin = "1993-01-01 00:00:00"),
                        time_CST = time_UTC-hours(6),
                        ACQ_DATE = as_date(time_CST))
        
        #deselect time UTC
        df_time <- df_time %>% 
          dplyr::select(-time,-time_UTC)
        
        #first join lon lat
        lon_lat_time <- df_latitude %>% 
          inner_join(df_longitude, c("x","y")) %>% 
          inner_join(df_time, c("x","y"))
        
        #joining these dfs
        MYD_data <- df_cor_optical_depth_1 %>% 
          inner_join(df_cor_optical_depth_2, c("x","y")) %>% 
          inner_join(df_cor_optical_depth_3, c("x","y")) %>%
          inner_join(df_cor_optical_depth_2110, c("x","y")) %>%
          inner_join(df_mass_conc_land, c("x","y")) %>%
          ungroup() %>% 
          #dplyr::select(-x, -y) %>% 
          rename_at(3, ~"opt_depth_470nm") %>% 
          rename_at(4, ~"opt_depth_550nm") %>%
          rename_at(5, ~"opt_depth_660nm") %>% 
          rename_at(6, ~"opt_depth_2110nm") %>% 
          rename_at(7, ~"mass_conc_land") %>% 
          mutate(satellite_name = satellite_name)
        
        MYD_data_with_lonlat <- merge(MYD_data,
                                      lon_lat_time,
                                      by = c("x","y"))
        #assigning a randomstring
        assign(paste("random", k, sep="_"), MYD_data_with_lonlat)
        
      }
      #list of dataframes
      list_df <- lapply(ls(pattern="random+"), function(x) get(x))
      
      day_MYD_data <- dplyr::bind_rows(list_df)
      
      #a vector of dates
      dates_MYD <- ymd(unique(day_MYD_data$ACQ_DATE))
      
      counter = 0
      foreach(j = dates_MYD) %do% {
        #create a smaller df to operate on
        singledate <- day_MYD_data %>% 
          dplyr::filter(ACQ_DATE == j)
        
        #counter
        counter = counter+1
        
        
        #setwd(MYD_output_path)
        
        #save the data
        save(singledate,
             file = paste0('MYD_',j,'_',counter,'.RData'))
        
      }
      
      
      
      
      #save(day_MYD_data, file = paste0(MYD_output_path,'/','MYD_data_',satellite_name,'_',as.Date(as.character(date_range[i]), "%Y%j"), '.RData'))
      rm(list=ls(pattern="random+"))
      
    }else { 
      print(paste0("No input data for ", as.Date(as.character(date_range[i]), "%Y%j")))
    }
  }
  
}




#execute #change it to 2015
for(year_val in 2019){
  process_MYD(MYD_input_path, MYD_output_path, satellite_name, year_val)
}
