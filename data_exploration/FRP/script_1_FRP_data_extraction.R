#namanpaul
#FRP data extraction

#loading the packages
library(sf)
library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(purrr)
library(stringr)
library(geojsonio)
library(gdalUtils)
library(rgeos)
library(RGeostats)
library(lubridate)
library(foreach)


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

#assign 100km to a buffer var (100,000 meters)
buffer_big_raster <- 100000

#resolution val 5km (5,000 meters)
resolution_val <- 5000

#adding 100km buffer to create a larger raster
x_min_big_raster <- bbox_vals[1]-buffer_big_raster
x_max_big_raster <- bbox_vals[3]+buffer_big_raster
y_min_big_raster <- bbox_vals[2]-buffer_big_raster
y_max_big_raster <- bbox_vals[4]+buffer_big_raster

#extent of the larger raster
ext_big_raster <- extent(x_min_big_raster,x_max_big_raster,
                         y_min_big_raster,y_max_big_raster)

#create a crs object
crs_3347 <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#big raster creation
big_raster <- raster(ext_big_raster, 
                      ncol=(x_max_big_raster - x_min_big_raster)/resolution_val,
                      nrow=(y_max_big_raster - y_min_big_raster)/resolution_val,
                      crs_3347)

#assigning cell values based on the number of cells
big_raster_cell_values = 1:ncell(big_raster)

#setting the values
big_raster  <- setValues(big_raster, big_raster_cell_values)

#set crs 3347
proj4string(big_raster) <- CRS("+init=epsg:3347")

#no. of cells in base raster
num_cells_big_raster <- as.numeric(ncell(big_raster))


#create a df for the base raster
big_raster_df = as.data.frame(seq(1,ncell(big_raster),1))

#rename the col
colnames(big_raster_df) = "rasterCellNum"

#final base raster
x_min <- bbox_vals[1]
x_max <- bbox_vals[3]
y_min <- bbox_vals[2]
y_max <- bbox_vals[4]

#extent
ext  <- extent(x_min,x_max, y_min,y_max)

#base_raster creation
base_raster <- raster(ext, 
                      ncol=(x_max - x_min)/resolution_val,
                      nrow=(y_max - y_min)/resolution_val,
                      "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
base_raster_cell_values = 1:ncell(base_raster)
base_raster  <- setValues(base_raster, base_raster_cell_values)

#set crs 3347
proj4string(base_raster) <- CRS("+init=epsg:3347")

#no. of cells in base raster
num_cells_base_raster <- as.numeric(ncell(base_raster))

#create a df
base_raster_df = as.data.frame(seq(1,num_cells_base_raster,1))
colnames(base_raster_df) = "rasterCellNum"

#the FRP data were downloaded from NASA FIRMS : https://firms.modaps.eosdis.nasa.gov/download/
#for the years 2010:2019

#sample for 2019 is included here:
FRP_input_path <- paste0(current_wd,'input/2019/')

#output RData path
FRP_output_RData_path <- paste0(current_wd,'/output/')


#defining the process_FRP function

#FRP file path is the location on Sync where the FRP annual data files are 1stored
#the following actions need to be done inorder to generate the final FRP raster
#1.create a base raster 5km x 5km resolution with a 100km buffer
#2.read in the .shp FRP file, do pre-processing steps
#3.extract frp data to the base raster
#4.extract all of the rastercellnumbers and associated FRP values
#5.ignore NAs, sum up same rastercellNum's FRP values
#6.compute focal vals
#7.generates daily files
#list all the FRP input files

process_FRP <- function(FRP_input_path, FRP_output_RData_path, FRP_output_tif_path){

  FRP_input_files <- list.files(path=paste0(FRP_input_path),
                                pattern="*.shp", full.names=TRUE, recursive=TRUE)
  
  #looping over the seq_along FRP input files
  foreach(i = seq_along(FRP_input_files)) %do% {
    
    FRP_input <- readOGR(FRP_input_files[i])
    
     #change the crs of the frp input
    FRP_input <- spTransform(FRP_input, CRSobj = crs_3347)
    
    #Extract cellnums from the baseraster
    FRP_input$rasterCellNum <- raster::extract(big_raster,FRP_input)
    
    #convert the sp to sf
    FRP_sf <- st_as_sf(FRP_input)
    
    #convert UTC
    FRP_sf_tz <- within(FRP_sf, { UTC_time = as.POSIXct(strptime(paste(ACQ_DATE, ACQ_TIME),
                                                              "%Y/%m/%d %H%M")) })
    
    #convert this into a df
    FRP_TZ <- as_data_frame(FRP_sf_tz)
    
    #the central time zone is used for this project CST: UTC-6
    FRP_TZ$CST_time <- FRP_TZ$UTC_time - hours(6)
    
    #now modify the ACQ_DATE
    FRP_TZ$ACQ_DATE <-lubridate::date(FRP_TZ$CST_time) 
    
    #calculate FRP daily sum
    FRP_df <- as_data_frame(FRP_TZ) %>%
      filter(!is.na(rasterCellNum)) %>%
      dplyr::group_by(rasterCellNum, ACQ_DATE) %>%
      summarise(FRP_daily_sum = sum(FRP, na.rm = T))
    
    assign(paste("FRP", i, sep="_"), FRP_df)
  
  }
   
    #now join all of the various dfs (leaving these here to bind dataframes from other 9 files)
    FRP_joined <- FRP_1 #%>% 
      #bind_rows(FRP_2) %>% 
      #bind_rows(FRP_3) %>% 
      #bind_rows(FRP_4) %>% 
      #bind_rows(FRP_5) %>% 
      #bind_rows(FRP_6) %>% 
      #bind_rows(FRP_7) %>% 
      #bind_rows(FRP_8) %>% 
      #bind_rows(FRP_9) %>%
      #bind_rows(FRP_10) 
  
    #save the full data
    save(FRP_joined, 
              file = paste0(FRP_output_RData_path,'FRP_extracted_data_', '.RData'))
    
    #create the date-range as per the current file's year
    date_range <- as.character(seq(as.Date(paste("2019-01-01", sep = "")),
                                   as.Date(paste("2019-01-02", sep = "")),
                                   by=1))
  
    
    #for loop for going over all the days
    foreach(k = date_range) %do% {
      #create a smaller df to operate on
      singledate <- FRP_joined %>% 
        dplyr::filter(ACQ_DATE == k)
      
      #here we match the cells of the big raster df and the smaller single date df
      match <- merge(big_raster_df,
                     singledate,
                     all.x=T)
      
      #set values to the big raster
      big_raster <- setValues(big_raster,
                              match$FRP_daily_sum)
    
      
      #compute focal values
      focalvalues <- raster::focal(big_raster, w=matrix(1,41,41), na.rm=T)
      
      #add the frp daily sum
      base_raster_df$FRP_daily_sum = raster::extract(focalvalues,extent(base_raster))
      
      #set vals
      base_raster <- setValues(base_raster,
                               base_raster_df$FRP_daily_sum)
      
     
      #convert the base_raster into a dataframe
      base_raster_WRITE <- as.data.frame(base_raster)
      
      #add the rownum
      FRP_filled_up_base <- base_raster_WRITE %>%
        mutate(rasterCellNum = 1:nrow(base_raster_WRITE)) %>%
        rename_at(1, ~"FRP_daily_sum") %>%
        select(rasterCellNum, everything())
      
      #save the RData df
      save(FRP_filled_up_base, 
           file = paste0(FRP_output_RData_path,'/','FRP_rCN_assigned_', k, '.RData'))
      
      
    }
}                                   
  


#execute the function
process_FRP(FRP_input_path, FRP_output_RData_path, FRP_output_tif_path)




