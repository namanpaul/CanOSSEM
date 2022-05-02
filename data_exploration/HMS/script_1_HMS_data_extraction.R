#namanpaul
#HMS data extraction

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
library(cleangeo)
library(foreign)


#set the working directory to the current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

current_wd <- getwd()

#load the base raster
base_raster <- raster('../base_raster_creation/base_raster.tif')



#create a crs object for the source 4326 and target 3347
crs_4326 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

crs_3347 <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")



#i/p o/p paths for HMS smoke
HMS_input_path <- paste0(current_wd,'/input/2019/')

#o/p path
HMS_RData_output_path <- paste0(current_wd,'/output/')

HMS_input_errordays <- readr::read_csv("known_HMS_input_errordays.csv")


#define the function
process_HMS <- function(HMS_input_path, year_val){
  
  #create the date range
  date_range1 <- seq(as.Date(paste(year_val,"-01-01", sep = "")),
                     as.Date(paste(year_val,"-01-02", sep = "")),
                     by=1)
  
  date_range1 <- format(date_range1, "%Y%m%d")
  
  dates_HMS <- HMS_input_errordays$HMS_date
  
  dates_HMS<- format(dates_HMS, "%Y%m%d")
  
  
  #filter out missing days
  date_range <- date_range1[!date_range1 %in% dates_HMS]
  
  #looping over the seq_along FRP input files
  for (i in seq_along(date_range)) {
    
    
    #read in the associated dbf
    read_DBF <- read.dbf(file = paste0(HMS_input_path,date_range[i],'/',date_range[i], ".dbf"))
    
    
    #checking if the dbf files are not empty
    if(nrow(read_DBF) > 0 & colSums(is.na(read_DBF))[2] == 0){
      
      #read in the HMS smoke
      read_HMS <- readOGR(paste0(HMS_input_path,date_range[i],'/',date_range[i],'.shp')) 
      
      
      #read_HMS <- st_read(paste0(HMS_input_path,date_range[i],'/',date_range[i],'.shp'))
      
      #read in the input files
      #clean the input
      hms_data_input <- clgeo_Clean(read_HMS)
      
      #set the crs 4326
      proj4string(hms_data_input) <- crs_4326
      
      #modify the crs
      hms_data_3347 <- spTransform(hms_data_input, crs_3347)
      
      #set the projection string
      proj4string(hms_data_3347) <- CRS("+init=epsg:3347")
      
      #repair geometry as per Angela's code
      hms_repair <- gBuffer(hms_data_3347, width=0, byid=TRUE)  
      hms_repaired  <- SpatialPolygonsDataFrame(hms_repair,data = hms_data_3347@data)
      
      #repaired the df
      hms_rp_df <- as.data.frame(hms_repaired)
      
      #select the CST
      hms_rp_time <- hms_rp_df %>% 
        dplyr::mutate(time_UTC = as.POSIXct(strptime(End, format = "%Y%j %H%M")),
                      time_CST = time_UTC-hours(6),
                      ACQ_DATE = as_date(time_CST)) %>% 
        select(-Start,-End,-time_UTC)
      
      #dissolve the polygons into 1 single 
      hms_repaired@data$Density = 1
      if (!rgeosStatus()) gpclibPermit()
      hms_one_polygon <- unionSpatialPolygons(hms_repaired,hms_repaired@data$Density)
      
      
      #intersect hms with raster, getting cells with HMS covered
      raster_masked <- mask(x=base_raster, mask=hms_one_polygon) #assign NA to all cells outside HMS plumes
      raster_masked[!is.na(raster_masked)]=1   # non-NA area was covered by HMS, assigned value of 1
      raster_masked[is.na(raster_masked)]=NA   # otherwise, assigned value of 0
      
      #convert it to a df
      HMS_df <- as.data.frame(raster_masked)
      
      #add the rownum
      HMS_filled_up_base <- HMS_df %>%
        mutate(rasterCellNum = 1:nrow(HMS_df)) %>%
        rename_at(1, ~"HMS_value") %>%
        select(rasterCellNum, everything())
      
      #save the RData df
      save(HMS_filled_up_base, 
           file = paste0(HMS_RData_output_path,'/','HMS_rCN_assigned_', date_range[i], '.RData'))
      
    }else {
      base_raster <- setValues(base_raster,0)
      #writeRaster(base_raster, 
      #            filename = file.path(HMS_tif_output_path,
      #                                 paste0('HMS_data_', date_range[i])),
      #            overwrite=TRUE,
       #           format = 'GTiff')
      
      #convert it to a df
      HMS_df <- as.data.frame(base_raster)
      
      #add the rownum
      HMS_filled_up_base <- HMS_df %>%
        mutate(rasterCellNum = 1:nrow(HMS_df)) %>%
        rename_at(1, ~"HMS_value") %>%
        select(rasterCellNum, everything())
      
      #save the RData df
      save(HMS_filled_up_base, 
           file = paste0(HMS_RData_output_path,'/','HMS_rCN_assigned_', date_range[i], '.RData'))
      
    }
    
  }
  
}



for(year_val in 2019){
  process_HMS(HMS_input_path,year_val)
}











#the code below can be used for checking NAs

file_list <-list.files(path = HMS_RData_output_path, pattern = paste("*"),
                                     full.names = T, recursive = T)

#combine multiple RData using mget and sapply
HMS_NA_Check <- bind_rows(sapply(file_list, function(x) mget(load(x)), simplify = TRUE))


nrows <- HMS_NA_Check %>% 
  #select(HMS_value) %>% 
  count(is.na(HMS_value))

#checking the number of NAs
((nrows$n[2]+(7*970215))/(970215*365))





